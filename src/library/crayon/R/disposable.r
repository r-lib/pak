
## nocov start

install_quietly <- TRUE

with_wd <- function(dir, expr) {
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(dir)
  eval(substitute(expr), envir = parent.frame())
}

#' @importFrom utils tar

build_pkg <- function(path, pkg_file = NULL) {
  if (!file.exists(path)) stop("path does not exist")
  pkg_name <- basename(path)
  if (is.null(pkg_file)) {
    pkg_file <- file.path(dirname(path), paste0(pkg_name, "_1.0.tar.gz"))
  }
  with_wd(dirname(path),
          tar(basename(pkg_file), pkg_name, compression = "gzip"))
  pkg_file
}

#' @importFrom utils package.skeleton install.packages

install_tmp_pkg <- function(..., pkg_name, lib_dir, imports = character()) {
  if (!file.exists(lib_dir)) stop("lib_dir does not exist")
  if (!is.character(pkg_name) || length(pkg_name) != 1) {
    stop("pkg_name is not a string")
  }

  ## Create a directory that will contain the source package
  src_dir <- tempfile()
  on.exit(try(unlink(src_dir, recursive = TRUE), silent = TRUE), add = TRUE)
  dir.create(src_dir)

  ## Create source package, need a non-empty environment,
  ## otherwise package.skeleton fails
  tmp_env <- new.env()
  assign("f", function(x) x, envir = tmp_env)
  suppressMessages(package.skeleton(pkg_name, path = src_dir,
                                    environment = tmp_env))
  pkg_dir <- file.path(src_dir, pkg_name)

  ## Make it installable: remove man, add imports
  unlink(file.path(pkg_dir, "man"), recursive = TRUE)
  if (length(imports) != 0) {
    cat("Imports: ", paste(imports, collapse = ", "), "\n",
        file = file.path(pkg_dir, "DESCRIPTION"), append = TRUE)
    cat(paste0("import(", imports, ")"), sep="\n",
        file = file.path(pkg_dir, "NAMESPACE"), append = TRUE)
  }

  ## Put the code in it, dput is noisy, so we need to redirect it to
  ## temporary file
  exprs <- list(...)
  unlink(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "R"))
  code_file <- file.path(pkg_dir, "R", "code.R")
  tmp_file <- tempfile()
  on.exit(try(unlink(tmp_file), silent = TRUE), add = TRUE)
  sapply(exprs, function(x)
         cat(deparse(dput(x, file = tmp_file)),
             file = code_file, append = TRUE, "\n", sep="\n"))

  ## Build it
  pkg_file <- build_pkg(pkg_dir)

  ## Install it into the supplied lib_dir
  install.packages(pkg_file, lib = lib_dir, repos = NULL, type = "source",
                   quiet = install_quietly)
}

with_libpath <- function(lib_path, ...) {
  cur_lib_path <- .libPaths()
  on.exit(.libPaths(cur_lib_path), add = TRUE)
  .libPaths(c(lib_path, cur_lib_path))
  exprs <- c(as.list(match.call(expand.dots = FALSE)$...))
  sapply(exprs, eval, envir = parent.frame())
}

make_packages <- function(..., lib_dir = tempfile(),
                          imports = character()) {
  if (!file.exists(lib_dir)) dir.create(lib_dir)
  exprs <- c(as.list(match.call(expand.dots = FALSE)$...))
  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    name <- names(exprs)[i]
    install_tmp_pkg(expr, pkg_name = name,
                     lib_dir = lib_dir, imports = imports)
    ## Unload everything if an error happens
    on.exit(try(unloadNamespace(name), silent = TRUE), add = TRUE)
    with_libpath(lib_dir, suppressMessages(library(name, quietly = TRUE,
                                                   character.only = TRUE)))
    on.exit()
  }
  invisible(lib_dir)
}

## nocov end
