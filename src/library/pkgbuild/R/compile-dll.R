#' Compile a .dll/.so from source.
#'
#' `compile_dll` performs a fake R CMD install so code that
#' works here should work with a regular install (and vice versa).
#' During compilation, debug flags are set with
#' \code{\link{compiler_flags}(TRUE)}.
#'
#' Invisibly returns the names of the DLL.
#'
#' ## Configuration
#'
#' ### Options
#'
#' * `pkg.build_extra_flags`: set this to `FALSE` to to opt out from adding
#'   debug compiler flags in `compile_dll()`. Takes precedence over the
#'   `PKG_BUILD_EXTRA_FLAGS` environment variable. Possible values:
#'
#'   - `TRUE`: add extra flags,
#'   - `FALSE`: do not add extra flags,
#'   - `"missing"`: add extra flags if the user does not have a
#'     `$HOME/.R/Makevars` file.
#'
#' ### Environment variables
#'
#' * `PKG_BUILD_EXTRA_FLAGS`: set this to `false` to to opt out from adding
#'   debug compiler flags in `compile_dll()`. The `pkg.build_extra_flags` option
#'   takes precedence over this environment variable. Possible values:
#'
#'   - `"true"`: add extra flags,
#'   - `"false"`: do not add extra flags,
#'   - `"missing"`: add extra flags if the user does not have a
#'     `$HOME/.R/Makevars` file.
#'
#' @note If this is used to compile code that uses Rcpp, you will need to
#'   add the following line to your `Makevars` file so that it
#'   knows where to find the Rcpp headers:
#'   ```
#'   PKG_CPPFLAGS=`$(R_HOME)/bin/Rscript -e 'Rcpp:::CxxFlags()'`
#'   ```
#'
#' @inheritParams build
#' @param force If `TRUE`, for compilation even if [needs_compile()] is
#'   `FALSE`.
#' @param debug If `TRUE`, and if no user Makevars is found, then the build
#'   runs without optimisation (`-O0`) and with debug symbols (`-g`). See
#'   [compiler_flags()] for details. If you have a user Makevars (e.g.,
#'   `~/.R/Makevars`) then this argument is ignored.
#' @seealso [clean_dll()] to delete the compiled files.
#' @export
compile_dll <- function(
  path = ".",
  force = FALSE,
  compile_attributes = pkg_links_to_cpp11(path) || pkg_links_to_rcpp(path),
  register_routines = FALSE,
  quiet = FALSE,
  debug = TRUE
) {
  path <- pkg_path(path)

  if (!needs_compile(path) && !isTRUE(force)) {
    return(invisible())
  }

  check_build_tools(quiet = TRUE)
  update_registration(path, compile_attributes, register_routines, quiet)

  # Mock install the package to generate the DLL
  xflags <- should_add_compiler_flags()
  if (!quiet) {
    cli::cli_alert_info(c(
      "Re-compiling {.pkg {pkg_name(path)}}",
      if (debug && xflags) " (debug build)"
    ))
  }

  install_dir <- tempfile("devtools_install_")
  dir.create(install_dir)

  build <- function() {
    install_min(
      path,
      dest = install_dir,
      components = "libs",
      args = if (needs_clean(path)) "--preclean",
      quiet = quiet
    )

    invisible(dll_path(file.path(install_dir, pkg_name(path))))
  }

  if (xflags) {
    withr_with_makevars(compiler_flags(debug), {
      if (debug) {
        withr_with_envvar(c(DEBUG = "true"), build())
      } else {
        build()
      }
    })
  } else {
    build()
  }
}

#' Remove compiled objects from /src/ directory
#'
#' Invisibly returns the names of the deleted files.
#'
#' @inheritParams build
#' @seealso [compile_dll()]
#' @export
clean_dll <- function(path = ".") {
  path <- pkg_path(path)

  # Clean out the /src/ directory and children:
  # * individual object files
  # * overall package definition file
  # * symbols.rds (added when run inside R CMD check)
  pattern <- sprintf(
    "\\.(o|sl|so|dylib|a|dll)$|(%s\\.def)$|^symbols.rds$",
    pkg_name(path)
  )
  files <- dir(
    file.path(path, "src"),
    pattern = pattern,
    full.names = TRUE,
    recursive = TRUE
  )
  unlink(files)

  invisible(files)
}

# Returns the full path and name of the DLL file
dll_path <- function(path = ".") {
  name <- paste(pkg_name(path), .Platform$dynlib.ext, sep = "")
  file.path(path, "src", name)
}

mtime <- function(x) {
  x <- x[file.exists(x)]
  if (length(x) == 0) {
    return(NULL)
  }
  max(file.info(x)$mtime)
}

globs <- function(path = ".", x) {
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(path)
  Sys.glob(x)
}

# List all source files in the package
sources <- function(path = ".") {
  srcdir <- file.path(path, "src")
  src <- c(
    dir(srcdir, "\\.(c.*|f|rs)$", recursive = TRUE, full.names = TRUE),
    dir(srcdir, "^Cargo\\.toml$", recursive = TRUE, full.names = TRUE)
  )
  extra <- desc::desc_get("Config/build/extra-sources", path)

  if (!is.na(extra)) {
    glb <- trimws(strsplit(extra, ",", fixed = TRUE)[[1]])
    xs <- file.path(path, globs(path, glb))
    xfls <- unlist(lapply(
      xs,
      dir,
      recursive = TRUE,
      full.names = TRUE
    ))
    src <- c(src, xs, xfls)
  }
  src
}

# List all header files in the package
headers <- function(path = ".") {
  incldir <- file.path(path, "inst", "include")
  srcdir <- file.path(path, "src")

  c(
    dir(srcdir, "^Makevars.*$", recursive = TRUE, full.names = TRUE),
    dir(srcdir, "\\.h.*$", recursive = TRUE, full.names = TRUE),
    dir(incldir, "\\.h.*$", recursive = TRUE, full.names = TRUE)
  )
}


#' Does the package need recompiling?
#' (i.e. is there a source or header file newer than the dll)
#' @inheritParams build
#' @keywords internal
#' @export
needs_compile <- function(path = ".") {
  source <- mtime(c(sources(path), headers(path)))
  # no source files, so doesn't need compile
  if (is.null(source)) {
    return(FALSE)
  }

  dll <- mtime(dll_path(path))
  # no dll, so needs compile
  if (is.null(dll)) {
    return(TRUE)
  }

  source > dll
}

# Does the package need a clean compile?
# (i.e. is there a header or Makevars newer than the dll)
needs_clean <- function(path = ".") {
  never_clean <- get_desc_config_flag(path, "never-clean")
  if (isTRUE(never_clean)) {
    return(FALSE)
  }

  headers <- mtime(headers(path))
  # no headers, so never needs clean compile
  if (is.null(headers)) {
    return(FALSE)
  }

  dll <- mtime(dll_path(path))
  # no dll, so needs compile
  if (is.null(dll)) {
    return(TRUE)
  }

  headers > dll
}

install_min <- function(
  path = ".",
  dest,
  components = NULL,
  args = NULL,
  quiet = FALSE
) {
  stopifnot(is.character(dest), length(dest) == 1, file.exists(dest))

  poss <- c("R", "data", "help", "demo", "inst", "docs", "exec", "libs")
  if (!is.null(components)) {
    components <- match.arg(components, poss, several.ok = TRUE)
  }
  no <- setdiff(poss, components)
  no_args <- paste0("--no-", no)

  rcmd_build_tools(
    "INSTALL",
    c(
      path,
      paste("--library=", dest, sep = ""),
      no_args,
      "--no-multiarch",
      "--no-test-load",
      args
    ),
    fail_on_status = TRUE,
    quiet = quiet
  )

  invisible(file.path(dest, pkg_name(path)))
}
