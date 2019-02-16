
check_for_private_lib <- function(
    pkgs = c("crayon", "ps", "processx", "callr")) {
  lib <- private_lib_dir()
  if (!file.exists(lib) || !is_dir(lib)) stop("No private lib")
  for (pkg in pkgs) {
    pkgdir <- file.path(lib, pkg)
    if (!file.exists(pkgdir) || !is_dir(pkgdir)) stop("Broken private lib")
  }
}

use_private_lib <- function() {
  lib <- private_lib_dir()
  old <- .libPaths()
  new <- c(lib, old[old != lib])
  .libPaths(new)
}

get_private_lib <- function(create = TRUE) {
  if (!is.null(l <- pkg_data$private_lib)) return(l)
  if (!is.null(l <- check_private_lib())) return(l)
  if (create) {
    pak_setup()
  } else {
    stop("No pak private library")
  }
}

private_lib_dir <- function()  {
  file.path(user_cache_dir("R-pkg"), "lib", get_minor_r_version())
}

get_package_hash <- function(pkgdir) {
  libs <- file.path("libs", list_files(file.path(pkgdir, "libs")))
  to_hash <- normalizePath(file.path(pkgdir, c("DESCRIPTION", libs)))
  res <- tools::md5sum(to_hash)
  names(res) <- basename(names(res))
  res
}

package_needs_update <- function(pkgdir, lib) {
  tryCatch({
    target_dir <- file.path(lib, basename(pkgdir))
    if (!file.exists(target_dir)) return(TRUE)

    data_rds <- file.path(target_dir, "pkg-data.rds")
    if (!file.exists(data_rds)) return(TRUE)

    old <- readRDS(data_rds)
    new_hash <- get_package_hash(pkgdir)
    new_ts <- file_mtime(file.path(pkgdir, "DESCRIPTION"))
    ! identical(old$hash, new_hash) || new_ts >= old$ts
  }, error = function(e) return(TRUE))
}

copy_package <- function(from, lib) {
  message("Copying package `", basename(from), "`")
  file.copy(from, lib, recursive = TRUE)
  pkgdir <- file.path(lib, basename(from))
  hash <- get_package_hash(pkgdir)
  saveRDS(
    list(hash = hash, ts = Sys.time()),
    file = file.path(pkgdir, "pkg-data.rds")
  )
}

create_private_lib <- function() {
  lib <- private_lib_dir()
  if (!is.null(pkg_data$remote)) pkg_data$remote$kill()
  liblock <- lock_private_lib(lib)
  on.exit(unlock_private_lib(liblock), add = TRUE)
  pkg_data$deps <- pkg_data$deps %||% lookup_deps(.packageName)
  pkg_dirs <- pkg_data$deps
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)

  upd <- vlapply(pkg_dirs, package_needs_update, lib = lib)
  for(i in which(upd)) copy_package(pkg_dirs[i], lib)

  pkg_data$private_lib <- lib
  lib
}

check_private_lib <- function() {
  lib <- private_lib_dir()
  pkg_data$deps <- pkg_data$deps %||% lookup_deps(.packageName)
  pkgs <- basename(pkg_data$deps)
  if (all(pkgs %in% dir(lib))) lib else NULL
}

#' @importFrom utils head

lookup_deps <- function(package) {
  path <- getNamespaceInfo(asNamespace(package), "path")
  lib_path <- .libPaths()
  lib_pkgs <- lapply(lib_path, dir)
  done <- package
  result <- character()
  todo <- path

  ## TODO: check for version requirements
  find_lib <- function(pkg) {
    w <- head(which(vlapply(lib_pkgs, `%in%`, x = pkg)), 1)
    if (!length(w)) {
      stop("Required package `", pkg, "` is not available")
    }
    file.path(lib_path[w], pkg)
  }

  while (length(todo)) {
    new <- unlist(lapply(todo, extract_deps))
    new <- setdiff(new, done)
    new_paths <- vcapply(new, find_lib)
    result <- unique(c(result, new_paths))
    done <- unique(done, new)
    todo <- new_paths
  }

  result
}

extract_deps <- function(path) {
  dcf <- read.dcf(file.path(path, "DESCRIPTION"))
  deps <- paste(c(
    if ("Imports" %in% colnames(dcf)) dcf[, "Imports"],
    if ("Depends" %in% colnames(dcf)) dcf[, "Depends"]
  ), collapse = ", ")

  parse_dep_fields(deps)
}

parse_dep_fields <- function(deps) {
  deps <- str_trim(strsplit(deps, ",")[[1]])
  deps <- lapply(strsplit(deps, "\\("), str_trim)
  deps <- lapply(deps, sub, pattern = "\\)$", replacement = "")
  setdiff(vapply(deps, "[", "", 1), c("R", base_packages()))
}

## TODO: in theory the set of base packages can change over time,
## so we would need an R version specific vector here.
## Not an issue currently, might be in the future.

base_packages <- function() {
  if (is.null(pkg_data$base_packages)) {
    pkg_data$base_packages <-
      rownames(utils::installed.packages(priority = "base"))
  }
  pkg_data$base_packages
}

download_private_lib <- function(quiet = FALSE) {
  lib <- private_lib_dir()
  l <- lock_private_lib(lib, download = TRUE)
  on.exit(unlock_private_lib(l), add = TRUE)
  if (!is.null(pkg_data$remote)) pkg_data$remote$kill()
  pkg_data$deps <- pkg_data$deps %||% lookup_deps("pkg")
  pkg_dirs <- pkg_data$deps
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  remotes <- utils::packageDescription(.packageName)$Remotes

  old_libs <- .libPaths()
  on.exit(.libPaths(old_libs), add = TRUE)
  .libPaths(lib)

  old_env <- Sys.getenv("R_REMOTES_STANDALONE", NA_character_)
  if (!is.na(old_env)) {
    on.exit(Sys.setenv("R_REMOTES_STANDALONE" = old_env), add = TRUE)
  } else {
    on.exit(Sys.unsetenv("R_REMOTES_STANDALONE"), add = TRUE)
  }
  Sys.setenv("R_REMOTES_STANDALONE" = "true")

  if (!is.null(remotes)) {
    remotes <- str_trim(strsplit(remotes, ",\\s*")[[1]])
    if (!quiet) {
      message("\n! This is a _development_ version of pak,\n",
              "! some packages will be installed from *GitHub*\n\n")
    }
    for (rem in remotes) {
      source(paste0("https://install-github.me/", rem))
    }
  }

  installed <- dir(lib, pattern = "^[a-zA-Z0-9\\.]+$")
  to_install <- setdiff(basename(pkg_data$deps), dir(lib))
  if (length(to_install)) {
    utils::update.packages(oldPkgs = to_install, lib.loc = lib,
                           instlib = lib, ask = FALSE)
  } else {
    message("CRAN packages are up to date.")
  }

  invisible(lib)
}

lock_private_lib <- function(path, download = FALSE) {
  lib <- private_lib_dir()
  lockfile <- file.path(lib, "pkg-lib.lock")
  load_filelock(download)
  mkdirp(dirname(lockfile))
  lock <- pkg_data$ns$filelock$lock(lockfile, timeout = 3000)
  if (is.null(lock)) {
    stop("Cannot lock private library")
  }
  lock
}

unlock_private_lib <- function(lock) {
  load_filelock()
  pkg_data$ns$filelock$unlock(lock)
}

load_filelock <- function(download = FALSE) {
  ## Maybe we have a useable instance
  if (!is.null(pkg_data$ns$filelock)) return()

  ## Try to load it from the private library
  tryCatch({
    load_private_package("filelock", "c_", create = FALSE)
  }, error = function(e) e)
  if (!is.null(pkg_data$ns$filelock)) return()

  ## Try to load it from the regular library
  tryCatch({
    fl <- find.package("filelock")
    load_private_package("filelock", "c_", lib = dirname(fl))
  }, error = function(e) e)
  if (!is.null(pkg_data$ns$filelock)) return()

  ## Try to download and install it into a temporary directory
  if (download) {
    mkdirp(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    utils::install.packages("filelock", lib = tmp)
    load_private_package("filelock", "c_", lib = tmp)
  }

  if (is.null(pkg_data$ns$filelock)) {
    stop("Cannot lock private library, cannot install filelock package")
  }
}
