
get_private_lib <- function() {
  pkgman_data$private_lib %||% create_private_lib()
}

private_lib_dir <- function()  {
  with_package(
    "rappdirs",
    file.path(rappdirs::user_cache_dir("R-pkg"), "lib")
  )
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

    data_rds <- file.path(target_dir, "pkgman-data.rds")
    if (!file.exists(data_rds)) return(TRUE)

    old <- readRDS(data_rds)
    new_hash <- get_package_hash(pkgdir)
    new_ts <- file_mtime(file.path(pkgdir, "DESCRIPTION"))
    ! identical(old$hash, new_hash) || new_ts >= old$ts
  }, error = function(e) return(TRUE))
}

copy_package <- function(from, lib) {
  file.copy(from, lib, recursive = TRUE)
  pkgdir <- file.path(lib, basename(from))
  hash <- get_package_hash(pkgdir)
  saveRDS(
    list(hash = hash, ts = Sys.time()),
    file = file.path(pkgdir, "pkgman-data.rds")
  )
}

create_private_lib <- function() {
  lib <- private_lib_dir()
  pkgman_data$deps <- pkgman_data$deps %||% lookup_deps("pkgman")
  pkg_dirs <- pkgman_data$deps
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)

  upd <- vlapply(pkg_dirs, package_needs_update, lib = lib)
  if (any(upd)) {
    with_package("filelock", {
      l <-  filelock::lock(file.path(lib, "pkgman-lib.lock"))
      if (is.null(l)) stop("Cannot create private lib, cannot lock")
      on.exit(filelock::unlock(l))
      for(i in which(upd)) copy_package(pkg_dirs[i], lib)
    })
  }

  pkgman_data$private_lib <- lib
  lib
}

#' @importFrom utils head

lookup_deps <- function(package) {
  path <- getNamespaceInfo(asNamespace(package), "path")
  lib_path <- .libPaths()
  lib_pkgs <- lapply(lib_path, dir)
  done <- package
  result <- character()
  todo <- path

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

  deps <- str_trim(strsplit(deps, ",")[[1]])
  deps <- lapply(strsplit(deps, "\\("), str_trim)
  deps <- lapply(deps, sub, pattern = "\\)$", replacement = "")
  setdiff(vapply(deps, "[", "", 1), c("R", base_packages()))
}

## TODO: in theory the set of base packages can change over time,
## so we would need an R version specific vector here.
## Not an issue currently, might be in the future.

base_packages <- function() {
  if (is.null(pkgman_data$base_packages)) {
    pkgman_data$base_packages <-
      rownames(utils::installed.packages(priority = "base"))
  }
  pkgman_data$base_packages
}
