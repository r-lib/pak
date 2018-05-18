
pkgman_data <- new.env(parent = emptyenv())

get_private_lib <- function() {
  pkgman_data$private_lib %||% create_private_lib()
}

create_private_lib <- function() {
  dir.create(lib <- tempfile())
  to_copy <- lookup_deps("pkgman")
  file.copy(to_copy, lib, recursive = TRUE)
  pkgman_data$private_lib <- lib
  lib
}

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

#' @importFrom utils installed.packages

base_packages <- function() {
  if (is.null(pkgman_data$base_packages)) {
    pkgman_data$base_packages <-
      rownames(installed.packages(priority = "base"))
  }
  pkgman_data$base_packages
}
