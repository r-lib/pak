#' Create a new library
#'
#' @param lib The library directory
#' @export
lib_create <- function(lib) {
  #TODO: what do we do if the directory already exists?

  dir.create(lib, recursive = TRUE)
}

#' Activate or deactivate a given library
#'
#' @param action `[character(1)]` Should lib `"replace'`, `"prepend"` or `"append"` the existing library paths.
#' @inheritParams lib_create
#' @export
lib_activate <- function(lib, action = "replace") {
  lib <- normalizePath(lib, mustWork = TRUE)
  old <- .libPaths()
  .libPaths(merge_new(old, lib, action))
  invisible(old)
}

#' @rdname lib_activate
#' @export
lib_deactivate <- function(lib) {
  lib <- normalizePath(lib, mustWork = TRUE)

  old <- .libPaths()
  .libPaths(setdiff(old, lib))
}

#' Create a session specific temporary library
#'
#' @param dir Directory to create the library in
#' @inheritParams base::tempfile
#' @export
lib_temporary <- function(dir = tempdir(), pattern = "library-") {
  lib <- tempfile(pattern = pattern, tmpdir = dir)
  dir.create(lib)
  lib_activate(lib)
}
