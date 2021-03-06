

#' Status of packages in a library
#'
#' @param lib Path to library.
#' @return Data frame (tibble) the contains data about the packages
#'   installed in the library.
#'
#' @export
#' @family library functions

lib_status <- function(lib = .libPaths()[1]) {
  load_extra("tibble")
  remote(
    function(...) asNamespace("pak")$lib_status_internal(...),
    list(lib = lib))
}

## TODO: lib_check()
## Run the solver and make sure all packages in the library are fine

## TODO: lib_doctor()
## Check if packages can be loaded, also for broken DLL files on Windows
## Maybe this should be merged into lib_check().

## TODO: lib_update_status()

## TODO: lib_update()

## ----------------------------------------------------------------------

lib_status_internal <- function(lib) {
  pkgdepends::lib_status(lib)
}
