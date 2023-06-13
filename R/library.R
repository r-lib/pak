

#' Status of packages in a library
#'
#' @param lib Path to library.
#' @return Data frame the contains data about the packages
#'   installed in the library.
#'   \eval{include_docs("pkgdepends", "docs/lib-status-return.rds")}

#' @export
#' @family library functions
#' @section Examples:
#' ```{asciicast lib-status-2}
#' lib_status(.Library)
#' ```

lib_status <- function(lib = .libPaths()[1]) {
  load_extra("pillar")
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
