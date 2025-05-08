#' Query UUID identifying the version of the R API
#'
#' Packages need to be recompiled if this id changes.
#'
#' @export
#' @return String, a UUID.
#' @examples
#' get_internals_id()

get_internals_id <- function() {
  get(".Internal", baseenv())(internalsID())
}

internalsID <- function() NULL

#' Query the version of the graphics API
#'
#' A package compiled with a certain version of the graphics API
#' will not work with R installations that use a different version.
#'
#' @export
#' @return An integer scalar, the version of the graphics API of this
#' R version.
#' @examples
#' get_graphics_api_version()

get_graphics_api_version <- function() {
  .Call(pkgcache_graphics_api_version)
}
