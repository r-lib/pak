#' Set up private pak library (deprecated)
#'
#' This function is deprecated and does nothing.
#' Recent versions of pak do not need a `pak_setup()` call.
#'
#' @param mode Where to get the packages from. "download" will try to
#'   download them from CRAN. "copy" will try to copy them from your
#'   current "regular" package library. "auto" will try to copy first,
#'   and if that fails, then it tries to download.
#' @param quiet Whether to omit messages.
#' @return The path to the private library, invisibly.
#'
#' @export

pak_setup <- function(mode = c("auto", "download", "copy"), quiet = FALSE) {
  warning("`pak_setup()` is deprecated and does nothing.")
  return(invisible())
}
