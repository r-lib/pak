#' Deprecated functions
#'
#' Use [as_root_criterion()] and [is_root_criterion()], respectively.
#'
#' @inheritParams as_root_criterion
#' @inheritParams is_root_criterion
#' @keywords internal
#' @name deprecated
#' @export
as.root_criterion <- function(...) {
  as_root_criterion(...)
}

#' @export
#' @rdname deprecated
is.root_criterion <- function(...) {
  is_root_criterion(...)
}
