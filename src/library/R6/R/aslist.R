#' Create a list from an R6 object
#'
#' This returns a list of public members from the object. It simply calls
#' \code{as.list.environment}.
#'
#' @param x An R6 object.
#' @param ... Other arguments, which will be ignored.
#'
#' @export
as.list.R6 <- function(x, ...) {
  as.list.environment(x, all.names = TRUE)
}
