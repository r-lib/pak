#' Is an object an R6 Class Generator or Object?
#'
#' Checks for R6 class generators and R6 objects.
#' @param x An object.
#' @return A logical value.
#' \itemize{
#' \item{\code{is.R6Class} returns \code{TRUE} when the input is an R6 class
#' generator and \code{FALSE} otherwise.}
#' \item{\code{is.R6} returns \code{TRUE} when the input is an R6 object and
#' \code{FALSE} otherwise.}
#' }
#' @examples
#' class_generator <- R6Class()
#' object <- class_generator$new()
#'
#' is.R6Class(class_generator)
#' is.R6(class_generator)
#'
#' is.R6Class(object)
#' is.R6(object)
#' @export
is.R6 <- function(x) {
  inherits(x, "R6")
}

#' @rdname is.R6
#' @export
is.R6Class <- function(x) {
  inherits(x, "R6ClassGenerator")
}
