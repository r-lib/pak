#' Alternative logical handler
#'
#' A yaml handler function that causes logical vectors to emit
#' `true`/`false` instead of `yes`/`no` values.
#'
#' Pass this function to [`as.yaml()`][as.yaml] as part of the
#' `handlers` argument list like `list(logical = verbatim_logical)`.
#'
#' @param x Logical vector to convert to `true`/`false`.
#' @return Returns a vector of strings of either `true` or `false` of
#' class `verbatim`.
#' @author Charles Dupont and James Goldie (jimjam-slam)
#' @seealso [as.yaml()]
#' @keywords data manip
#' @export
#' @examples
#'
#' vector <- c(TRUE, FALSE, TRUE)
#'
#' as.yaml(vector, handlers=list(logical=verbatim_logical))
#'
verbatim_logical <- function(x) {
  result <- tolower(as.logical(x))
  class(result) <- "verbatim"
  return(result)
}
