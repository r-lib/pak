#' @useDynLib jsonlite C_is_recordlist
is_recordlist_c <- function(x) {
  .Call(C_is_recordlist, x)
}

is_recordlist_r <- function(x) {
  if (!(is.unnamedlist(x) && length(x))) {
    return(FALSE)
  }
  at_least_one_object = FALSE
  for (i in x) {
    if (!(is.namedlist(i) || is.null(i))) return(FALSE)
    if (!at_least_one_object && is.namedlist(i)) at_least_one_object <- TRUE
  }
  return(at_least_one_object)
}

is.recordlist <- is_recordlist_c

is.namedlist <- function(x) {
  isTRUE(is.list(x) && !is.null(names(x)))
}

is.unnamedlist <- function(x) {
  isTRUE(is.list(x) && is.null(names(x)))
}
