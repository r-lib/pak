is_scalarlist_r <- function(x) {
  if (!is.list(x)) return(FALSE)
  for (i in x) {
    if (!is.atomic(i) || length(i) > 1) return(FALSE)
  }
  return(TRUE)
}

#' @useDynLib jsonlite C_is_scalarlist
is_scalarlist_c <- function(x) {
  .Call(C_is_scalarlist, x)
}

is.scalarlist <- is_scalarlist_c
