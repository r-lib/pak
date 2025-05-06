#' @useDynLib jsonlite C_null_to_na
null_to_na <- function(x) {
  .Call(C_null_to_na, x)
}

#' @useDynLib jsonlite C_is_datelist
is_datelist <- function(x) {
  .Call(C_is_datelist, x)
}
