#' @useDynLib jsonlite C_transpose_list
transpose_list <- function(x, names) {
  .Call(C_transpose_list, x, names)
}
