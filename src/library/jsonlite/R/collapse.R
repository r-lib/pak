#' @useDynLib jsonlite C_collapse_array C_collapse_array_pretty_inner C_collapse_array_pretty_outer
collapse <- function(x, inner = TRUE, indent = 0L) {
  if (is.na(indent)) {
    .Call(C_collapse_array, x)
  } else if (isTRUE(inner)) {
    .Call(C_collapse_array_pretty_inner, x)
  } else {
    .Call(C_collapse_array_pretty_outer, x, indent)
  }
}

#' @useDynLib jsonlite C_row_collapse_array
row_collapse <- function(m, indent = NA_integer_) {
  .Call(C_row_collapse_array, m, indent = indent)
}


# Iteratively collapse a high dimensional matrix / array
# Does not perform the final collapse, which is done in asJSON.array()
collapse_array <- function(x, columnmajor = FALSE, indent) {
  # dimensionality of the array
  n <- length(dim(x))

  # Collapse the inner vectors
  dim <- 1:(n - 1) + as.numeric(columnmajor)
  x <- apply(x, dim, collapse, inner = TRUE, indent = indent)

  # Collapse higher dimensions
  for (i in rev(seq_along(dim(x)))[-1]) {
    dim <- 1:(length(dim(x)) - 1) + as.numeric(columnmajor)
    x <- apply(x, dim, collapse, inner = FALSE, indent = indent_increment(indent) * i)
  }
  x
}
