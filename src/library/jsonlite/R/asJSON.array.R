asjson_array_fun <- function(x, collapse = TRUE, na = NULL, oldna = NULL, matrix = c("rowmajor", "columnmajor"), auto_unbox = FALSE, keep_vec_names = FALSE, indent = NA_integer_, ...) {
  matrix <- match.arg(matrix)

  # reset na arg when called from data frame
  if (identical(na, "NA")) {
    na <- oldna
  }

  # 1D arrays are vectors
  if (length(dim(x)) < 2) {
    return(asJSON(c(x), matrix = matrix, na = na, indent = indent_increment(indent), ...))
  }

  # if collapse == FALSE, then this matrix is nested inside a data frame,
  # and therefore row major must be forced to match dimensions
  if (identical(matrix, "columnmajor") && collapse == FALSE) {
    return(apply(x, 1, asJSON, matrix = matrix, na = na, indent = indent_increment(indent), ...))
  }

  # dont pass auto_unbox (never unbox within matrix)
  m <- asJSON(c(x), collapse = FALSE, matrix = matrix, na = na, ...)
  dim(m) <- dim(x)
  tmp <- if (length(dim(x)) == 2 && identical(matrix, "rowmajor")) {
    # Faster special case for 2D matrices
    row_collapse(m, indent = indent_increment(indent))
  } else {
    collapse_array(m, columnmajor = identical(matrix, "columnmajor"), indent = indent)
  }

  # collapse it
  if (collapse) {
    collapse(tmp, inner = FALSE, indent = indent)
  } else {
    tmp
  }
}

# Some objects have class Matrix but not class Array
setMethod("asJSON", "array", asjson_array_fun)
setMethod("asJSON", "matrix", asjson_array_fun)
