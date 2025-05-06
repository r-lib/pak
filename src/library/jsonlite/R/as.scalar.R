as.scalar <- function(obj) {
  # Lists can never be a scalar (this can arise if a dataframe contains a column
  # with lists)
  if (length(dim(obj)) > 1) {
    if (!identical(nrow(obj), 1L)) {
      warning("Tried to use as.scalar on an array or dataframe with ", nrow(obj), " rows.", call. = FALSE)
      return(obj)
    }
  } else if (!identical(length(obj), 1L)) {
    warning("Tried to use as.scalar on an object of length ", length(obj), call. = FALSE)
    return(obj)
  } else if (is.namedlist(obj)) {
    warning("Tried to use as.scalar on a named list.", call. = FALSE)
    return(obj)
  }

  class(obj) <- c("scalar", class(obj))
  return(obj)
}
