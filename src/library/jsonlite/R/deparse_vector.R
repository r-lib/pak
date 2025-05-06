#' @useDynLib jsonlite C_escape_chars
deparse_vector_c <- function(x) {
  .Call(C_escape_chars, x)
}

deparse_vector_r <- function(x) {
  stopifnot(is.character(x))
  if (!length(x)) return(x)
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  x <- gsub("\b", "\\b", x, fixed = TRUE)
  x <- gsub("\f", "\\f", x, fixed = TRUE)
  paste0("\"", x, "\"")
}

# Which implementation to use
deparse_vector <- deparse_vector_c

#Below are older implementations of the same function
deparse_vector_old <- function(x) {
  stopifnot(is.character(x))
  x <- gsub("[\v\a]", "", x)
  vapply(x, deparse, character(1), USE.NAMES = FALSE)
}
