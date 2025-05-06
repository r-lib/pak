verbatim_logical <- function(x) {
  result <- tolower(as.logical(x))
  class(result) <- "verbatim"
  return(result)
}
