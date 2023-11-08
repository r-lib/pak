# Note: 'symbol' is the same thing as 'name' For some reason, as.name('') gives
# an error, even though it is needed sometimes. This is a workaround
makesymbol <- function(x) {
  if (missing(x) || nchar(x) == 0) {
    return(substitute())
  } else {
    as.name(x)
  }
}
