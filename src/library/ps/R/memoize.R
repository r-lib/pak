
## nocov start
memoize <- function(fun) {
  fun
  cache <- NULL
  if (length(formals(fun)) > 0) {
    stop("Only memoizing functions without arguments")
  }
  dec <- function() {
    if (is.null(cache)) cache <<- fun()
    cache
  }
  attr(dec, "clear") <- function() cache <<- TRUE
  class(dec) <- c("memoize", class(dec))
  dec
}

`$.memoize`  <- function(x, name) {
  switch(
    name,
    "clear" = attr(x, "clear"),
    stop("unknown memoize method")
  )
}
## nocov end
