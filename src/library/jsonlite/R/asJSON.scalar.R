setMethod("asJSON", "scalar", function(x, collapse, ...) {
  # TODO: There must be a way to do this with NextMethod()
  if (length(class(x)) > 1) {
    class(x) <- class(x)[-1]
  } else {
    x <- unclass(x)
  }

  # Print JSON without []
  return(asJSON(x, collapse = FALSE, ...))
})
