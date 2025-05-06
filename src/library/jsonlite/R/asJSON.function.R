setMethod("asJSON", "function", function(x, collapse = TRUE, fun = c("source", "list"), ...) {
  # validate
  fun <- match.arg(fun)

  if (fun == "source") {
    return(asJSON(deparse(x), ...))
  } else {
    return(asJSON(as.list(x), ...))
  }
})
