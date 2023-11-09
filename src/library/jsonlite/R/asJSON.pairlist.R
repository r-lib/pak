setMethod("asJSON", "pairlist", function(x, ...) {
  asJSON(as.vector(x, mode = "list"), ...)
})
