setMethod("asJSON", "ITime", function(x, ...) {
  asJSON(as.character(x), ...)
})
