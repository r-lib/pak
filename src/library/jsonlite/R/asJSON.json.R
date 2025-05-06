# If an object has already been encoded by toJSON(), do not encode it again
setMethod("asJSON", "json", function(x, json_verbatim = FALSE, ...) {
  if (isTRUE(json_verbatim)) {
    x
  } else {
    asJSON(as.character(x), ...)
  }
})
