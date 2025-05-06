# Note that this is different from RJSONIO because null values are NA.
setMethod("asJSON", "NULL", function(x, null = "list", ...) {
  if (null == "null") {
    return("null")
  } else {
    return("{}")
  }
})
