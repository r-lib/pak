#setOldClass("int64")
#setMethod("asJSON", "int64", function(x, digits, ...) {
#  asJSON(as.double(as.character(x)), digits = 0, ...)
#})
