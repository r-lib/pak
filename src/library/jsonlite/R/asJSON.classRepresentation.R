# classRepresentation is an object that defines an S4 class encoding it usually
# doesn't serve much purpose, however as we don't wnat to encode it as a regular
# S4 data object.

# it currently only encodes the slots. we could add encoding of methods of that
# would be desired.

setMethod("asJSON", "classRepresentation", function(x, ...) {
  return(asJSON(attributes(x)$slots, ...))
})
