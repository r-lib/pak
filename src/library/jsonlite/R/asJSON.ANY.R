#' @import methods
setMethod("asJSON", "ANY", function(x, force = FALSE, ...) {
  if (isS4(x) && !is(x, "classRepresentation")) {
    if (isTRUE(force)) {
      return(asJSON(attributes(x), force = force, ...))
    } else {
      stop("No method for S4 class:", class(x))
    }
  } else if (length(class(x)) > 1) {
    # If an object has multiple classes, we recursively try the next class. This is
    # S3 style dispatching that doesn't work by default for formal method definitions
    # There should be a more native way to accomplish this
    return(asJSON(structure(x, class = class(x)[-1]), force = force, ...))
  } else if (isTRUE(force) && existsMethod("asJSON", class(unclass(x))[1])) {
    # As a last resort we can force encoding using the unclassed object
    return(asJSON(unclass(x), force = force, ...))
  } else if (isTRUE(force)) {
    return(asJSON(NULL))
    warning("No method asJSON S3 class: ", class(x))
  } else {
    # If even that doesn't work, we give up.
    stop("No method asJSON S3 class: ", class(x))
  }
})
