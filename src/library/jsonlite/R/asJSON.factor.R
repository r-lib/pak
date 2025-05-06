setMethod("asJSON", "factor", function(x, factor = c("string", "integer"), keep_vec_names = FALSE, ...) {
  # validate
  factor <- match.arg(factor)

  # dispatch
  if (factor == "integer") {
    # encode factor as enum
    asJSON(unclass(x), ...)
  } else {
    # encode as strings
    xc <- as.character(x)
    if (isTRUE(keep_vec_names)) {
      names(xc) <- names(x)
    }
    asJSON(xc, keep_vec_names = keep_vec_names, ...)
  }
})
