setMethod("asJSON", "AsIs", function(x, auto_unbox = FALSE, ...) {
  # Strip off the AsIs class so we can dispatch to other asJSON methods.
  class(x) <- setdiff(class(x), "AsIs")

  if (is.atomic(x) && length(x) == 1) {
    # Never auto_unbox single values when wrapped with I()
    asJSON(x, auto_unbox = FALSE, ...)
  } else {
    asJSON(x, auto_unbox = auto_unbox, ...)
  }
})
