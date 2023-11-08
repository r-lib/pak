setMethod("asJSON", "vctrs_vctr", function(x, ...) {
  # dispatch based on the underlying type
  class(x) <- setdiff(class(x), 'vctrs_vctr')
  asJSON(x, ...)
})
