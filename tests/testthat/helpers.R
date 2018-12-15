
if_fail <- function(expr, fn) {
  withCallingHandlers(expr, expectation_failure = fn)
}
