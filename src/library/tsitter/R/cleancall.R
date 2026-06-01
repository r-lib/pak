call_with_cleanup <- function(ptr, ...) {
  .Call(c_cleancall_call, pairlist(ptr, ...), parent.frame())
}
