#' @useDynLib foo foo_
#' @export
foo <- function() {
  .Call(foo_)
}

.onUnload <- function(libpath) {
  library.dynam.unload("foo", libpath)
}
