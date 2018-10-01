
cli <- NULL

.onLoad <- function(libname, pkgname) {
  cli <<- cliapp::cliapp$new()
}
