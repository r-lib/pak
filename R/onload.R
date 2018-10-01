
pkgman_data <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  ## TODO: load callr from the private library
  if (should_remote()) pkgman_data$remote <- new_remote_session()
}
