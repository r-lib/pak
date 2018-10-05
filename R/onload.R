
pkgman_data <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  ## TODO: load callr from the private library
  if (Sys.getenv("R_PKG_PKGMAN_WORKER", "") == "" &&
      should_remote()) pkgman_data$remote <- new_remote_session()
}
