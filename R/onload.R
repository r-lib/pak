
pkgman_data <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  if (should_remote()) pkgman_data$remote <- callr::r_session$new()
}
