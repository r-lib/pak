
pkgman_data <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  ## TODO: load callr from the private library
  if (Sys.getenv("R_PKG_PKGMAN_WORKER", "") == "") {
    ## In the main process
    pkgman_data$remote <- new_remote_session()

  } else  {
    ## In the worker process
    Sys.unsetenv("R_PKG_PKGMAN_WORKER")
    options(
      crayon.enabled = (Sys.getenv("R_PKG_PKGMAN_COLORS") == "TRUE"),
      crayon.colors = as.numeric(Sys.getenv("R_PKG_PKGMAN_NUM_COLORS", "1"))
    )
  }
}
