
pkgman_data <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  ## TODO: load callr from the private library
  worker <- Sys.getenv("R_PKG_PKGMAN_WORKER", "")
  if (worker == "") {
    ## In the main process
    new_remote_session()

  } else if (worker == "true") {
    ## In the worker process
    Sys.setenv("R_PKG_PKGMAN_WORKER" = "false")
    options(
      crayon.enabled = (Sys.getenv("R_PKG_PKGMAN_COLORS") == "TRUE"),
      crayon.colors = as.numeric(Sys.getenv("R_PKG_PKGMAN_NUM_COLORS", "1"))
    )
    use_private_lib()
    cliapp::start_app(theme = cliapp::simple_theme())

  } else {
    ## In a subprocess of a worker
    use_private_lib()
  }
}
