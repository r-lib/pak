
pkgman_data <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  pkgman_data$ns <- list()

  worker <- Sys.getenv("R_PKG_PKGMAN_WORKER", "")
  if (worker == "") {
    ## In the main process
    load_private_package("crayon")
    load_private_package("rappdirs")
    load_private_package("ps")
    load_private_package("processx", "c_")
    load_private_package("callr")
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
