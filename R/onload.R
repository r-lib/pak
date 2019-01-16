
pkg_data <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  pkg_data$ns <- list()

  worker <- Sys.getenv("R_PKG_PKG_WORKER", "")
  if (worker == "") {
    ## In the main process
    try_new_remote_session()

  } else if (worker == "true") {
    ## In the worker process
    Sys.setenv("R_PKG_PKG_WORKER" = "false")
    options(
      crayon.enabled = (Sys.getenv("R_PKG_PKG_COLORS") == "TRUE"),
      crayon.colors = as.numeric(Sys.getenv("R_PKG_PKG_NUM_COLORS", "1"))
    )
    use_private_lib()
    cliapp::start_app(theme = cliapp::simple_theme())

  } else {
    ## In a subprocess of a worker
    use_private_lib()
  }

  invisible()
}
