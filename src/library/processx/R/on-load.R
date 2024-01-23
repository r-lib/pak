
## nocov start

if (Sys.getenv("PAK_INSTALL_DUMMY_CROSS") != "true") {

.onLoad <- function(libname, pkgname) {
  ## This is needed to fix the boot time to a given value,
  ## because in a Docker container (maybe elsewhere as well?) on
  ## Linux it can change (!).
  ## See https://github.com/r-lib/processx/issues/258
  if (ps::ps_is_supported()) {
    ps::ps_handle()
    bt <- ps::ps_boot_time()
    .Call(c_processx__set_boot_time, bt)
  }

  supervisor_reset()
  if (Sys.getenv("DEBUGME", "") != "" &&
      requireNamespace("debugme", quietly = TRUE)) {
    debugme::debugme()
  }

  err$onload_hook()
}

.onUnload <- function(libpath) {
  chain_call(c_processx__unload_cleanup)
  supervisor_reset()
}

}
## nocov end
