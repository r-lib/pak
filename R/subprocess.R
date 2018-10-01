
## ----------------------------------------------------------------------
## Helper functions
## ----------------------------------------------------------------------

remote_is_alive <- function() {
  inherits(rs <- pkgman_data$remote, "process") && rs$is_alive()
}

should_remote <- function() {
  !isFALSE(getOption("pkgman.subprocess"))
}

remote <- function(func, args = list()) {
  if (!should_remote() || !remote_is_alive()) {
    return(do.call(func, args))
  }

  rs <- pkgman_data$remote
  state <- rs$get_state()
  if (state %in% c("busy", "starting")) {
    pr <- callr::poll(list(rs$get_poll_connection()), 5000)[[1]]
    state <- rs$get_state()
  }
  if (state != "idle") stop("Subprocess is busy or cannot start")

  withCallingHandlers(
    rs$run(func, args),
    "callr_message" = function(x) print(x))
}
