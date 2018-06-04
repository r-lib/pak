
remote_is_alive <- function() {
  inherits(rs <- pkgman_data$remote, "process") && rs$is_alive()
}

should_remote <- function() {
  !isFALSE(getOption("pkgman.subprocess"))
}

remote <- function(func) {
  if (should_remote() || !remote_is_alive()) {
    return(func())
  }

  rs <- pkgman_data$remote
  state <- rs$get_state()
  if (state %in% c("busy", "starting")) {
    pr <- callr::poll(list(rs$get_poll_connection()), 5000)[[1]]
    state <- rs$get_state()
  }
  if (state != "idle") return(func())

  rs$run(func)$result
}
