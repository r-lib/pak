
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

  ## TODO: try to restart it if dead

  ## TODO Kill it if it is busy,
  ## Actually we should kill it if it is busy in on.exit(), and
  ## start a new instance. If it is starting, then we wait for it.

  rs <- pkgman_data$remote
  state <- rs$get_state()
  if (state %in% c("busy", "starting")) {
    pr <- callr::poll(list(rs$get_poll_connection()), 5000)[[1]]
    state <- rs$get_state()
    if (state == "starting") {
      rs$read()
      state <- rs$get_state()
    }
  }
  if (state != "idle") stop("Subprocess is busy or cannot start")

  res <- withCallingHandlers(
    rs$run_with_output(func, args),
    "cliapp_message" = function(x) message(x$message, appendLF = FALSE))
  if (!is.null(res$error)) stop(res$error)

  res$result
}

new_remote_session <- function() {
  opts <- callr::r_session_options(stderr = NULL,  stdout = NULL)
  opts$env <- c(
    opts$env, R_PKG_SHOW_PROGRESS = "true",
    R_PKG_PKGMAN_WORKER = "true",
    R_PKG_PKGMAN_COLORS = as.character(crayon::has_color()),
    R_PKG_PKGMAN_NUM_COLORS = as.character(crayon::num_colors()))
  pkgman_data$remote <- callr::r_session$new(opts, wait = FALSE)
}
