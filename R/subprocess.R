
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
  restart_remote_if_needed()
  on.exit(restart_remote_if_needed(), add = TRUE)

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

restart_remote_if_needed <- function() {
  "!DEBUG Restarting background process"
  rs <- pkgman_data$remote
  if (inherits(rs, "r_session") &&
      rs$is_alive() &&
      rs$get_state() != "busy") return()

  ## Try to interrupt nicely (SIGINT/CTRL+C), if that fails within 100ms,
  ## kill it.
  rs$interrupt()
  rs$wait(100)
  rs$kill()
  new_remote_session()
}
