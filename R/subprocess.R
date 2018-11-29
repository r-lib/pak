
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

  func2 <- func
  body(func2) <- substitute({
    withCallingHandlers(
      cliapp_message = function(msg) {
        withCallingHandlers(
          cliapp:::cli_server_default(msg),
          message = function(mmsg) {
            class(mmsg) <- c("callr_message", "message", "condition")
            signalCondition(mmsg)
            invokeRestart("muffleMessage")
          }
        )
        invokeRestart("muffleMessage")
      },
      `__body__`
    )},
    list("__body__" = body(func))
  )

  res <- withCallingHandlers(
    callr_message = function(msg) {
      message(msg)
      if (!is.null(findRestart("muffleMessage"))) {
        invokeRestart("muffleMessage")
      }
    },
    rs$run_with_output(func2, args)
  )
  if (!is.null(res$error)) stop(res$error)

  res$result
}

new_remote_session <- function() {
  opts <- callr::r_session_options(stderr = NULL,  stdout = NULL)
  opts$env <- c(
    opts$env, R_PKG_SHOW_PROGRESS = is_verbose(),
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
