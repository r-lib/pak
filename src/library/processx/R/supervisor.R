# Stores information about the supervisor process
supervisor_info <- new.env()

reg.finalizer(supervisor_info, function(s) {
  # Pass s to `supervisor_kill`, in case the GC event happens _after_ a new
  # `processx:::supervisor_info` has been created and the name
  # `supervisor_info` is bound to the new object. This could happen if the
  # package is unloaded and reloaded.
  supervisor_kill2(s)
}, onexit = TRUE)

#' Terminate all supervised processes and the supervisor process itself as
#' well
#'
#' On Unix the supervisor sends a `SIGTERM` signal to all supervised
#' processes, and gives them five seconds to quit, before sending a
#' `SIGKILL` signal. Then the supervisor itself terminates.
#'
#' Windows is similar, but instead of `SIGTERM`, a console CTRL+C interrupt
#' is sent first, then a `WM_CLOSE` message is sent to the windows of the
#' supervised processes, if they have windows.
#'
#' @keywords internal
#' @export

supervisor_kill <- function() {
  supervisor_kill2()
}

# This takes an object s, because a new `supervisor_info` object could have been
# created.
supervisor_kill2 <- function(s = supervisor_info) {
  if (is.null(s$pid))
    return()

  if (!is.null(s$stdin) && is_pipe_open(s$stdin)) {
    write_lines_named_pipe(s$stdin, "kill")
  }

  if (!is.null(s$stdin) && is_pipe_open(s$stdin)) {
    close_named_pipe(s$stdin)
  }
  if (!is.null(s$stdout) && is_pipe_open(s$stdout)) {
    close_named_pipe(s$stdout)
  }

  s$pid <- NULL
}


supervisor_reset <- function() {
  if (supervisor_running()) {
    supervisor_kill()
  }

  supervisor_info$pid         <- NULL
  supervisor_info$stdin       <- NULL
  supervisor_info$stdout      <- NULL
  supervisor_info$stdin_file  <- NULL
  supervisor_info$stdout_file <- NULL
}


supervisor_ensure_running <- function() {
  if (!supervisor_running())
    supervisor_start()
}


supervisor_running <- function() {
  if (is.null(supervisor_info$pid)) {
    FALSE
  } else {
    TRUE
  }
}


# Tell the supervisor to watch a PID
supervisor_watch_pid <- function(pid) {
  supervisor_ensure_running()
  write_lines_named_pipe(supervisor_info$stdin, as.character(pid))
}


# Tell the supervisor to un-watch a PID
supervisor_unwatch_pid <- function(pid) {
  write_lines_named_pipe(supervisor_info$stdin, as.character(-pid))
}


# Start the supervisor process. Information about the process will be stored in
# supervisor_info. If startup fails, this function will throw an error.
supervisor_start <- function() {

  supervisor_info$stdin_file  <- named_pipe_tempfile("supervisor_stdin")
  supervisor_info$stdout_file <- named_pipe_tempfile("supervisor_stdout")

  supervisor_info$stdin  <- create_named_pipe(supervisor_info$stdin_file)
  supervisor_info$stdout <- create_named_pipe(supervisor_info$stdout_file)

  # Start the supervisor, passing the R process's PID to it.
  # Note: for debugging, you can add "-v" to args and use stdout="log.txt".
  p <- process$new(
    supervisor_path(),
    args = c("-p", Sys.getpid(), "-i", supervisor_info$stdin_file),
    stdout = "|",
    cleanup = FALSE
  )

  # Wait for supervisor to emit the line "Ready", which indicates it is ready
  # to receive information.
  ready <- FALSE
  cur_time <- Sys.time()
  end_time <- cur_time + 5
  while (cur_time < end_time) {
    p$poll_io(round(as.numeric(end_time - cur_time, units = "secs") * 1000))

    if (!p$is_alive())
      break

    if (any(p$read_output_lines() == "Ready")) {
      ready <- TRUE
      break
    }

    cur_time <- Sys.time()
  }

  if (p$is_alive())
    close(p$get_output_connection())

  # Two ways of reaching this: if process has died, or if it hasn't emitted
  # "Ready" after 5 seconds.
  if (!ready)
    throw(new_error("processx supervisor was not ready after 5 seconds."))

  supervisor_info$pid <- p$get_pid()
}


# Returns full path to the supervisor binary. Works when package is loaded the
# normal way, and when loaded with devtools::load_all().
supervisor_path <- function() {
  supervisor_name <- "supervisor"
  if (is_windows())
    supervisor_name <- paste0(supervisor_name, ".exe")

  # Detect if package was loaded via devtools::load_all()
  dev_meta <- parent.env(environment())$.__DEVTOOLS__
  devtools_loaded <- !is.null(dev_meta)

  if (devtools_loaded) {
    subdir <- file.path("src", "supervisor")
  } else {
    subdir <- "bin"
    # Add arch (it may be ""; on Windows it may be "/X64")
    subdir <- paste0(subdir, Sys.getenv("R_ARCH"))
  }

  system.file(subdir, supervisor_name, package = "processx", mustWork = TRUE)
}
