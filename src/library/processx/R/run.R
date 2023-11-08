#' Run external command, and wait until finishes
#'
#' `run` provides an interface similar to [base::system()] and
#' [base::system2()], but based on the [process] class. This allows some
#' extra features, see below.
#'
#' `run` supports
#' * Specifying a timeout for the command. If the specified time has
#'   passed, and the process is still running, it will be killed
#'   (with all its child processes).
#' * Calling a callback function for each line or each chunk of the
#'   standard output and/or error. A chunk may contain multiple lines, and
#'   can be as short as a single character.
#' * Cleaning up the subprocess, or the whole process tree, before exiting.
#'
#' @section Callbacks:
#'
#' Some notes about the callback functions. The first argument of a
#' callback function is a character scalar (length 1 character), a single
#' output or error line. The second argument is always the [process]
#' object. You can manipulate this object, for example you can call
#' `$kill()` on it to terminate it, as a response to a message on the
#' standard output or error.
#'
#' @section Error conditions:
#'
#' `run()` throws error condition objects if the process is interrupted,
#' timeouts or fails (if `error_on_status` is `TRUE`):
#' * On interrupt, a condition with classes `system_command_interrupt`,
#'   `interrupt`, `condition` is signalled. This can be caught with
#'   `tryCatch(..., interrupt = ...)`.
#' * On timeout, a condition with classes `system_command_timeout_error`,
#'   `system_command_error`, `error`, `condition` is thrown.
#' * On error (if `error_on_status` is `TRUE`), an error with classes
#'   `system_command_status_error`, `system_command_error`, `error`,
#'   `condition` is thrown.
#'
#' All of these conditions have the fields:
#' * `message`: the error message,
#' * `stderr`: the standard error of the process, or the standard output
#'    of the process if `stderr_to_stdout` was `TRUE`.
#' * `call`: the captured call to `run()`.
#' * `echo`: the value of the `echo` argument.
#' * `stderr_to_stdout`: the value of the `stderr_to_stdout` argument.
#' * `status`: the exit status for `system_command_status_error` errors.
#'
#' @param command Character scalar, the command to run. If you are
#'   running `.bat` or `.cmd` files on Windows, make sure you read the
#'   'Batch files' section in the [process] manual page.
#' @param args Character vector, arguments to the command.
#' @param error_on_status Whether to throw an error if the command returns
#'   with a non-zero status, or it is interrupted. The error classes are
#'   `system_command_status_error` and `system_command_timeout_error`,
#'   respectively, and both errors have class `system_command_error` as
#'   well. See also "Error conditions" below.
#' @param wd Working directory of the process. If `NULL`, the current
#'   working directory is used.
#' @param echo_cmd Whether to print the command to run to the screen.
#' @param echo Whether to print the standard output and error
#'   to the screen. Note that the order of the standard output and error
#'   lines are not necessarily correct, as standard output is typically
#'   buffered. If the standard output and/or error is redirected to a
#'   file or they are ignored, then they also not echoed.
#' @param spinner Whether to show a reassuring spinner while the process
#'   is running.
#' @param timeout Timeout for the process, in seconds, or as a `difftime`
#'   object. If it is not finished before this, it will be killed.
#' @param stdout What to do with the standard output. By default it
#'   is collected in the result, and you can also use the
#'   `stdout_line_callback` and `stdout_callback` arguments to pass
#'   callbacks for output. If it is the empty string (`""`), then
#'   the child process inherits the standard output stream of the
#'   R process. (If the main R process does not have a standard output
#'   stream, e.g. in RGui on Windows, then an error is thrown.)
#'   If it is `NULL`, then standard output is discarded. If it is a string
#'   other than `"|"` and `""`, then it is taken as a file name and the
#'   output is redirected to this file.
#' @param stderr What to do with the standard error. By default it
#'   is collected in the result, and you can also use the
#'   `stderr_line_callback` and `stderr_callback` arguments to pass
#'   callbacks for output. If it is the empty string (`""`), then
#'   the child process inherits the standard error stream of the
#'   R process. (If the main R process does not have a standard error
#'   stream, e.g. in RGui on Windows, then an error is thrown.)
#'   If it is `NULL`, then standard error is discarded. If it is a string
#'   other than `"|"` and `""`, then it is taken as a file name and the
#'   standard error is redirected to this file.
#' @param stdout_line_callback `NULL`, or a function to call for every
#'   line of the standard output. See `stdout_callback` and also more
#'   below.
#' @param stdout_callback `NULL`, or a function to call for every chunk
#'   of the standard output. A chunk can be as small as a single character.
#'   At most one of `stdout_line_callback` and `stdout_callback` can be
#'   non-`NULL`.
#' @param stderr_line_callback `NULL`, or a function to call for every
#'   line of the standard error. See `stderr_callback` and also more
#'   below.
#' @param stderr_callback `NULL`, or a function to call for every chunk
#'   of the standard error. A chunk can be as small as a single character.
#'   At most one of `stderr_line_callback` and `stderr_callback` can be
#'   non-`NULL`.
#' @param stderr_to_stdout Whether to redirect the standard error to the
#'   standard output. Specifying `TRUE` here will keep both in the
#'   standard output, correctly interleaved. However, it is not possible
#'   to deduce where pieces of the output were coming from. If this is
#'   `TRUE`, the standard error callbacks  (if any) are never called.
#' @param env Environment variables of the child process. If `NULL`,
#'   the parent's environment is inherited. On Windows, many programs
#'   cannot function correctly if some environment variables are not
#'   set, so we always set `HOMEDRIVE`, `HOMEPATH`, `LOGONSERVER`,
#'   `PATH`, `SYSTEMDRIVE`, `SYSTEMROOT`, `TEMP`, `USERDOMAIN`,
#'   `USERNAME`, `USERPROFILE` and `WINDIR`. To append new environment
#'   variables to the ones set in the current process, specify
#'   `"current"` in `env`, without a name, and the appended ones with
#'   names. The appended ones can overwrite the current ones.
#' @param windows_verbatim_args Whether to omit the escaping of the
#'   command and the arguments on windows. Ignored on other platforms.
#' @param windows_hide_window Whether to hide the window of the
#'   application on windows. Ignored on other platforms.
#' @param encoding The encoding to assume for `stdout` and
#'   `stderr`. By default the encoding of the current locale is
#'   used. Note that `processx` always reencodes the output of
#'   both streams in UTF-8 currently.
#' @param cleanup_tree Whether to clean up the child process tree after
#'   the process has finished.
#' @param ... Extra arguments are passed to `process$new()`, see
#'   [process]. Note that you cannot pass `stout` or `stderr` here,
#'   because they are used internally by `run()`. You can use the
#'   `stdout_callback`, `stderr_callback`, etc. arguments to manage
#'   the standard output and error, or the [process] class directly
#'   if you need more flexibility.
#' @return A list with components:
#'   * status The exit status of the process. If this is `NA`, then the
#'     process was killed and had no exit status.
#'   * stdout The standard output of the command, in a character scalar.
#'   * stderr The standard error of the command, in a character scalar.
#'   * timeout Whether the process was killed because of a timeout.
#'
#' @export
#' @examplesIf .Platform$OS.type == "unix"
#' # This works on Unix systems
#' run("ls")
#' system.time(run("sleep", "10", timeout = 1, error_on_status = FALSE))
#' system.time(
#'   run(
#'     "sh", c("-c", "for i in 1 2 3 4 5; do echo $i; sleep 1; done"),
#'     timeout = 2, error_on_status = FALSE
#'   )
#' )
#'
#' @examplesIf FALSE
#' # This works on Windows systems, if the ping command is available
#' run("ping", c("-n", "1", "127.0.0.1"))
#' run("ping", c("-n", "6", "127.0.0.1"), timeout = 1,
#'     error_on_status = FALSE)

run <- function(
  command = NULL, args = character(), error_on_status = TRUE, wd = NULL,
  echo_cmd = FALSE, echo = FALSE, spinner = FALSE,
  timeout = Inf, stdout = "|", stderr = "|",
  stdout_line_callback = NULL, stdout_callback = NULL,
  stderr_line_callback = NULL, stderr_callback = NULL,
  stderr_to_stdout = FALSE, env = NULL,
  windows_verbatim_args = FALSE, windows_hide_window = FALSE,
  encoding = "", cleanup_tree = FALSE, ...) {

  assert_that(is_flag(error_on_status))
  assert_that(is_time_interval(timeout))
  assert_that(is_flag(spinner))
  assert_that(is_string_or_null(stdout))
  assert_that(is_string_or_null(stderr))
  assert_that(is.null(stdout_line_callback) ||
              is.function(stdout_line_callback))
  assert_that(is.null(stderr_line_callback) ||
              is.function(stderr_line_callback))
  assert_that(is.null(stdout_callback) || is.function(stdout_callback))
  assert_that(is.null(stderr_callback) || is.function(stderr_callback))
  assert_that(is_flag(cleanup_tree))
  assert_that(is_flag(stderr_to_stdout))
  ## The rest is checked by process$new()
  "!DEBUG run() Checked arguments"

  if (!interactive()) spinner <- FALSE

  ## Run the process
  if (stderr_to_stdout) stderr <- "2>&1"
  pr <- process$new(
    command, args, echo_cmd = echo_cmd, wd = wd,
    windows_verbatim_args = windows_verbatim_args,
    windows_hide_window = windows_hide_window,
    stdout = stdout, stderr = stderr, env = env, encoding = encoding,
    cleanup_tree = cleanup_tree, ...
  )
  "#!DEBUG run() Started the process: `pr$get_pid()`"

  ## We make sure that the process is eliminated
  if (cleanup_tree) {
    on.exit(pr$kill_tree(), add = TRUE)
  } else {
    on.exit(pr$kill(), add = TRUE)
  }

  ## If echo, then we need to create our own callbacks.
  ## These are merged to user callbacks if there are any.
  if (echo) {
    stdout_callback <- echo_callback(stdout_callback, "stdout")
    stderr_callback <- echo_callback(stderr_callback, "stderr")
  }

  ## Make the process interruptible, and kill it on interrupt
  runcall <- sys.call()
  resenv <- new.env(parent = emptyenv())
  has_stdout <- !is.null(stdout) && stdout == "|"
  has_stderr <- !is.null(stderr) && stderr == "|"

  if (has_stdout) {
    resenv$outbuf <- make_buffer()
    on.exit(resenv$outbuf$done(), add = TRUE)
  }
  if (has_stderr) {
    resenv$errbuf <- make_buffer()
    on.exit(resenv$errbuf$done(), add = TRUE)
  }

  res <- tryCatch(
    run_manage(pr, timeout, spinner, stdout, stderr,
               stdout_line_callback, stdout_callback,
               stderr_line_callback, stderr_callback, resenv),
    interrupt = function(e) {
      "!DEBUG run() process `pr$get_pid()` killed on interrupt"
      out <- if (has_stdout) {
        resenv$outbuf$push(pr$read_output())
        resenv$outbuf$push(pr$read_output())
        resenv$outbuf$read()
      }
      err <- if (has_stderr) {
        resenv$errbuf$push(pr$read_error())
        resenv$errbuf$push(pr$read_error())
        resenv$errbuf$read()
      }
      tryCatch(pr$kill(), error = function(e) NULL)
      signalCondition(new_process_interrupt_cond(
        list(
          interrupt = TRUE, stderr = err, stdout = out,
          command = command, args = args
        ),
        runcall, echo = echo, stderr_to_stdout = stderr_to_stdout
      ))
      cat("\n")
      invokeRestart("abort")
    }
  )

  if (error_on_status && (is.na(res$status) || res$status != 0)) {
    "!DEBUG run() error on status `res$status` for process `pr$get_pid()`"
    throw(new_process_error(res, call = sys.call(), echo = echo,
                            stderr_to_stdout, res$status, command = command,
                            args = args))
  }

  res
}

echo_callback <- function(user_callback, type) {
  force(user_callback)
  force(type)
  function(x, ...) {
    if (type == "stderr" && has_package("cli")) x <- cli::col_red(x)
    cat(x, sep = "")
    if (!is.null(user_callback)) user_callback(x, ...)
  }
}

run_manage <- function(proc, timeout, spinner, stdout, stderr,
                       stdout_line_callback, stdout_callback,
                       stderr_line_callback, stderr_callback, resenv) {

  timeout <- as.difftime(timeout, units = "secs")
  start_time <- proc$get_start_time()

  has_stdout <- !is.null(stdout) && stdout == "|"
  has_stderr <- !is.null(stderr) && stderr == "|"

  pushback_out <- ""
  pushback_err <- ""

  do_output <- function() {

    ok <- FALSE
    if (has_stdout) {
      newout <- tryCatch({
        ret <- proc$read_output(2000)
        ok <- TRUE
        ret
      }, error = function(e) NULL)

      if (length(newout) && nzchar(newout)) {
        if (!is.null(stdout_callback)) stdout_callback(newout, proc)
        resenv$outbuf$push(newout)
        if (!is.null(stdout_line_callback)) {
          newout <- paste0(pushback_out, newout)
          pushback_out <<- ""
          lines <- strsplit(newout, "\r?\n")[[1]]
          if (last_char(newout) != "\n") {
            pushback_out <<- utils::tail(lines, 1)
            lines <- utils::head(lines, -1)
          }
          lapply(lines, function(x) stdout_line_callback(x, proc))
        }
      }
    }

    if (has_stderr) {
      newerr <- tryCatch({
        ret <- proc$read_error(2000)
        ok <- TRUE
        ret
      }, error = function(e) NULL)

      if (length(newerr) && nzchar(newerr)) {
        resenv$errbuf$push(newerr)
        if (!is.null(stderr_callback)) stderr_callback(newerr, proc)
        if (!is.null(stderr_line_callback)) {
          newerr <- paste0(pushback_err, newerr)
          pushback_err <<- ""
          lines <- strsplit(newerr, "\r?\n")[[1]]
          if (last_char(newerr) != "\n") {
            pushback_err <<- utils::tail(lines, 1)
            lines <- utils::head(lines, -1)
          }
          lapply(lines, function(x) stderr_line_callback(x, proc))
        }
      }
    }

    ok
  }

  spin <- (function() {
    state <- 1L
    phases <- c("-", "\\", "|", "/")
    function() {
      cat("\r", phases[state], "\r", sep = "")
      state <<- state %% length(phases) + 1L
      utils::flush.console()
    }
  })()

  timeout_happened <- FALSE

  while (proc$is_alive()) {
    ## Timeout? Maybe finished by now...
    if (!is.null(timeout) && is.finite(timeout) &&
        Sys.time() - start_time > timeout) {
      if (proc$kill(close_connections = FALSE)) timeout_happened <- TRUE
      "!DEBUG Timeout killed run() process `proc$get_pid()`"
      break
    }

    ## Otherwise just poll for 200ms, or less if a timeout is sooner.
    ## We cannot poll until the end, even if there is not spinner,
    ## because RStudio does not send a SIGINT to the R process,
    ## so interruption does not work.
    if (!is.null(timeout) && timeout < Inf) {
      remains <- timeout - (Sys.time() - start_time)
      remains <- max(0, as.integer(as.numeric(remains) * 1000))
      if (spinner) remains <- min(remains, 200)
    } else {
      remains <- 200
    }
    "!DEBUG run is polling for `remains` ms, process `proc$get_pid()`"
    polled <- proc$poll_io(remains)

    ## If output/error, then collect it
    if (any(polled == "ready")) do_output()

    if (spinner) spin()
  }

  ## Needed to get the exit status
  "!DEBUG run() waiting to get exit status, process `proc$get_pid()`"
  proc$wait()

  ## We might still have output
  "!DEBUG run() reading leftover output / error, process `proc$get_pid()`"
  while ((has_stdout && proc$is_incomplete_output()) ||
         (proc$has_error_connection() && proc$is_incomplete_error())) {
    proc$poll_io(-1)
    if (!do_output()) break
  }

  if (spinner) cat("\r \r")

  list(
    status = proc$get_exit_status(),
    stdout = if (has_stdout) resenv$outbuf$read(),
    stderr = if (has_stderr) resenv$errbuf$read(),
    timeout = timeout_happened
  )
}

new_process_error <- function(result, call, echo, stderr_to_stdout,
                              status = NA_integer_, command, args) {
  if (isTRUE(result$timeout)) {
    new_process_timeout_error(result, call, echo, stderr_to_stdout, status,
                              command, args)
  } else {
    new_process_status_error(result, call, echo, stderr_to_stdout, status,
                             command, args)
  }
}

new_process_status_error <- function(result, call, echo, stderr_to_stdout,
                                     status = NA_integer_, command, args) {
  err <- new_error(
    "System command '", basename(command), "' failed",
    call. = call
  )
  err$stderr <- if (stderr_to_stdout) result$stdout else result$stderr
  err$echo <- echo
  err$stderr_to_stdout <- stderr_to_stdout
  err$status <- status

  add_class(err, c("system_command_status_error", "system_command_error"))
}

new_process_interrupt_cond <- function(result, call, echo, stderr_to_stdout,
                                      status = NA_integer_) {
  cond <- new_cond(
    "System command '", basename(result$command), "' interrupted",
    call. = call
  )
  cond$stderr <- if (stderr_to_stdout) result$stdout else result$stderr
  cond$echo <- echo
  cond$stderr_to_stdout <- stderr_to_stdout
  cond$status <- status

  add_class(cond, c("system_command_interrupt", "interrupt"))
}

new_process_timeout_error <- function(result, call, echo, stderr_to_stdout,
                                      status = NA_integer_, command, args) {
  err <- new_error(
    "System command '", basename(command), "' timed out", call. = call)
  err$stderr <- if (stderr_to_stdout) result$stdout else result$stderr
  err$echo <- echo
  err$stderr_to_stdout <- stderr_to_stdout
  err$status <- status

  add_class(err, c("system_command_timeout_error", "system_command_error"))
}

#' @export

format.system_command_error <- function(x, trace = TRUE, class = TRUE,
                                        advice = !trace, ...) {
  class(x) <- setdiff(class(x), "system_command_error")

  lines <- NextMethod(
    object = x,
    trace = FALSE,
    class = class,
    advice = FALSE,
    ...
  )

  c(
    lines,
    system_error_parts(x),
    if (advice) c("---", err$format$advice()),
    if (trace && !is.null(x$trace)) {
      c("---", "Backtrace:", err$format$trace(x$trace))
    }
  )
}

#' @export

print.system_command_error <- function(x, ...) {
  writeLines(format(x, ...))
}

system_error_parts <- function(x) {
  c(
    "---",
    paste0("Exit status: ", x$status),
    if (x$echo) {
      "stdout & stderr: <printed>"
    } else {
      std <- if (x$stderr_to_stdout) "Stdout & stderr" else "Stderr"
      last_stderr_lines(x$stderr, std)
    }
  )
}

last_stderr_lines <- function(text, std, prefix = "") {
  if (!nzchar(text)) return(paste0(std, ": <empty>"))
  lines <- strsplit(text, "\r?\n")[[1]]

  if (is_interactive() && length(lines) > 10) {
    std <- paste0(std, " (last 10 lines, see `$stderr` for more)")
    lines <- utils::tail(lines, 10)
  }

  c(
    paste0(std, ":"),
    paste0(prefix, lines)
  )
}
