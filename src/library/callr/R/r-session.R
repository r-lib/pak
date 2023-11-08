
#' External R Session
#'
#' @description
#' A permanent R session that runs in the background. This is an R6 class
#' that extends the [processx::process] class.
#'
#' The process is started at the creation of the object, and then it can
#' be used to evaluate R function calls, one at a time.
#'
#' @param func Function object to call in the background R process.
#'   Please read the notes for the similar argument of [r()].
#' @param args Arguments to pass to the function. Must be a list.
#' @param package Whether to keep the environment of `func` when passing
#'   it to the other package. Possible values are:
#'   * `FALSE`: reset the environment to `.GlobalEnv`. This is the default.
#'   * `TRUE`: keep the environment as is.
#'   * `pkg`: set the environment to the `pkg` package namespace.
#'
#' @examplesIf FALSE
#' rs <- r_ression$new()
#'
#' rs$run(function() 1 + 2)
#'
#' rs$call(function() Sys.sleep(1))
#' rs$get_state()
#'
#' rs$poll_process(-1)
#' rs$get_state()
#' rs$read()
#' @export

r_session <- R6::R6Class(
  "r_session",
  inherit = processx::process,

  public = list(

    #' @field status
    #' Status codes returned by `read()`.
    status = list(
      DONE        = 200L,
      STARTED     = 201L,
      ATTACH_DONE = 202L,
      MSG         = 301L,
      EXITED      = 500L,
      CRASHED     = 501L,
      CLOSED      = 502L
    ),

    #' @description
    #' creates a new R background process. It can wait for the process to
    #' start up (`wait = TRUE`), or return immediately, i.e. before
    #' the process is actually ready to run. In the latter case you may call
    #' the `poll_process()` method to make sure it is ready.
    #'
    #' @param options A list of options created via [r_session_options()].
    #' @param wait Whether to wait for the R process to start and be ready
    #'   for running commands.
    #' @param wait_timeout Timeout for waiting for the R process to start,
    #'   in milliseconds.
    #' @return An `r_session` object.
    initialize = function(options = r_session_options(), wait = TRUE,
                          wait_timeout = 3000)
      rs_init(self, private, super, options, wait, wait_timeout),

    #' @description
    #' Similar to [r()], but runs the function in a permanent background
    #' R session. It throws an error if the function call generated an
    #' error in the child process.
    #' @return The return value of the R expression.
    run = function(func, args = list(), package = FALSE)
      rs_run(self, private, func, args, package),

    #' @description
    #' Similar to `$run()`, but returns the standard output and error of
    #' the child process as well. It does not throw on errors, but
    #' returns a non-`NULL` `error` member in the result list.
    #'
    #' @return A list with the following entries.
    #' * `result`: The value returned by `func`. On error this is `NULL`.
    #' * `stdout`: The standard output of the process while evaluating
    #    the `func` call,
    #' * `stderr`: The standard error of the process while evaluating
    #'   the `func` call.
    #' * `error`: On error it contains an error object, that contains the
    #'   error thrown in the subprocess. Otherwise it is `NULL`.
    #' * `code`, `message`: These fields are used by call internally and
    #'   you can ignore them.
    run_with_output = function(func, args = list(), package = FALSE)
      rs_run_with_output(self, private, func, args, package),

    #' @description
    #' Starts running a function in the background R session, and
    #' returns immediately. To check if the function is done, call the
    #' `poll_process()` method.
    call = function(func, args = list(), package = FALSE)
      rs_call(self, private, func, args, package),

    #' @description
    #' Poll the R session with a timeout. If the session has finished the
    #' computation, it returns with `"ready"`. If the timeout
    #' is reached, it returns with `"timeout"`.
    #'
    #' @param timeout Timeout period in milliseconds.
    #' @return Character string `"ready"` or `"timeout"`.
    poll_process = function(timeout)
      rs_poll_process(self, private, timeout),

    #' @description
    #' Return the state of the R session.
    #'
    #' @return Possible values:
    #'   * `"starting"`: starting up,
    #'   * `"idle"`: ready to compute,
    #'   * `"busy"`: computing right now,
    #'   * `"finished"`: the R process has finished.
    get_state = function()
      rs_get_state(self, private),

    #' @description
    #' Returns the elapsed time since the R process has started, and the
    #' elapsed time since the current computation has started. The latter
    #' is `NA` if there is no active computation.
    #' @return Named vector of `POSIXct` objects. The names are `"total"`
    #'   and `"current"`.
    get_running_time = function()
      rs_get_running_time(self, private),

    #' @description
    #' Reads an event from the child process, if there is one available.
    #' Events might signal that the function call has finished, or they
    #' can be progress report events.
    #'
    #' This is a low level function that you only need to use if you
    #' want to process events (messages) from the R session manually.
    #'
    #' @return `NULL` if no events are available. Otherwise a named list,
    #'   which is also a `callr_session_result` object. The list always has
    #'   a `code` entry which is the type of the event. See also
    #'   `r_session$public_fields$status` for symbolic names of the
    #'   event types.
    #'   * `200`: (`DONE`) The computation is done, and the event includes
    #'      the result, in the same form as for the `run()` method.
    #'   * `201`: (`STARTED`) An R session that was in 'starting' state is
    #'      ready to go.
    #'   * `202`: (`ATTACH_DONE`) Used by the `attach()` method.
    #'   * `301`: (`MSG`) A message from the subprocess. The message is a
    #'      condition object with class `callr_message`. (It typically has
    #'      other classes, e.g. `cli_message` for output from the cli
    #'      package.)
    #'   * `500`: (`EXITED`) The R session finished cleanly. This means
    #'      that the evaluated expression quit R.
    #'   * `501`: (`CRASHED`) The R session crashed or was killed.
    #'   * `502`: (`CLOSED`) The R session closed its end of the connection
    #'      that callr uses for communication.

    read = function()
      rs_read(self, private),

    #' @description
    #' Terminate the current computation and the R process.
    #' The session object will be in `"finished"` state after this.
    #' @param grace Grace period in milliseconds, to wait for the
    #' subprocess to exit cleanly, after its standard input is closed.
    #' If the process is still running after this period, it will be
    #' killed.
    close = function(grace = 1000)
      rs_close(self, private, grace),

    #' @description
    #' The `traceback()` method can be used after an error in the R
    #' subprocess. It is equivalent to the [base::traceback()] call, in
    #' the subprocess.
    #'
    #' On callr version 3.8.0 and above, you need to set the
    #' `callr.traceback` option to `TRUE` (in the main process) to make
    #' the subprocess save the trace on error. This is because saving
    #' the trace can be costly for large objects passed as arguments.
    #' @return The same output as from [base::traceback()]
    traceback = function()
      rs_traceback(self, private),

    #' @description
    #' Interactive debugger to inspect the dumped frames in the subprocess,
    #' after an error. See more at [r_session_debug].
    #'
    #' On callr version 3.8.0 and above, you need to set the
    #' `callr.traceback` option to `TRUE` (in the main process) to make
    #' the subprocess dump frames on error. This is because saving
    #' the frames can be costly for large objects passed as arguments.
    debug = function()
      rs_debug(self, private),

    #' @description Experimental function that provides a REPL
    #' (Read-Eval-Print-Loop) to the subprocess.
    attach = function()
      rs_attach(self, private),

    #' @description
    #' Finalizer that is called when garbage collecting an `r_session`
    #' object, to clean up temporary files.
    finalize = function() {
      unlink(private$tmp_output_file)
      unlink(private$tmp_error_file)
      unlink(private$options$tmp_files, recursive = TRUE)
      if ("finalize" %in% ls(super)) super$finalize()
    },

    #' @description
    #' Print method for an `r_session`.
    #' @param ... Arguments are not used currently.
    print = function(...) {
      cat(
        sep = "",
        "R SESSION, ",
        if (self$is_alive()) {
          paste0("alive, ", self$get_state(), ", ")
        } else {
          "finished, "
        },
        "pid ", self$get_pid(), ".\n")
      invisible(self)
    }
  ),

  private = list(
    options = NULL,
    state = NULL,
    started_at = NULL,
    fun_started_at = as.POSIXct(NA),
    pipe = NULL,

    tmp_output_file = NULL,
    tmp_error_file = NULL,

    func_file = NULL,
    res_file = NULL,

    buffer = NULL,
    read_buffer = function()
      rs__read_buffer(self, private),
    read_message = function()
      rs__read_message(self, private),

    get_result_and_output = function(std = FALSE)
      rs__get_result_and_output(self, private, std),
    report_back = function(code, text = "")
      rs__report_back(self, private, code, text),
    write_for_sure = function(text)
      rs__write_for_sure(self, private, text),
    parse_msg = function(msg)
      rs__parse_msg(self, private, msg),
    attach_wait = function()
      rs__attach_wait(self, private)
  )
)

rs_init <- function(self, private, super, options, wait, wait_timeout) {

  options$func <- options$func %||% function() { }
  options$args <- list()
  options$load_hook <- session_load_hook(options$load_hook)

  options <- convert_and_check_my_args(options)
  options <- setup_context(options)
  options <- setup_r_binary_and_args(options, script_file = FALSE)

  private$options <- options

  prepare_client_files()
  with_envvar(
    options$env,
    do.call(super$initialize, c(list(options$bin, options$real_cmdargs,
      stdin = "|", stdout = "|", stderr = "|", poll_connection = TRUE),
      options$extra))
  )

  ## Make child report back when ready
  private$report_back(201, "ready to go")

  private$pipe <- self$get_poll_connection()

  private$started_at <- Sys.time()
  private$state <- "starting"

  if (wait) {
    timeout <- wait_timeout
    have_until <- Sys.time() + as.difftime(timeout / 1000, units = "secs")
    pr <- self$poll_io(timeout)
    out <- ""
    err <- ""
    while (any(pr == "ready")) {
      if (pr["output"] == "ready") out <- paste0(out, self$read_output())
      if (pr["error"] == "ready") err <- paste0(err, self$read_error())
      if (pr["process"] == "ready") break
      timeout <- as.double(have_until - Sys.time(), units = "secs") * 1000
      pr <- self$poll_io(as.integer(timeout))
    }

    if (pr["process"] == "ready") {
      msg <- self$read()
      out <- paste0(out, msg$stdout)
      err <- paste0(err, msg$stderr)
      if (msg$code != 201) {
        data <- list(
          status = self$get_exit_status(),
          stdout = out,
          stderr = err,
          timeout = FALSE
        )
        throw(new_callr_crash_error(data, "Failed to start R session"))
      }
    } else if (pr["process"] != "ready") {
      cat("stdout:]\n", out, "\n")
      cat("stderr:]\n", err, "\n")
      throw(new_error("Could not start R session, timed out"))
    }
  }

  invisible(self)
}

rs_read <- function(self, private) {
  if (!is.null(private$buffer)) {
    # There is a partial message in the buffer, try to finish it.
    out <- private$read_buffer()
  } else {
    # A new message.
    out <- private$read_message()
  }
  if (!length(out)) {
    if (processx::processx_conn_is_incomplete(private$pipe)) return()
    if (self$is_alive()) {
      # We do this in on.exit(), because parse_msg still reads the streams
      on.exit(self$kill(), add = TRUE)
      out <- list(header = list(
        code = 502, length = 0,
        rest = "R session closed the process connection, killed"
      ))
    } else if (identical(es <- self$get_exit_status(), 0L)) {
      out <- list(header = list(
        code = 500, length = 0,
        rest = "R session finished cleanly"
      ))
    } else {
      out <- list(header = list(
        code = 501, length = 0,
        rest = paste0("R session crashed with exit code ", es)
      ))
    }
  }
  if (length(out)) private$parse_msg(out)
}

rs__read_buffer <- function(self, private) {
  # There is a partial message in the buffer already, we need to
  # read some more
  need <- private$buffer$header$length - private$buffer$got
  chunk <- processx::processx_conn_read_chars(private$pipe, need)
  got <- nchar(chunk)
  if (got == 0) {
    # make this special case fast
    NULL
  } else if (got == need) {
    msg <- list(
      header = private$buffer$header,
      body = paste(c(private$buffer$chunks, list(chunk)), collapse = "")
    )
    private$buffer <- NULL
    msg
  } else {
    private$buffer$got <- private$buffer$got + got
    private$buffer$chunks <- c(private$buffer$chunks, list(chunk))
    NULL
  }
}

rs__read_message <- function(self, private) {
  # A new message, we can surely read the first line
  out <- processx::processx_conn_read_lines(private$pipe, 1)
  if (length(out) == 0) return(NULL)

  header <- rs__parse_header(out)
  body <- ""
  if (header$length > 0) {
    body <- processx::processx_conn_read_chars(
      private$pipe,
      header$length
    )
  }
  got <- nchar(body)
  if (got < header$length) {
    # Partial message
    private$buffer <- list(
      header = header,
      got = got,
      chunks = list(body)
    )
    NULL
  } else {
    list(header = header, body = body)
  }
}

rs__parse_header <- function(line) {
  parts <- strsplit(line, " ", fixed = TRUE)[[1]]
  parts2 <- suppressWarnings(as.integer(parts[1:2]))
  rest <- paste(parts[-(1:2)], collapse = " ")
  header <- list(code = parts2[1], length = parts2[2], rest = rest)
  if (is.na(header$code) || is.na(header$length)) {
    stop("Internal callr error, invalid message header")
  }
  header
}

rs_close <- function(self, private, grace) {
  processx::processx_conn_close(self$get_input_connection())
  self$poll_process(grace)
  self$kill()
  self$wait(1000)
  if (self$is_alive()) throw(new_error("Could not kill background R session"))
  private$state <- "finished"
  private$fun_started_at <- as.POSIXct(NA)
  processx::processx_conn_close(private$pipe)
  processx::processx_conn_close(self$get_output_connection())
  processx::processx_conn_close(self$get_error_connection())
  invisible()
}

rs_call <- function(self, private, func, args, package) {

  ## We only allow a new command if the R session is idle.
  ## This allows keeping a clean state
  ## TODO: do we need a state at all?
  if (private$state == "starting") throw(new_error("R session not ready yet"))
  if (private$state == "finished") throw(new_error("R session finished"))
  if (private$state == "busy") throw(new_error("R session busy"))

  ## Save the function in a file
  private$options$func <- func
  private$options$args <- args
  private$options$package <- package
  private$options$func_file <- save_function_to_temp(private$options)
  private$options$result_file <- tempfile("callr-rs-result-")
  private$options$tmp_files <-
    c(private$options$tmp_files, private$options$func_file,
      private$options$result_file)

  ## Maybe we need to redirect stdout / stderr
  re_stdout <- if (is.null(private$options$stdout)) {
    private$tmp_output_file <- tempfile("callr-rs-stdout-")
  }
  re_stderr <- if (is.null(private$options$stderr)) {
    private$tmp_error_file <- tempfile("callr-rs-stderr-")
  }

  pre <- rs__prehook(re_stdout, re_stderr)
  post <- rs__posthook(re_stdout, re_stderr)

  ## Run an expr that loads it, in the child process, with error handlers
  expr <- make_vanilla_script_expr(private$options$func_file,
                                   private$options$result_file,
                                   private$options$error,
                                   pre_hook = pre, post_hook = post,
                                   messages = TRUE,
                                   print_error = FALSE)
  cmd <- paste0(deparse(expr), "\n")

  ## Write this to stdin
  private$write_for_sure(cmd)
  private$fun_started_at <- Sys.time()

  ## Report back when done
  report_str <- paste0("done ", basename(private$options$result_file))
  private$report_back(200, report_str)

  private$state <- "busy"
}

rs_run_with_output <- function(self, private, func, args, package) {
  self$call(func, args, package)

  go <- TRUE
  res <- NULL

  while (go) {
    ## TODO: why is this in a tryCatch?
    res <- tryCatch(
      { processx::poll(list(private$pipe), -1)
        msg <- self$read()
        if (is.null(msg)) next
        if (msg$code == 200 || (msg$code >= 500 && msg$code < 600)) {
          return(msg)
        }
        if (msg$code == 301) {
          rs__handle_condition(msg$message)
        }
      },
      interrupt = function(e) {
        self$interrupt()
        ## The R process will catch the interrupt, and then save the
        ## error object to a file, but this might still take some time,
        ## so we need to poll here. If the bg process ignores
        ## interrupts, then we kill it.
        ps <- processx::poll(list(private$pipe), 1000)[[1]]
        if (ps == "timeout") {
          self$kill()
        } else {
          res <<- self$read()
          go <<- FALSE
        }
        iconn <- structure(
          list(message = "Interrupted"),
          class = c("interrupt", "condition"))
        signalCondition(iconn)
        cat("\n")
        invokeRestart("abort")
    })
  }
  res
}

rs_run <- function(self, private, func, args, package) {
  res <- rs_run_with_output(self, private, func, args, package)
  if (is.null(res$error)) {
    res$result
  } else{
    res$stdout <- paste0(res$stdout, self$read_output())
    res$stderr <- paste0(res$stderr, self$read_error())
    throw(res$error)
  }
}

rs_get_state <- function(self, private) {
  private$state
}

rs_get_running_time <- function(self, private) {
  now <- Sys.time()
  finished <- private$state == "finished"
  c(total = if (finished) now - private$started_at else as.POSIXct(NA),
    current = now - private$fun_started_at)
}

rs_poll_process <- function(self, private, timeout) {
  processx::poll(list(self$get_poll_connection()), timeout)[[1]]
}

rs_traceback <- function(self, private) {
  tb <- self$run(function() {
    traceback(as.environment("tools:callr")$`__callr_data__`$.Traceback, 10)
  })
  if (is.null(tb)) {
    throw(new_error("No traceback was recorded in the subprocess (yet?)"))
  } else {
    traceback(utils::head(tb, -12))
  }
}

rs_debug <- function(self, private) {
  hasdump <- self$run(function() {
    ! is.null(as.environment("tools:callr")$`__callr_data__`$.Last.dump)
  })
  if (!hasdump) stop("Can't find dumped frames, nothing to debug")

  help <- function() {
    cat("Debugging in process ", self$get_pid(),
        ", press CTRL+C (ESC) or type .q to quit. Commands:\n", sep = "")
    cat("  .where       -- print stack trace\n",
        "  .inspect <n> -- inspect a frame, 0 resets to .GlobalEnv\n",
        "  .help        -- print this message\n",
        "  .q           -- quit debugger\n",
        "  <cmd>        -- run <cmd> in frame or .GlobalEnv\n\n", sep = "")
  }

  should_quit <- FALSE
  translate_cmd <- function(cmd) {
    if (cmd == ".where") {
      traceback(tb)
      if (frame) cat("Inspecting frame", frame, "\n")
      NULL

    } else if (cmd == ".help") {
      help()
      NULL

    } else if (grepl("^.inspect ", cmd)) {
      newframe <- as.integer(strsplit(cmd, " ")[[1]][[2]])
      if (is.na(newframe)) {
        message("Cannot parse frame number")
      } else {
        frame <<- newframe
      }
      NULL

    } else if (cmd == ".q") {
      should_quit <<- TRUE
      NULL

    } else {
      cmd
    }
  }

  help()
  tb <- self$traceback()
  frame <- 0L

  while (TRUE) {
    cat("\n")
    prompt <- paste0(
      "RS ", self$get_pid(),
      if (frame) paste0(" (frame ", frame, ")"), " > ")
    cmd <- rs__attach_get_input(prompt)
    cmd2 <- translate_cmd(cmd)
    if (should_quit) break
    if (is.null(cmd2)) next

    try(update_history(cmd), silent = TRUE)

    ret <- self$run_with_output(function(cmd, frame) {
      dump <- as.environment("tools:callr")$`__callr_data__`$.Last.dump
      envir <- if (!frame) .GlobalEnv else dump[[frame + 12L]]
      eval(parse(text = cmd), envir = envir)
    }, list(cmd = cmd, frame = frame))
    cat(ret$stdout)
    cat(ret$stderr)
    if (!is.null(ret$error)) {
      print(ret$error)
    } else {
      print(ret$result)
    }
  }
  invisible()
}

rs_attach <- function(self, private) {
  out <- self$get_output_connection()
  err <- self$get_error_connection()
  while (nchar(x <- processx::processx_conn_read_chars(out))) cat(x)
  while (nchar(x <- processx::processx_conn_read_chars(err))) cat(bold(x))
  tryCatch({
    while (TRUE) {
      cmd <- rs__attach_get_input(paste0("RS ", self$get_pid(), " > "))
      if (cmd == ".q") break
      try(update_history(cmd), silent = TRUE)
      private$write_for_sure(paste0(cmd, "\n"))
      private$report_back(202, "done")
      private$attach_wait()
    } },
    interrupt = function(e) { self$interrupt(); invisible() }
  )
}

## Internal functions ----------------------------------------------------

rs__attach_get_input <- function(prompt) {
  cmd <- readline(prompt = prompt)
  while (! is_complete_expression(cmd)) {
    cmd <- paste0(cmd, sep = "\n", readline(prompt = "+ "))
  }
  cmd
}

rs__attach_wait <- function(self, private) {
  out <- self$get_output_connection()
  err <- self$get_error_connection()
  pro <- private$pipe
  while (TRUE) {
    pr <- processx::poll(list(out, err, pro), -1)
    if (pr[[1]] == "ready") {
      if (nchar(x <- processx::processx_conn_read_chars(out))) cat(x)
    }
    if (pr[[2]] == "ready") {
      if (nchar(x <- processx::processx_conn_read_chars(err))) cat(bold(x))
    }
    if (pr[[3]] == "ready") {
      msg <- self$read()
      if (msg$code == 202) break;
    }
  }
}

rs__report_back <- function(self, private, code, text) {
  cmd <- paste0(
    deparse(rs__status_expr(code, text, fd = 3)),
    "\n"
  )
  private$write_for_sure(cmd)
}

rs__write_for_sure <- function(self, private, text) {
  while (1) {
    text <- self$write_input(text)
    if (!length(text)) break;
    Sys.sleep(.1)
  }
}

rs__parse_msg <- function(self, private, msg) {
  code <- as.character(msg$header$code)
  message <- msg$body
  if (length(message) && substr(message, 1, 8) == "base64::") {
    message <- substr(message, 9, nchar(message))
    message <- unserialize(processx::base64_decode(message))
  } else {
    message <- msg$header$rest
  }

  if (! code %in% names(rs__parse_msg_funcs)) {
    throw(new_error("Unknown message code: `", code, "`"))
  }
  structure(
    rs__parse_msg_funcs[[code]](self, private, msg$header$code, message),
    class = "callr_session_result")
}

rs__parse_msg_funcs <- list()
rs__parse_msg_funcs[["200"]] <- function(self, private, code, message) {
  if (private$state != "busy") {
    throw(new_error("Got `done` message when session is not busy"))
  }
  private$state <- "idle"

  res <- private$get_result_and_output()
  c(list(code = code, message = message), res)
}

rs__parse_msg_funcs[["201"]] <- function(self, private, code, message) {
  if (private$state != "starting") {
    throw(new_error("Session already started, invalid `starting` message"))
  }
  private$state <- "idle"
  list(code = code, message = message)
}

rs__parse_msg_funcs[["202"]] <- function(self, private, code, message) {
  private$state <- "idle"
  list(code = code, message = message)
}

rs__parse_msg_funcs[["301"]] <- function(self, private, code, message) {
  ## TODO: progress bar update, what to do here?
  list(code = code, message = message)
}

rs__parse_msg_funcs[["500"]] <- function(self, private, code, message) {
  private$state <- "finished"
  res <- private$get_result_and_output(std = TRUE)
  c(list(code = code, message = message), res)
}

rs__parse_msg_funcs[["501"]] <- function(self, private, code, message) {
  private$state <- "finished"
  err <- structure(
    list(message = message),
    class = c("error", "condition"))
  res <- private$get_result_and_output(std = TRUE)
  res$error <- err
  c(list(code = code, message = message), res)
}

rs__parse_msg_funcs[["502"]] <- rs__parse_msg_funcs[["501"]]

rs__status_expr <- function(code, text = "", fd = 3L) {
  substitute(
    local({
      pxlib <- as.environment("tools:callr")$`__callr_data__`$pxlib
      code_ <- code; fd_ <- fd; text_ <- text
      data <- paste0(code_, " 0 ", text_, "\n")
      pxlib$write_fd(as.integer(fd), data)
    }),
    list(code = code, fd = fd, text = text)
  )
}

rs__prehook <- function(stdout, stderr) {
  oexpr <- if (!is.null(stdout)) substitute({
    assign(
      ".__stdout__",
      as.environment("tools:callr")$`__callr_data__`$pxlib$
                                   set_stdout_file(`__fn__`),
      envir = as.environment("tools:callr")$`__callr_data__`)
  }, list(`__fn__` = stdout))
  eexpr <- if (!is.null(stderr)) substitute({
    assign(
      ".__stderr__",
      as.environment("tools:callr")$`__callr_data__`$pxlib$
                                   set_stderr_file(`__fn__`),
      envir = as.environment("tools:callr")$`__callr_data__`)
  }, list(`__fn__` = stderr))

  substitute({ o; e }, list(o = oexpr, e = eexpr))
}

rs__posthook <- function(stdout, stderr) {
  oexpr <- if (!is.null(stdout)) substitute({
    as.environment("tools:callr")$`__callr_data__`$
      pxlib$set_stdout(as.environment("tools:callr")$`__callr_data__`$
      .__stdout__)
  })
  eexpr <- if (!is.null(stderr)) substitute({
    as.environment("tools:callr")$`__callr_data__`$
      pxlib$set_stderr(as.environment("tools:callr")$`__callr_data__`$
      .__stderr__)
  })

  substitute({ o; e }, list(o = oexpr, e = eexpr))
}

rs__get_result_and_output <- function(self, private, std) {

  ## Get stdout and stderr
  stdout <- if (!is.null(private$tmp_output_file) &&
             file.exists(private$tmp_output_file)) {
    tryCatch(suppressWarnings(read_all(private$tmp_output_file)),
             error = function(e) "")
  } else if (std && self$has_output_connection()) {
    tryCatch(self$read_all_output(), error = function(err) NULL)
  }
  stderr <- if (!is.null(private$tmp_error_file) &&
             file.exists(private$tmp_error_file)) {
    tryCatch(suppressWarnings(read_all(private$tmp_error_file)),
             error = function(e) "")
  } else if (std && self$has_error_connection()) {
    tryCatch(self$read_all_error(), error = function(err) NULL)
  }
  unlink(c(private$tmp_output_file, private$tmp_error_file))
  private$tmp_output_file <- private$tmp_error_file <- NULL

  ## Get result or error from RDS
  outp <- list(
    status = 0,
    stdout = stdout %||% "",
    stderr = stderr %||% "",
    timeout = FALSE
  )
  res <- err <- NULL
  tryCatch(
    res <- get_result(outp, private$options),
    error = function(e) err <<- e,
    interrupt = function(e) err <<- e
  )
  unlink(private$options$tmp_files, recursive = TRUE)
  private$options$tmp_files <- NULL

  ## Assemble result
  list(result = res, stdout = stdout, stderr = stderr, error = err)
}

rs__handle_condition <- function(cond) {

  default_handler <- function(x) {
    classes <- class(x)
    for (cl in classes) {
      opt <- paste0("callr.condition_handler_", cl)
      if (!is.null(val <- getOption(opt)) && is.function(val)) {
        val(x)
        break
      }
    }
  }

  if (is.list(cond) && is.null(cond$muffle)) {
    cond$muffle <- "callr_r_session_muffle"
  }
  withRestarts({
    signalCondition(cond)
    default_handler(cond) },
    callr_r_session_muffle = function() NULL,
    muffleMessage = function() NULL
  )

  invisible()
}

## Helper functions ------------------------------------------------------

#' Create options for an [r_session] object
#'
#' @param ... Options to override, named arguments.
#' @return Named list of options.
#'
#' The current options are:
#' * `libpath`: Library path for the subprocess. By default the same as the
#'   _current_ library path. I.e. _not_ necessarily the library path of
#'   a fresh R session.)
#' * `repos`: `repos` option for the subprocess. By default the current
#'   value of the main process.
#' * `stdout`: Standard output of the sub-process. This can be `NULL` or
#'   a pipe: `"|"`. If it is a pipe then the output of the subprocess is
#'   not included in the responses, but you need to poll and read it
#'   manually. This is for experts. Note that this option is not used
#'   for the startup phase that currently always runs with `stdout = "|"`.
#' * `stderr`: Similar to `stdout`, but for the standard error. Like
#'   `stdout`, it is not used for the startup phase, which runs with
#'   `stderr = "|"`.
#' * `error`: See 'Error handling' in [r()].
#' * `cmdargs`: See the same argument of [r()]. (Its default might be
#'   different, though.)
#' * `system_profile`: See the same argument of [r()].
#' * `user_profile`: See the same argument of [r()].
#' * `env`: See the same argument of [r()].
#' * `load_hook`: `NULL`, or code (quoted) to run in the sub-process
#'   at start up. (I.e. not for every single `run()` call.)
#' * `extra`: List of extra arguments to pass to [processx::process].
#'
#' Call `r_session_options()` to see the default values.
#' `r_session_options()` might contain undocumented entries, you cannot
#' change these.
#'
#' @export
#' @examples
#' r_session_options()

r_session_options <- function(...) {
  update_options(r_session_options_default(), ...)
}

r_session_options_default <- function() {
  list(
    func = NULL,
    args = NULL,
    libpath = .libPaths(),
    repos = default_repos(),
    stdout = NULL,
    stderr = NULL,
    error = getOption("callr.error", "error"),
    cmdargs = c(
      if (os_platform() != "windows") "--no-readline",
      "--slave",
      "--no-save",
      "--no-restore"
    ),
    system_profile = FALSE,
    user_profile = "project",
    env = c(TERM = "dumb"),
    supervise = FALSE,
    load_hook = NULL,
    extra = list(),
    arch = "same"
  )
}

#' Interactive debugging of persistent R sessions
#'
#' The `r_session$debug()` method is an interactive debugger to inspect
#' the stack of the background process after an error.
#'
#' Note that on callr version 3.8.0 and above, you need to set the
#' `callr.traceback` option to `TRUE` (in the main process) to make
#' the subprocess dump the frames on error. This is because saving
#' the frames can be costly for large objects passed as arguments.
#'
#' `$debug()` starts a REPL (Read-Eval-Print-Loop), that evaluates R
#' expressions in the subprocess. It is similar to [browser()] and
#' [debugger()] and also has some extra commands:
#'
#' * `.help` prints a short help message.
#' * `.where` prints the complete stack trace of the error. (The same as
#'   the `$traceback()` method.
#' * `.inspect <n>` switches the "focus" to frame `<n>`. Frame 0 is the
#'   global environment, so `.inspect 0` will switch back to that.
#'
#' To exit the debugger, press the usual interrupt key, i.e. `CTRL+c` or
#' `ESC` in some GUIs.
#'
#' Here is an example session that uses `$debug()` (some output is omitted
#' for brevity):
#'
#' ```
#' # ----------------------------------------------------------------------
#' > rs <- r_session$new()
#' > rs$run(function() knitr::knit("no-such-file"))
#' Error in rs_run(self, private, func, args) :
#'  callr subprocess failed: cannot open the connection
#'
#' > rs$debug()
#' Debugging in process 87361, press CTRL+C (ESC) to quit. Commands:
#'   .where       -- print stack trace
#'   .inspect <n> -- inspect a frame, 0 resets to .GlobalEnv
#'   .help        -- print this message
#'   <cmd>        -- run <cmd> in frame or .GlobalEnv
#'
#' 3: file(con, "r")
#' 2: readLines(input2, encoding = "UTF-8", warn = FALSE)
#' 1: knitr::knit("no-such-file") at #1
#'
#' RS 87361 > .inspect 1
#'
#' RS 87361 (frame 1) > ls()
#'  [1] "encoding"  "envir"     "ext"       "in.file"   "input"     "input.dir"
#'  [7] "input2"    "ocode"     "oconc"     "oenvir"    "oopts"     "optc"
#' [13] "optk"      "otangle"   "out.purl"  "output"    "quiet"     "tangle"
#' [19] "text"
#'
#' RS 87361 (frame 1) > input
#' [1] "no-such-file"
#'
#' RS 87361 (frame 1) > file.exists(input)
#' [1] FALSE
#'
#' RS 87361 (frame 1) > # <CTRL + C>
#' # ----------------------------------------------------------------------
#' ```
#'
#' @name r_session_debug
NULL
