#' Update the status bar (superseded)
#'
#' @description
#' **The `cli_status_*()` functions are superseded by
#' the [cli_progress_message()] and [cli_progress_step()] functions,
#' because they have a better default behavior.**
#'
#' The status bar is the last line of the terminal. cli apps can use this
#' to show status information, progress bars, etc. The status bar is kept
#' intact by all semantic cli output.
#'
#' @details
#' Use [cli_status_clear()] to clear the status bar.
#'
#' Often status messages are associated with processes. E.g. the app starts
#' downloading a large file, so it sets the status bar accordingly. Once the
#' download is done (or has failed), the app typically updates the status bar
#' again. cli automates much of this, via the `msg_done`, `msg_failed`, and
#' `.auto_result` arguments. See examples below.
#'
#' @param msg The text to show, a character vector. It will be
#'   collapsed into a single string, and the first line is kept and cut to
#'   [console_width()]. The message is often associated with the start of
#'   a calculation.
#' @param msg_done The message to use when the message is cleared, when
#'   the calculation finishes successfully. If `.auto_close` is `TRUE`
#'   and `.auto_result` is `"done"`, then this is printed automatically
#'   when the calling function (or `.envir`) finishes.
#' @param msg_failed The message to use when the message is cleared, when
#'   the calculation finishes unsuccessfully. If `.auto_close` is `TRUE`
#'   and `.auto_result` is `"failed"`, then this is printed automatically
#'   when the calling function (or `.envir`) finishes.
#' @param .keep What to do when this status bar is cleared. If `TRUE` then
#'   the content of this status bar is kept, as regular cli output (the
#'   screen is scrolled up if needed). If `FALSE`, then this status bar
#'   is deleted.
#' @param .auto_close Whether to clear the status bar when the calling
#'   function finishes (or `.envir` is removed from the stack, if
#'   specified).
#' @param .envir Environment to evaluate the glue expressions in. It is
#'   also used to auto-clear the status bar if `.auto_close` is `TRUE`.
#' @param .auto_result What to do when auto-closing the status bar.
#' @return The id of the new status bar container element, invisibly.
#'
#' @seealso Status bars support [inline markup][inline-markup].
#' @seealso The [cli_progress_message()] and [cli_progress_step()]
#'   functions, for a superior API.
#' @family status bar
#' @family functions supporting inline markup
#' @export

cli_status <- function(
  msg,
  msg_done = paste(msg, "... done"),
  msg_failed = paste(msg, "... failed"),
  .keep = FALSE,
  .auto_close = TRUE,
  .envir = parent.frame(),
  .auto_result = c("clear", "done", "failed", "auto")
) {
  id <- new_uuid()
  cli__message(
    "status",
    list(
      id = id,
      msg = glue_cmd(msg, .envir = .envir),
      msg_done = glue_cmd(msg_done, .envir = .envir),
      msg_failed = glue_cmd(msg_failed, .envir = .envir),
      keep = .keep,
      auto_result = match.arg(.auto_result),
      globalenv = identical(.envir, .GlobalEnv)
    ),
    .auto_close = .auto_close,
    .envir = .envir
  )

  invisible(id)
}

#' Clear the status bar (superseded)
#'
#' @description
#' **The `cli_status_*()` functions are superseded by
#' the [cli_progress_message()] and [cli_progress_step()] functions,
#' because they have a better default behavior.**
#'
#' Clear the status bar
#'
#' @param id Id of the status bar container to clear. If `id` is not the id
#'   of the current status bar (because it was overwritten by another
#'   status bar container), then the status bar is not cleared. If `NULL`
#'   (the default) then the status bar is always cleared.
#' @param result Whether to show a message for success or failure or just
#'   clear the status bar.
#' @param msg_done If not `NULL`, then the message to use for successful
#'   process termination. This overrides the message given when the status
#'   bar was created.
#' @param msg_failed If not `NULL`, then the message to use for failed
#'   process termination. This overrides the message give when the status
#'   bar was created.
#' @inheritParams cli_status
#'
#' @family status bar
#' @seealso The [cli_progress_message()] and [cli_progress_step()]
#'   functions, for a superior API.
#' @export

cli_status_clear <- function(
  id = NULL,
  result = c("clear", "done", "failed"),
  msg_done = NULL,
  msg_failed = NULL,
  .envir = parent.frame()
) {
  cli__message(
    "status_clear",
    list(
      id = id %||% NA_character_,
      result = match.arg(result[1], c("clear", "done", "failed", "auto")),
      msg_done = if (!is.null(msg_done)) glue_cmd(msg_done, .envir = .envir),
      msg_failed = if (!is.null(msg_failed))
        glue_cmd(msg_failed, .envir = .envir)
    )
  )
}

#' Update the status bar (superseded)
#'
#' @description
#' **The `cli_status_*()` functions are superseded by
#' the [cli_progress_message()] and [cli_progress_step()] functions,
#' because they have a better default behavior.**
#'
#' Update the status bar
#'
#' @param msg Text to update the status bar with. `NULL` if you don't want
#'   to change it.
#' @param msg_done Updated "done" message. `NULL` if you don't want to
#'   change it.
#' @param msg_failed Updated "failed" message. `NULL` if you don't want to
#'   change it.
#' @param id Id of the status bar to update. Defaults to the current
#'   status bar container.
#' @param .envir Environment to evaluate the glue expressions in.
#' @return Id of the status bar container.
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @seealso The [cli_progress_message()] and [cli_progress_step()]
#'   functions, for a superior API.
#' @family status bar
#' @family functions supporting inline markup
#' @export

cli_status_update <- function(
  id = NULL,
  msg = NULL,
  msg_done = NULL,
  msg_failed = NULL,
  .envir = parent.frame()
) {
  cli__message(
    "status_update",
    list(
      msg = if (!is.null(msg)) glue_cmd(msg, .envir = .envir),
      msg_done = if (!is.null(msg_done)) glue_cmd(msg_done, .envir = .envir),
      msg_failed = if (!is.null(msg_failed))
        glue_cmd(msg_failed, .envir = .envir),
      id = id %||% NA_character_
    )
  )
}

#' Indicate the start and termination of some computation in the status bar
#' (superseded)
#'
#' @description
#' **The `cli_process_*()` functions are superseded by
#' the [cli_progress_message()] and [cli_progress_step()] functions,
#' because they have a better default behavior.**
#'
#' Typically you call `cli_process_start()` to start the process, and then
#' `cli_process_done()` when it is done. If an error happens before
#' `cli_process_done()` is called, then cli automatically shows the message
#' for unsuccessful termination.
#'
#' @details
#' If you handle the errors of the process or computation, then you can do
#' the opposite: call `cli_process_start()` with `on_exit = "done"`, and
#' in the error handler call `cli_process_failed()`. cli will automatically
#' call `cli_process_done()` on successful termination, when the calling
#' function finishes.
#'
#' See examples below.
#'
#' @param msg The message to show to indicate the start of the process or
#'   computation. It will be collapsed into a single string, and the first
#'   line is kept and cut to [console_width()].
#' @param msg_done The message to use for successful termination.
#' @param msg_failed The message to use for unsuccessful termination.
#' @param on_exit Whether this process should fail or terminate
#'   successfully when the calling function (or the environment in `.envir`)
#'   exits.
#' @param msg_class The style class to add to the message. Use an empty
#'   string to suppress styling.
#' @param done_class The style class to add to the successful termination
#'   message. Use an empty string to suppress styling.a
#' @param failed_class The style class to add to the unsuccessful
#'   termination message. Use an empty string to suppress styling.a
#' @inheritParams cli_status
#' @return Id of the status bar container.
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @seealso The [cli_progress_message()] and [cli_progress_step()]
#'   functions, for a superior API.
#' @family status bar
#' @family functions supporting inline markup
#' @export
#' @examples
#'
#' ## Failure by default
#' fun <- function() {
#'   cli_process_start("Calculating")
#'   if (interactive()) Sys.sleep(1)
#'   if (runif(1) < 0.5) stop("Failed")
#'   cli_process_done()
#' }
#' tryCatch(fun(), error = function(err) err)
#'
#' ## Success by default
#' fun2 <- function() {
#'   cli_process_start("Calculating", on_exit = "done")
#'   tryCatch({
#'     if (interactive()) Sys.sleep(1)
#'     if (runif(1) < 0.5) stop("Failed")
#'   }, error = function(err) cli_process_failed())
#' }
#' fun2()

cli_process_start <- function(
  msg,
  msg_done = paste(msg, "... done"),
  msg_failed = paste(msg, "... failed"),
  on_exit = c("auto", "failed", "done"),
  msg_class = "alert-info",
  done_class = "alert-success",
  failed_class = "alert-danger",
  .auto_close = TRUE,
  .envir = parent.frame()
) {
  # Force the defaults, because we might modify msg
  msg_done
  msg_failed

  if (length(msg_class) > 0 && msg_class != "") {
    msg <- paste0("{.", msg_class, " ", msg, "}")
  }
  if (length(done_class) > 0 && done_class != "") {
    msg_done <- paste0("{.", done_class, " ", msg_done, "}")
  }
  if (length(failed_class) > 0 && failed_class != "") {
    msg_failed <- paste0("{.", failed_class, " ", msg_failed, "}")
  }

  cli_status(
    msg,
    msg_done,
    msg_failed,
    .auto_close = .auto_close,
    .envir = .envir,
    .auto_result = match.arg(on_exit)
  )
}

#' @param id Id of the status bar container to clear. If `id` is not the id
#'   of the current status bar (because it was overwritten by another
#'   status bar container), then the status bar is not cleared. If `NULL`
#'   (the default) then the status bar is always cleared.
#'
#' @rdname cli_process_start
#' @export

cli_process_done <- function(
  id = NULL,
  msg_done = NULL,
  .envir = parent.frame(),
  done_class = "alert-success"
) {
  if (!is.null(msg_done) && length(done_class) > 0 && done_class != "") {
    msg_done <- paste0("{.", done_class, " ", msg_done, "}")
  }
  cli_status_clear(id, result = "done", msg_done = msg_done, .envir = .envir)
}

#' @rdname cli_process_start
#' @export

cli_process_failed <- function(
  id = NULL,
  msg = NULL,
  msg_failed = NULL,
  .envir = parent.frame(),
  failed_class = "alert-danger"
) {
  if (!is.null(msg_failed) && length(failed_class) > 0 && failed_class != "") {
    msg_failed <- paste0("{.", failed_class, " ", msg_failed, "}")
  }
  cli_status_clear(
    id,
    result = "failed",
    msg_failed = msg_failed,
    .envir = .envir
  )
}

# -----------------------------------------------------------------------

clii_status <- function(
  app,
  id,
  msg,
  msg_done,
  msg_failed,
  keep,
  auto_result,
  globalenv
) {
  app$status_bar[[id]] <- list(
    content = "",
    msg_done = msg_done,
    msg_failed = msg_failed,
    keep = keep,
    auto_result = auto_result
  )
  if (isTRUE(getOption("cli.hide_cursor", TRUE)) && !isTRUE(globalenv)) {
    ansi_hide_cursor(app$output)
  }
  clii_status_update(app, id, msg, msg_done = NULL, msg_failed = NULL)
}

clii_status_clear <- function(app, id, result, msg_done, msg_failed) {
  ## If NA then the most recent one
  if (is.na(id)) id <- names(app$status_bar)[1]

  ## If no active status bar, then ignore
  if (is.null(id) || is.na(id)) return(invisible())
  if (!id %in% names(app$status_bar)) return(invisible())

  if (result == "auto") {
    r1 <- random_marker
    if (identical(returnValue(r1), r1)) {
      result <- "failed"
    } else {
      result <- "done"
    }
  }

  if (result == "done") {
    msg <- msg_done %||% app$status_bar[[id]]$msg_done
    clii_status_update(app, id, msg, NULL, NULL)
    app$status_bar[[id]]$keep <- TRUE
  } else if (result == "failed") {
    msg <- msg_failed %||% app$status_bar[[id]]$msg_failed
    clii_status_update(app, id, msg, NULL, NULL)
    app$status_bar[[id]]$keep <- TRUE
  }

  if (names(app$status_bar)[1] == id) {
    ## This is the active one
    if (app$status_bar[[id]]$keep) {
      ## Keep? Just emit it
      app$cat("\n")
    } else {
      ## Not keep? Remove it
      clii__clear_status_bar(app)
    }
    if (isTRUE(getOption("cli.hide_cursor", TRUE))) {
      ansi_show_cursor(app$output)
    }
  } else {
    if (app$status_bar[[id]]$keep) {
      ## Keep?
      clii__clear_status_bar(app)
      app$cat(paste0(app$status_bar[[id]]$content, "\n"))
      app$cat(paste0(app$status_bar[[1]]$content, "\r"))
    } else {
      ## Not keep? Nothing to output
    }
  }

  ## Remove
  app$status_bar[[id]] <- NULL

  ## Switch to the previous one
  if (length(app$status_bar)) {
    app$cat(paste0(app$status_bar[[1]]$content, "\r"))
  }
}

clii_status_update <- function(app, id, msg, msg_done, msg_failed) {
  ## If NA then the most recent one
  if (is.na(id)) id <- names(app$status_bar)[1]

  ## If no active status bar, then ignore
  if (is.na(id)) return(invisible())

  ## Update messages
  if (!is.null(msg_done)) app$status_bar[[id]]$msg_done <- msg_done
  if (!is.null(msg_failed)) app$status_bar[[id]]$msg_failed <- msg_failed

  ## Do we have a new message?
  if (is.null(msg)) return(invisible())

  ## Do we need to clear the current content?
  current <- paste0("", app$status_bar[[1]]$content)

  ## Format the line
  content <- ""
  fmsg <- app$inline(msg)
  cfmsg <- ansi_strtrim(fmsg, width = app$get_width())
  content <- strsplit(cfmsg, "\r?\n")[[1]][1]
  if (is.na(content)) content <- ""

  ## Update status bar, put it in front
  app$status_bar[[id]]$content <- content
  app$status_bar <- c(
    app$status_bar[id],
    app$status_bar[setdiff(names(app$status_bar), id)]
  )

  ## New content, if it is an ANSI terminal we'll overwrite and clear
  ## until the end of the line. Otherwise we add some space characters
  ## to the content to make sure we clear up residual content.
  output <- get_real_output(app$output)
  if (is_ansi_tty(output)) {
    app$cat(paste0("\r", content, ANSI_EL, "\r"))
  } else if (is_dynamic_tty(output)) {
    nsp <- max(ansi_nchar(current) - ansi_nchar(content), 0)
    app$cat(paste0("\r", content, strrep(" ", nsp), "\r"))
  } else {
    app$cat(paste0(content, "\n"))
  }

  ## Reset timer
  .Call(clic_tick_reset)

  invisible()
}

clii__clear_status_bar <- function(app) {
  output <- get_real_output(app$output)
  if (is_ansi_tty(output)) {
    app$cat(paste0("\r", ANSI_EL))
  } else if (is_dynamic_tty(output)) {
    text <- app$status_bar[[1]]$content
    len <- ansi_nchar(text, type = "width")
    app$cat(paste0("\r", strrep(" ", len + rstudio_r_fix), "\r"))
  }
}
