
#' cli progress bars
#'
#' @description
#' This is the reference manual of the three functions that create,
#' update and terminate progress bars. For a tutorial see the
#' [cli progress bars](https://cli.r-lib.org/articles/progress.html).
#'
#' `cli_progress_bar()` creates a new progress bar.
#'
#' @details
#'
#' ## Basic usage
#'
#' `cli_progress_bar()` creates a progress bar, `cli_progress_update()`
#' updates an existing progress bar, and `cli_progress_done()` terminates
#' it.
#'
#' It is good practice to always set the `name` argument, to make the
#' progress bar more informative.
#'
#' ```{asciicast progress-1}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' clean <- function() {
#'   cli_progress_bar("Cleaning data", total = 100)
#'   for (i in 1:100) {
#'     Sys.sleep(5/100)
#'     cli_progress_update()
#'   }
#'   cli_progress_done()
#' }
#' clean()
#' ```
#'
#' ## Progress bar types
#'
#' There are three builtin types of progress bars, and a custom type.
#'
#' ```{asciicast progress-tasks}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' tasks <- function() {
#'   cli_progress_bar("Tasks", total = 3, type = "tasks")
#'   for (i in 1:3) {
#'     Sys.sleep(1)
#'     cli_progress_update()
#'   }
#'   cli_progress_done()
#' }
#' tasks()
#' ```
#'
#' ## Unknown `total`
#'
#' If `total` is not known, then cli shows a different progress bar.
#' Note that you can also set `total` in `cli_progress_update()`, if it
#' not known when the progress bar is created, but you learn it later.
#'
#' ```{asciicast progress-natotal}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' nototal <- function() {
#'   cli_progress_bar("Parameter tuning")
#'   for (i in 1:100) {
#'     Sys.sleep(3/100)
#'     cli_progress_update()
#'   }
#'   cli_progress_done()
#' }
#' nototal()
#' ```
#'
#' ## Clearing the progress bar
#'
#' By default cli removes terminated progress bars from the screen, if
#' the terminal supports this. If you want to change this, use the
#' `clear` argument of `cli_progress_bar()`, or the `cli.progress_clear`
#' global option (see [cli-config]) to change this.
#'
#' (In the cli documentation we usually set `cli.progress_clear` to `FALSE`,
#' so users can see how finished progress bars look.)
#'
#' In this example the first progress bar is cleared, the second is not.
#'
#' ```{asciicast progress-clear}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' fun <- function() {
#'   cli_progress_bar("Data cleaning", total = 100, clear = TRUE)
#'   for (i in 1:100) {
#'     Sys.sleep(3/100)
#'     cli_progress_update()
#'   }
#'   cli_progress_bar("Parameter tuning", total = 100, clear = FALSE)
#'   for (i in 1:100) {
#'     Sys.sleep(3/100)
#'     cli_progress_update()
#'   }
#' }
#' fun()
#' ```
#'
#' ## Initial delay
#'
#' Updating a progress bar on the screen is costly, so cli tries to avoid
#' it for quick loops. By default a progress bar is only shown after two
#' seconds, or after half of that if less than 50% of the iterations are
#' complete. You can change the two second default with the
#' `cli.progress_show_after` global option (see [cli-config]).
#'
#' (In the cli documentation we usually set `cli.progress_show_after` to
#' `0` (zero seconds), so progress bars are shown immediately.)
#'
#' In this example we only show the progress bar after one second, because
#' more than 50% of the iterations remain after one second.
#'
#' ```{asciicast progress-after}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' fun <- function() {
#'   cli_alert("Starting now, at {Sys.time()}")
#'   cli_progress_bar(
#'     total = 100,
#'     format = "{cli::pb_bar} {pb_percent} @ {Sys.time()}"
#'   )
#'   for (i in 1:100) {
#'     Sys.sleep(4/100)
#'     cli_progress_update()
#'   }
#' }
#' options(cli.progress_show_after = 2)
#' fun()
#' ```
#'
#' ```{asciicast, include = FALSE, cache = FALSE}
#' # reset to our default
#' options(cli.progress_show_after = 0)
#' ```
#'
#' ## The _current_ progress bar
#'
#' By default cli sets the new progress bar as the _current_ progress bar
#' of the calling function. The current progress bar is the default one
#' in cli progress bar operations. E.g. if no progress bar id is supplied
#' in `cli_progress_update()`, then the current progress bar is updated.
#'
#' Every function can only have a single _current_ progress bar, and if a
#' new one is created, then the previous one (if any) is automatically
#' terminated. The current progress bar is also terminated when the function
#' that created it exits. Thanks to these rules, most often you don't need
#' to explicitly deal with progress bar ids, and you don't need to
#' explicitly call `cli_progress_done()`:
#'
#' ```{asciicast progress-current}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' fun <- function() {
#'   cli_progress_bar("First step ", total = 100)
#'   for (i in 1:100) {
#'     Sys.sleep(2/100)
#'     cli_progress_update()
#'   }
#'   cli_progress_bar("Second step", total = 100)
#'   for (i in 1:100) {
#'     Sys.sleep(2/100)
#'     cli_progress_update()
#'   }
#' }
#' fun()
#' ```
#'
#' ## cli output while the progress bar is active
#'
#' cli allows emitting regular cli output (alerts, headers, lists, etc.)
#' while a progress bar is active. On terminals that support this, cli
#' will remove the progress bar temporarily, emit the output, and then
#' restores the progress bar.
#'
#' ```{asciicast progress-output}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' fun <- function() {
#'   cli_alert_info("Before the progress bar")
#'   cli_progress_bar("Calculating", total = 100)
#'   for (i in 1:50) {
#'     Sys.sleep(4/100)
#'     cli_progress_update()
#'   }
#'   cli_alert_info("Already half way!")
#'   for (i in 1:50) {
#'     Sys.sleep(4/100)
#'     cli_progress_update()
#'   }
#'   cli_alert_info("All done")
#' }
#' fun()
#' ```
#'
#' See also [cli_progress_output()], which sends text for the current
#' progress handler. E.g. in a Shiny app it will send the output to the
#' Shiny progress bar, as opposed to the `cli_alert()` etc. cli functions
#' which will print the text to the console.
#'
#' ## Custom formats
#'
#' In addition to the builtin types, you can also specify a custom
#' format string. In this case [progress variables][progress-variables]
#' are probably useful to avoid calculating some progress bar quantities
#' like the elapsed time, of the ETA manually. You can also use your own
#' variables in the calling function:
#'
#' ```{asciicast progress-format}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' fun <- function(urls) {
#'   cli_progress_bar(
#'     format = paste0(
#'       "{pb_spin} Downloading {.path {basename(url)}} ",
#'       "[{pb_current}/{pb_total}]   ETA:{pb_eta}"
#'     ),
#'     format_done = paste0(
#'       "{col_green(symbol$tick)} Downloaded {pb_total} files ",
#'       "in {pb_elapsed}."
#'     ),,
#'     total = length(urls)
#'   )
#'   for (url in urls) {
#'     cli_progress_update()
#'     Sys.sleep(5/10)
#'   }
#' }
#' fun(paste0("https://acme.com/data-", 1:10, ".zip"))
#' ```
#'
#' @param name This is typically used as a label, and should be short,
#'   at most 20 characters.
#' @param status Initial status of the progress bar. If not empty, then
#'   it is typically shown after the label.
#' @param type Type of the progress bar. It is used to select a default
#'   display if `format` is not specified. Currently supported types:
#'   * `iterator`: e.g. a for loop or a mapping function,
#'   * `tasks`: a (typically small) number of tasks,
#'   * `download`: download of one file,
#'   * `custom`: custom type, `format` must not be `NULL` for this type.
#' @param total Total number of progress units, or `NA` if it is unknown.
#'   `cli_progress_update()` can update the total number of units. This is
#'   handy if you don't know the size of a download at the beginning, and
#'   also in some other cases. If `format` is set to `NULL`, `format` (plus
#'   `format_done` and `format_failed`) will be updated when you change
#'   `total` from `NA` to a number. I.e. default format strings will be
#'   updated, custom ones won't be.
#' @param format Format string. It has to be specified for custom progress
#'   bars, otherwise it is optional, and a default display is selected
#'   based on the progress bat type and whether the number of total units
#'   is known. Format strings may contain glue substitution, the support
#'   pluralization and cli styling. See [progress-variables] for special
#'   variables that you can use in the custom format.
#' @param format_done Format string for successful termination. By default
#'   the same as `format`.
#' @param format_failed Format string for unsuccessful termination. By
#'   default the same as `format`.
#' @param clear Whether to remove the progress bar from the screen after
#'   it has terminated. Defaults to the `cli.progress_clear` option, or
#'   `TRUE` if unset.
#' @param current Whether to use this progress bar as the current progress
#'   bar of the calling function. See more at 'The current progress bar'
#'   below.
#' @param auto_terminate Whether to terminate the progress bar if the
#'   number of current units reaches the number of total units.
#' @param extra Extra data to add to the progress bar. This can be
#'   used in custom format strings for example. It should be a named list.
#'   `cli_progress_update()` can update the extra data. Often you can get
#'   away with referring to local variables in the format string, and
#'   then you don't need to use this argument. Explicitly including these
#'   constants or variables in `extra` can result in cleaner code. In
#'   the rare cases when you need to refer to the same progress bar from
#'   multiple functions, and you can them to `extra`.
#' @param .auto_close Whether to terminate the progress bar when the
#'   calling function (or the one with execution environment in `.envir`
#'   exits. (Auto termination does not work for progress bars created
#'   from the global environment, e.g. from a script.)
#' @param .envir The environment to use for auto-termination and for glue
#'   substitution. It is also used to find and set the current progress bar.
#'
#' @return `cli_progress_bar()` returns the id of the new progress bar.
#' The id is a string constant.
#'
#' @seealso These functions support [inline markup][inline-markup].
#' @seealso [cli_progress_message()] and [cli_progress_step()] for simpler
#'   progress messages.
#' @family progress bar functions
#' @family functions supporting inline markup
#' @aliases __cli_update_due cli_tick_reset ccli_tick_reset ticking
#' @export

cli_progress_bar <- function(name = NULL,
                             status = NULL,
                             type = c("iterator", "tasks", "download",
                                      "custom"),
                             total = NA,
                             format = NULL,
                             format_done = NULL,
                             format_failed = NULL,
                             clear = getOption("cli.progress_clear", TRUE),
                             current = TRUE,
                             auto_terminate = type != "download",
                             extra = NULL,
                             .auto_close = TRUE,
                             .envir = parent.frame()) {

  start <- .Call(clic_get_time)
  id <- new_uuid()
  envkey <- format(.envir)
  type <- match.arg(type)
  if (type == "custom" && is.null(format)) {
    stop("Need to specify format if `type == \"custom\"")
  }

  ## If `total` is infinite, use behavior seen when `total` is NA
  if (is.infinite(total)) {
    total <- NA
  }

  ## If changes, synchronize with C API in progress.c
  bar <- new.env(parent = emptyenv())
  bar$id <- id
  bar$name <- name
  bar$status <- status
  bar$type <- match.arg(type)
  bar$total <- total
  bar$show_after <- start + getOption("cli.progress_show_after", 2)
  bar$show_50 <- start + getOption("cli.progress_show_after", 2) / 2
  bar$format_orig <- bar$format <- format
  bar$format_done_orig <- bar$format_done <- format_done %||% format
  bar$format_failed_orig <- bar$format_failed <- format_failed %||% format
  bar$clear <- clear
  bar$auto_terminate <- auto_terminate
  bar$envkey <- if (current) envkey else NULL
  bar$current <- 0L
  bar$start <- start
  bar$tick <- 0L
  bar$extra <- extra
  clienv$progress[[id]] <- bar
  if (current) {
    if (!is.null(clienv$progress_ids[[envkey]])) {
      cli_progress_done(clienv$progress_ids[[envkey]], .envir = .envir, result = "done")
    }
    clienv$progress_ids[[envkey]] <- id
  }

  if (.auto_close && envkey != clienv$globalenv) {
    defer(
      cli_progress_done(id = id, .envir = .envir, result = "auto"),
      envir = .envir
    )
  }

  opt <- options(cli__pb = bar)
  on.exit(options(opt), add = TRUE)

  bar$handlers <- cli_progress_select_handlers(bar, .envir)
  for (h in bar$handlers) {
    if ("create" %in% names(h)) h$create(bar, .envir = .envir)
  }

  invisible(id)
}

#' @description
#' `cli_progress_update()` updates the state of a progress bar, and
#' potentially the display as well.
#'
#' @param inc Increment in progress units. This is ignored if `set` is
#'   not `NULL`.
#' @param set Set the current number of progress units to this value.
#'   Ignored if `NULL`.
#' @param status New status string of the progress bar, if not `NULL`.
#' @param id Progress bar to update or terminate. If `NULL`, then the
#'   current progress bar of the calling function (or `.envir` if
#'   specified) is updated or terminated.
#' @param force Whether to force a display update, even if no update is
#'   due.
#'
#' @return `cli_progress_update()` returns the id of the progress bar,
#' invisibly.
#'
#' @name cli_progress_bar
#' @export

cli_progress_update <- function(inc = NULL, set = NULL, total = NULL,
                                status = NULL, extra = NULL,
                                id = NULL, force = FALSE,
                                .envir = parent.frame()) {

  id <- id %||% clienv$progress_ids[[format(.envir)]]
  if (is.null(id)) {
    envkey <- format(.envir)
    stop("Cannot find current progress bar for `", envkey, "`")
  }
  pb <- clienv$progress[[id]]
  if (is.null(pb)) stop("Cannot find progress bar `", id, "`")

  if (!is.null(status)) pb$status <- status

  if (!is.null(extra)) pb$extra <- utils::modifyList(pb$extra, extra)

  if (!is.null(set)) {
    pb$current <- set
  } else {
    inc <- inc %||% 1L
    pb$current <- pb$current + inc
  }

  if (!is.null(total)) {
    if (is.na(pb$total) != is.na(total) ||
        (!is.na(total) && pb$total != total)) {
      pb$total <- total
      if (!is.null(pb$format) && is.null(pb$format_orig)) {
        pb$format <- pb__default_format(pb$type, pb$total)
        pb$format_done <- pb$format_done_orig %||% pb$format
        pb$format_failed <- pb$format_failed_orig %||% pb$format
      }
    }
  }

  if (pb$auto_terminate && !is.na(pb$total) && pb$current == pb$total) {
    cli_progress_done(id, .envir = .envir, result = "done")
    return(invisible(id))
  }

  now <- .Call(clic_get_time)
  upd <- .Call(clic_update_due)
  if (force || (upd && now > pb$show_after) ||
      (!is.na(pb$total) && upd && now > pb$show_50 && pb$current <= pb$total / 2)) {
    if (upd) cli_tick_reset()
    pb$tick <- pb$tick + 1L

    if (is.null(pb$format)) {
      pb$format <- pb__default_format(pb$type, pb$total)
      pb$format_done <- pb$format_done_orig %||% pb$format
      pb$format_failed <- pb$format_failed_orig %||% pb$format
    }

    opt <- options(cli__pb = pb)
    on.exit(options(opt), add = TRUE)

    if (is.null(pb$added)) {
      pb$added <- TRUE
      for (h in pb$handlers) {
        if ("add" %in% names(h)) h$add(pb, .envir = .envir)
      }
    }

    for (h in pb$handlers) {
      if ("set" %in% names(h)) h$set(pb, .envir = .envir)
    }
  }

  # Return TRUE, to allow cli_progress_update() && break in loops
  invisible(id)
}

#' @description
#' `cli_progress_done()` terminates a progress bar.
#'
#' @param result String to select successful or unsuccessful termination.
#'   It is only used if the progress bar is not cleared from the screen.
#'   It can be one of `"done"`, `"failed"`, `"clear"`, and `"auto"`.
#'
#' @return `cli_progress_done()` returns `TRUE`, invisibly, always.
#'
#' @name cli_progress_bar
#' @export

cli_progress_done <- function(id = NULL, .envir = parent.frame(),
                              result = "done") {
  envkey <- format(.envir)
  id <- id %||% clienv$progress_ids[[envkey]]
  if (is.null(id)) return(invisible(TRUE))
  pb <- clienv$progress[[id]]
  if (is.null(pb)) return(invisible(TRUE))

  opt <- options(cli__pb = pb)
  on.exit(options(opt), add = TRUE)

  if (result == "auto") {
    r1 <- random_marker
    if (identical(returnValue(r1), r1)) {
      result <- "failed"
    } else {
      result <- "done"
    }
  }

  for (h in pb$handlers) {
    if ("complete" %in% names(h)) {
      h$complete(pb, .envir = .envir, result = result)
    }
  }

  clienv$progress[[id]] <- NULL
  if (!is.null(pb$envkey)) clienv$progress_ids[[pb$envkey]] <- NULL

  invisible(TRUE)
}

#' Add text output to a progress bar
#'
#' The text is calculated via [cli_text()], so all cli features can be
#' used here, including progress variables.
#'
#' The text is passed to the progress handler(s), that may or may not be
#' able to print it.
#'
#' ```{asciicast progress-output2}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' fun <- function() {
#'   cli_alert_info("Before the progress bar")
#'   cli_progress_bar("Calculating", total = 100)
#'   for (i in 1:50) {
#'     Sys.sleep(4/100)
#'     cli_progress_update()
#'   }
#'   cli_progress_output("Already half way!")
#'   for (i in 1:50) {
#'     Sys.sleep(4/100)
#'     cli_progress_update()
#'   }
#'   cli_alert_info("All done")
#' }
#' fun()
#' ```
#'
#' @param text Text to output. It is formatted via [cli_text()].
#' @param id Progress bar id. The default is the current progress bar.
#' @param .envir Environment to use for glue interpolation of `text`.
#' @return `TRUE`, always.
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @family progress bar functions
#' @family functions supporting inline markup
#' @export

cli_progress_output <- function(text, id = NULL, .envir = parent.frame()) {
  envkey <- format(.envir)
  id <- id %||% clienv$progress_ids[[envkey]]
  if (is.null(id)) {
    stop("Cannot find current progress bar for `", envkey, "`")
  }
  pb <- clienv$progress[[id]]
  if (is.null(pb)) stop("Cannot find progress bar `", id, "`")

  txt <- cli_fmt(cli_text(text, .envir = .envir))
  for (h in pb$handlers) {
    if ("output" %in% names(h)) {
      h$output(pb, .envir = .envir, text = txt)
    }
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------

#' Simplified cli progress messages
#'
#' @description This is a simplified progress bar, a single (dynamic)
#' message, without progress units.
#'
#' @details `cli_progress_message()` always shows the message, even if no
#' update is due. When the progress message is terminated, it is removed
#' from the screen by default.
#'
#' Note that the message can be dynamic: if you update it with
#' [cli_progress_update()], then cli uses the current values in the string
#' substitutions.
#'
#' ```{asciicast progress-message}
#' #| echo = c(-2, -3),
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' fun <- function() {
#'   opts <- options(cli.progress_clear = TRUE)
#'   on.exit(options(opts), add = TRUE)
#'   cli_progress_message("Task one is running...")
#'   Sys.sleep(2)
#'
#'   cli_progress_message("Task two is running...")
#'   Sys.sleep(2)
#'
#'   step <- 1L
#'   cli_progress_message("Task three is underway: step {step}")
#'   for (step in 1:5) {
#'     Sys.sleep(0.5)
#'     cli_progress_update()
#'   }
#' }
#' fun()
#' ```
#'
#' @param msg Message to show. It may contain glue substitution and cli
#'   styling. It can be updated via [cli_progress_update()], as usual.
#' @param current Passed to [cli_progress_bar()].
#' @param .auto_close Passed to [cli_progress_bar()].
#' @param .envir Passed to [cli_progress_bar()].
#' @param ... Passed to [cli_progress_bar()].
#'
#' @return The id of the new progress bar.
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @seealso [cli_progress_bar()] for the complete progress bar API.
#'   [cli_progress_step()] for a similar display that is styled by default.
#' @family progress bar functions
#' @family functions supporting inline markup
#' @export

cli_progress_message <- function(msg,
                                 current = TRUE,
                                 .auto_close = TRUE,
                                 .envir = parent.frame(),
                                 ...) {

  id <- cli_progress_bar(
    type = "custom",
    format = msg,
    current = current,
    .auto_close = .auto_close,
    .envir = .envir,
    ...
  )

  cli_progress_update(id = id, force = TRUE, .envir = .envir)

  invisible(id)
}

# ------------------------------------------------------------------------

#' Simplified cli progress messages, with styling
#'
#' @description This is a simplified progress bar, a single (dynamic)
#' message, without progress units.
#'
#' @details `cli_progress_step()` always shows the progress message,
#' even if no update is due.
#'
#' ## Basic use
#'
#' ```{asciicast progress-step}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' f <- function() {
#'   cli_progress_step("Downloading data")
#'   Sys.sleep(2)
#'   cli_progress_step("Importing data")
#'   Sys.sleep(1)
#'   cli_progress_step("Cleaning data")
#'   Sys.sleep(2)
#'   cli_progress_step("Fitting model")
#'   Sys.sleep(3)
#' }
#' f()
#' ```
#'
#' ## Spinner
#'
#' You can add a spinner to some or all steps with `spinner = TRUE`,
#' but note that this will only work if you call [cli_progress_update()]
#' regularly.
#'
#' ```{asciicast progress-step-spin}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' f <- function() {
#'   cli_progress_step("Downloading data", spinner = TRUE)
#'   for (i in 1:100) { Sys.sleep(2/100); cli_progress_update() }
#'   cli_progress_step("Importing data")
#'   Sys.sleep(1)
#'   cli_progress_step("Cleaning data")
#'   Sys.sleep(2)
#'   cli_progress_step("Fitting model", spinner = TRUE)
#'   for (i in 1:100) { Sys.sleep(3/100); cli_progress_update() }
#' }
#' f()
#' ```
#'
#' ## Dynamic messages
#'
#' You can make the step messages dynamic, using glue templates.
#' Since `cli_progress_step()` show that message immediately, we need
#' to initialize `msg` first.
#'
#' ```{asciicast progress-step-dynamic}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' f <- function() {
#'   msg <- ""
#'   cli_progress_step("Downloading data{msg}", spinner = TRUE)
#'   for (i in 1:100) {
#'     Sys.sleep(2/100)
#'     msg <- glue::glue(", got file {i}/100")
#'     cli_progress_update()
#'   }
#'   cli_progress_step("Importing data")
#'   Sys.sleep(1)
#'   cli_progress_step("Cleaning data")
#'   Sys.sleep(2)
#'   cli_progress_step("Fitting model", spinner = TRUE)
#'   for (i in 1:100) { Sys.sleep(3/100); cli_progress_update() }
#' }
#' f()
#' ```
#'
#' ## Termination messages
#'
#' You can specify a different message for successful and/or
#' unsuccessful termination:
#'
#' ```{asciicast progress-step-msg}
#' #| error = FALSE,
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' f <- function() {
#'   size <- 0L
#'   cli_progress_step(
#'     "Downloading data.",
#'     msg_done = "Downloaded {prettyunits::pretty_bytes(size)}.",
#'     spinner = TRUE
#'   )
#'   for (i in 1:100) {
#'     Sys.sleep(3/100)
#'     size <- size + 8192
#'     cli_progress_update()
#'   }
#' }
#' f()
#' ```
#'
#' @param msg Message to show. It may contain glue substitution and cli
#'   styling. It can be updated via [cli_progress_update()], as usual.
#'   It is style as a cli info alert (see [cli_alert_info()]).
#' @param msg_done Message to show on successful termination. By default
#'   this it is the same as `msg` and it is styled as a cli success alert
#'   (see [cli_alert_success()]).
#' @param msg_failed Message to show on unsuccessful termination. By
#'   default it is the same as `msg` and it is styled as a cli danger alert
#'   (see [cli_alert_danger()]).
#' @param spinner Whether to show a spinner at the beginning of the line.
#'   To make the spinner spin, you'll need to call `cli_progress_update()`
#'   regularly.
#' @param class cli class to add to the message. By default there is no
#'   class for steps with a spinner.
#' @param current Passed to [cli_progress_bar()].
#' @param .auto_close Passed to [cli_progress_bar()].
#' @param .envir Passed to [cli_progress_bar()].
#' @param ... Passed to [cli_progress_bar()].
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @family progress bar functions
#' @family functions supporting inline markup
#' @export

cli_progress_step <- function(msg,
                              msg_done = msg,
                              msg_failed = msg,
                              spinner = FALSE,
                              class = if (!spinner) ".alert-info",
                              current = TRUE,
                              .auto_close = TRUE,
                              .envir = parent.frame(),
                              ...) {

  format <- paste0(
    if (!is.null(class)) paste0("{", class, " "),
    if (spinner) "{cli::pb_spin} ",
    msg,
    if (!is.null(class)) "}"
  )
  ts <- " {.timestamp {cli::pb_elapsed}}"
  format_done <- paste0("{.alert-success ", msg_done, ts, "}")
  format_failed <- paste0("{.alert-danger ", msg_failed, ts, "}")

  opt <- options(cli.progress_show_after = 0)
  on.exit(options(opt), add = TRUE)
  id <- cli_progress_bar(
    type = "custom",
    format = format,
    format_done = format_done,
    format_failed = format_failed,
    clear = FALSE,
    current = current,
    .auto_close = .auto_close,
    .envir = .envir,
    ...
  )

  cli_progress_update(id = id, force = TRUE, .envir = .envir)

  invisible(id)
}

# ------------------------------------------------------------------------

pb__default_format <- function(type, total) {
  if (type == "iterator") {
    if (!is.na(total)) {
      opt <- getOption("cli.progress_format_iterator")
      if (!is.null(opt)) return(opt)
      paste0(
        "{cli::pb_name}{cli::pb_bar} {cli::pb_percent} | {cli::pb_status}",
        "ETA: {cli::pb_eta}"
      )
    } else {
      opt <- getOption("cli.progress_format_iterator_nototal") %||%
        getOption("cli.progress_format_iterator")
      if (!is.null(opt)) return(opt)
      paste0(
        "{cli::pb_spin} {cli::pb_name}{cli::pb_status}",
        "{cli::pb_current} done ({cli::pb_rate}) | {cli::pb_elapsed}"
      )
    }

  } else if (type == "tasks") {
    if (!is.na(total)) {
      opt <- getOption("cli.progress_format_tasks")
      if (!is.null(opt)) return(opt)
      paste0(
        "{cli::pb_spin} {cli::pb_current}/{cli::pb_total} ",
        "ETA: {cli::pb_eta} | {cli::pb_name}{cli::pb_status}"
      )
    } else {
      opt <- getOption("cli.progress_format_tasks_nototal") %||%
        getOption("cli.progress_format_tasks")
      if (!is.null(opt)) return(opt)
      paste0(
        "{cli::pb_spin} {cli::pb_name}{cli::pb_status}",
        "{cli::pb_current} done ({cli::pb_rate}) | {cli::pb_elapsed}"
      )
    }

  } else if (type == "download") {
    if (!is.na(total)) {
      opt <- getOption("cli.progress_format_download")
      if (!is.null(opt)) return(opt)
      paste0(
        "{cli::pb_name}{cli::pb_status}{cli::pb_bar}| ",
        "{cli::pb_current_bytes}/{cli::pb_total_bytes} {cli::pb_eta_str}"
      )
    } else {
      opt <- getOption("cli.progress_format_download_nototal") %||%
        getOption("cli.progress_format_download")
      if (!is.null(opt)) return(opt)
      paste0(
        "{cli::pb_name}{cli::pb_status}{cli::pb_spin} ",
        "{cli::pb_current_bytes} ({cli::pb_rate_bytes}) | {cli::pb_elapsed}"
      )
    }
  }
}
