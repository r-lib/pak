
# ------------------------------------------------------------------------

#' @title Progress bar variables
#'
#' @details These variables can be used in cli progress bar format
#' strings. They are calculated on demand. To use a variable, e.g. `pb_bar`
#' in a package, you either need to to import `pb_bar` from cli, or use
#' the qualified form in the format string: `cli::pb_bar`.
#'
#' Similarly, in R scripts, you can use `pb_bar` after `library(cli)`,
#' or `cli::pb_bar` if you do not attach the cli package.
#'
#' @family progress bar functions
#' @name progress-variables
NULL

#' @name progress-variables
#' @export pb_bar
#' @usage NULL
#' @aliases pb_bar
#'
#' @details
#' ### `pb_bar`
#'
#' Creates a visual progress bar. If the number of total units
#' is unknown, then it will return an empty string.
#'
#' ```{asciicast progress-var-bar, echo = 2:5}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 100,
#'   format = "Fitting model {cli::pb_bar} {cli::pb_percent}"
#' )
#' ))
#' cli:::var_helper(x, current = 66)
#' ```

cli__pb_bar <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  if (is.na(pb$total)) return("")
  structure(
    list(current = pb$current, total = pb$total),
    class = "cli-progress-bar"
  )
}

#' @name progress-variables
#' @export pb_current
#' @usage NULL
#' @aliases pb_current
#'
#' @details
#' ### `pb_current`
#'
#' The number of current progress units.
#'
#' ```{asciicast progress-var-current, echo = 2:5}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 100,
#'   format = "{cli::pb_spin} Reading file {cli::pb_current}/{cli::pb_total}"
#' )
#' ))
#' cli:::var_helper(x, current = 66)
#' ```

cli__pb_current <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  pb$current
}

#' @name progress-variables
#' @export pb_current_bytes
#' @usage NULL
#' @aliases pb_current_bytes
#'
#' @details
#' ### `pb_current_bytes`
#'
#' The number of current progress units formatted as bytes.
#' The output has a constant width of six characters.
#'
#' ```{asciicast progress-var-current-bytes, echo = 2:4}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   format = "Got {cli::pb_current_bytes} in {cli::pb_elapsed}"
#' )
#' ))
#' cli:::var_helper2(x, current = 1024 * 512, delay = 5)
#' ```

cli__pb_current_bytes <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  format_bytes$pretty_bytes(pb$current, style = "6")
}

#' @name progress-variables
#' @export pb_elapsed
#' @usage NULL
#' @aliases pb_elapsed
#'
#' @details
#' ### `pb_elapsed`
#'
#' The elapsed time since the start of the progress bar. The time is
#' measured since the progress bar was created with [cli_progress_bar()]
#' or similar.
#'
#' ```{asciicast progress-var-elapsed, echo = 2:5}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 100,
#'   format = "{cli::pb_bar} {cli::pb_percent} [{cli::pb_elapsed}]"
#' )
#' ))
#' cli:::var_helper2(x, current = 65, delay = 5)
#' ```

cli__pb_elapsed <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  secs <- (.Call(clic_get_time) - pb$start) * clienv$speed_time
  format_time$pretty_sec(secs)
}

#' @name progress-variables
#' @export pb_elapsed_clock
#' @usage NULL
#' @aliases pb_elapsed_clock
#'
#' @details
#' ### `pb_elapsed_clock`
#'
#' The elapsed time, in `hh::mm::ss` format.
#'
#' ```{asciicast progress-var-elapsed-clock, echo = 2:5}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 100,
#'   format = "{cli::pb_bar} {cli::pb_percent} [{cli::pb_elapsed_clock}]"
#' )
#' ))
#' cli:::var_helper2(x, current = 65, delay = 5)
#' ```

cli__pb_elapsed_clock <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  s <- (.Call(clic_get_time) - pb$start) * clienv$speed_time
  hours <- floor(s / 3600)
  minutes <- floor((s / 60) %% 60)
  seconds <- round(s %% 60, 1)
  paste0(
    formatC(hours, width = 2, flag = "0"),
    ":",
    formatC(minutes, width = 2, flag = "0"),
    ":",
    formatC(seconds, width = 2, flag = "0")
  )
}

#' @name progress-variables
#' @export pb_elapsed_raw
#' @usage NULL
#' @aliases pb_elapsed_raw
#'
#' @details
#' ### `pb_elapsed_raw`
#'
#' The number of seconds since the start of the progress bar.
#'
#' ```{asciicast progress-var-elapsed-raw, echo = 2:5}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 100,
#'   format = "{cli::pb_bar} {cli::pb_percent} [{round(cli::pb_elapsed_raw)}s]"
#' )
#' ))
#' cli:::var_helper2(x, current = 65, delay = 5)
#' ```

cli__pb_elapsed_raw <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  (.Call(clic_get_time) - pb$start) * clienv$speed_time
}

#' @name progress-variables
#' @export pb_eta
#' @usage NULL
#' @aliases pb_eta
#'
#' @details
#' ### `pb_eta`
#'
#' The estimated time until the end of the progress bar,
#' in human readable form.
#'
#' ```{asciicast progress-var-eta, echo = 2:5}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 100,
#'   format = "{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
#' )
#' ))
#' cli:::var_helper2(x, current = 65, delay = 5)
#' ```

cli__pb_eta <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  eta <- cli__pb_eta_raw(pb)
  if (is.na(eta)) {
    "?"
  } else {
    format_time_ago$vague_dt(eta, format = "terse")
  }
}

#' @name progress-variables
#' @export pb_eta_raw
#' @usage NULL
#' @aliases pb_eta_raw
#'
#' @details
#' ### `pb_eta_raw`
#'
#' The estimated time until the end of the progress
#' bar, in seconds. This is useful if you want to adjust the default
#' `pb_eta` display.
#'
#' ```{asciicast progress-var-eta-raw, echo = 2:5}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 100,
#'   format = "{cli::pb_bar} {cli::pb_percent} | ETA: {round(cli::pb_eta_raw)}s"
#' )
#' ))
#' cli:::var_helper2(x, current = 65, delay = 5)
#' ```

cli__pb_eta_raw <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  if (is.na(pb$total)) return(NA_real_)
  if (pb$current == pb$total) return(as.difftime(0, units = "secs"))
  if (pb$current == 0L) return(NA_real_)
  elapsed <- (.Call(clic_get_time) - pb$start) * clienv$speed_time
  as.difftime(elapsed * (pb$total / pb$current - 1.0), units = "secs")
}

#' @name progress-variables
#' @export pb_eta_str
#' @usage NULL
#' @aliases pb_eta_str
#'
#' @details
#' ### `pb_eta_str`
#'
#' The estimated time until the end of the progress bar.
#' It includes the `"ETA:"` prefix. It is only shown if the time can be
#' estimated, otherwise it is the empty string.
#'
#' ```{asciicast progress-var-eta-str, echo = 2:5}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 100,
#'   format = "{cli::pb_bar} {cli::pb_percent} | {cli::pb_eta_str}"
#' )
#' ))
#' cli:::var_helper2(x, current = 65, delay = 5)
#' ```

cli__pb_eta_str <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  eta <- cli__pb_eta(pb)
  if (eta != "?") paste0("ETA: ", eta) else ""
}

#' @name progress-variables
#' @export pb_extra
#' @usage NULL
#' @aliases pb_extra
#'
#' @details
#' ### `pb_extra`
#'
#' `pb_extra` can be used to access extra data, see the `extra` argument
#' of `cli_progress_bar()` and `cli_progress_update()`.
#'
#' ```{asciicast progress-var-extra, echo = 2:6}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 100,
#'   extra = list(user = whoami::username()),
#'   format = "Cleaning cache for user '{cli::pb_extra$user}': {cli::pb_current_bytes}"
#' )
#' ))
#' cli:::var_helper(x, current = 1024 * 1024 * 154)
#' ```

cli__pb_extra <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  pb$extra
}

#' @name progress-variables
#' @export pb_id
#' @usage NULL
#' @aliases pb_id
#'
#' @details
#' ### `pb_id`
#'
#' The id of the progress bar. The id has the format
#' `cli-<pid>-<counter>` where `<pid>` is the process id, and
#' `<counter>` is an integer counter that is incremented every time
#' cli needs a new unique id.
#'
#' This is useful for debugging progress bars.
#'
#' ```{asciicast progress-var-id, echo = 2:4}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   format = "Progress bar '{cli::pb_id}' is at {cli::pb_current}"
#' )
#' ))
#' cli:::var_helper(x, current = 64)
#' ```

cli__pb_id <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  pb$id
}

#' @name progress-variables
#' @export pb_name
#' @usage NULL
#' @aliases pb_name
#'
#' @details
#' ### `pb_name`
#'
#' The name of the progress bar. This is supplied by the
#' developer, and it is by default the empty string. A space character
#' is added to non-empty names.
#'
#' ```{asciicast progress-var-name, echo = 2:6}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   name = "Loading training data",
#'   total = 100,
#'   format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent}"
#' )
#' ))
#' cli:::var_helper(x, current = 66)
#' ```
#'

cli__pb_name <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  if (!is.null(pb$name)) {
    paste0(pb$name, " ")
  } else {
    ""
  }
}

#' @name progress-variables
#' @export pb_percent
#' @usage NULL
#' @aliases pb_percent
#'
#' @details
#' ### `pb_percent`
#'
#' The percentage of the progress bar, always formatted
#' in three characters plus the percentage sign. If the total number of
#' units is unknown, then it is `" NA%"`.
#'
#' ```{asciicast progress-var-percent, echo = 2:5}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 100,
#'   format = "{cli::pb_bar} {cli::pb_percent}"
#' )
#' ))
#' cli:::var_helper(x, current = 66)
#' ```

cli__pb_percent <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  paste0(format(round(pb$current / pb$total * 100), width = 3), "%")
}

#' @name progress-variables
#' @export pb_pid
#' @usage NULL
#' @aliases pb_pid
#'
#' @details
#' ### `pb_pid`
#'
#' The integer process id of the progress bar. This is useful if you are
#' aggregating logging output or progress results from multiple processes.

cli__pb_pid <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  pb$pid %||% Sys.getpid()
}

#' @name progress-variables
#' @export pb_rate
#' @usage NULL
#' @aliases pb_rate
#'
#' @details
#' ### `pb_rate`
#'
#' The progress rate, in number of units per second, formatted in a string.
#'
#' ```{asciicast progress-var-rate, echo = 2:5}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 156,
#'   format = "Reading input files {pb_current}/{pb_total} [{pb_rate}]"
#' )
#' ))
#' cli:::var_helper2(x, current = 67, delay = 5)
#' ```

cli__pb_rate <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  rate <- cli__pb_rate_raw(pb)
  if (is.nan(rate) || is.na(rate) || is.infinite(rate)) return("?/s")
  paste0(format(rate, digits = 2), "/s")
}

#' @name progress-variables
#' @export pb_rate_raw
#' @usage NULL
#' @aliases pb_rate_raw
#'
#' @details
#' ### `pb_rate_raw`
#'
#' The raw progress rate, in number of units per second.
#'
#' ```{asciicast progress-var-rate-raw, echo = 2:5}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 156,
#'   format = "Reading input files {pb_current}/{pb_total} [{round(pb_rate_raw)}/s]"
#' )
#' ))
#' cli:::var_helper2(x, current = 67, delay = 5)
#' ```

cli__pb_rate_raw <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  elapsed <- cli__pb_elapsed_raw(pb)
  pb$current / elapsed
}

#' @name progress-variables
#' @export pb_rate_bytes
#' @usage NULL
#' @aliases pb_rate_bytes
#'
#' @details
#' ### `pb_rate_bytes`
#'
#' The progress rate, formatted as bytes per second, in human readable form.
#'
#' ```{asciicast progress-var-rate-bytes, echo = 2:7}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 256 * 1024 * 1014,
#'   format = paste0(
#'     "Reading data {pb_current_bytes}/{pb_total_bytes} ",
#'     "[{ansi_trimws(pb_rate_bytes)}]"
#'   )
#' )
#' ))
#' cli:::var_helper2(x, current = 67 * 1024 * 1024, delay = 5)
#' ```

cli__pb_rate_bytes <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  rate <- cli__pb_rate_raw(pb)
  paste0(
    format_bytes$pretty_bytes(rate, style = "6"),
    "/s"
  )
}

#' @name progress-variables
#' @export pb_spin
#' @usage NULL
#' @aliases pb_spin
#'
#' @details
#' ### `pb_spin`
#'
#' A spinner. The default spinner is selected via a [get_spinner()] call.
#'
#' ```{asciicast progress-var-current, echo = 2:5}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 100,
#'   format = "{cli::pb_spin} Reading file {cli::pb_current}/{cli::pb_total}"
#' )
#' ))
#' cli:::var_helper(x, current = 66)
#' ```

cli__pb_spin <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")

  pb$spinner <- pb$spinner %||% get_spinner()
  nx <- pb$tick %% length(pb$spinner$frames) + 1L
  pb$spinner$frames[[nx]]
}

#' @name progress-variables
#' @export pb_status
#' @usage NULL
#' @aliases pb_status
#'
#' @details
#' ### `pb_status`
#'
#' The status string of the progress bar. By default this is an empty
#' string, but it is possible to set it in [cli_progress_bar()]
#' and `cli_progress_update()].
#'
#' ```{asciicast progress-var-status, echo = 2}
#' x <- invisible(quote(
#' cli_progress_bar(status = "Connecting...")
#' ))
#' cli:::var_helper(x, current = 0, delay = 1)
#' ```

cli__pb_status <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  if (!is.null(pb$status)) {
    paste0(pb$status, " ")
  } else {
    ""
  }
}

#' @name progress-variables
#' @export pb_timestamp
#' @usage NULL
#' @aliases pb_timestamp
#'
#' @details
#' ### `pb_timestamp`
#'
#' A time stamp for the current time in ISO 8601 format.
#'
#' ```{asciicast progress-var-timestamp, echo = 2:4}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   "Loading training data files",
#'   format = "{pb_timestamp} {pb_current} ({pb_rate})"
#' )
#' ))
#' cli:::var_helper(x, current = 125, delay = 5)
#' ```

cli__pb_timestamp <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  st <- Sys.time()
  if (clienv$speed_time != 1.0) {
    st <- clienv$load_time + (st - clienv$load_time) * clienv$speed_time
  }
  format_iso_8601(st)
}

#' @name progress-variables
#' @export pb_total
#' @usage NULL
#' @aliases pb_total
#'
#' @details
#' ### `pb_total`
#'
#' The total number of progress units, or `NA` if the number of units is
#' unknown.
#'
#' ```{asciicast progress-var-current, echo = 2:5}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 100,
#'   format = "{cli::pb_spin} Reading file {cli::pb_current}/{cli::pb_total}"
#' )
#' ))
#' cli:::var_helper(x, current = 66)
#' ```

cli__pb_total <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  pb$total
}

#' @name progress-variables
#' @export pb_total_bytes
#' @usage NULL
#' @aliases pb_total_bytes
#'
#' @details
#' ### `pb_total_bytes`
#'
#' The total number of progress units, formatted as
#' bytes, in a human readable format.
#'
#' ```{asciicast progress-var-rate-bytes, echo = 2:7}
#' x <- invisible(quote(
#' cli_progress_bar(
#'   total = 256 * 1024 * 1014,
#'   format = paste0(
#'     "Reading data {pb_current_bytes}/{pb_total_bytes} ",
#'     "[{ansi_trimws(pb_rate_bytes)}]"
#'   )
#' )
#' ))
#' cli:::var_helper2(x, current = 67 * 1024 * 1024, delay = 5)
#' ```

cli__pb_total_bytes <- function(pb = getOption("cli__pb")) {
  if (is.null(pb)) return("")
  format_bytes$pretty_bytes(pb$total, style = "6")
}

# ------------------------------------------------------------------------

var_helper <- function(expr, current = 66, delay = 1) {
  expr[[1]] <- quote(cli_progress_demo)
  expr$at <- current
  expr$start <- as.difftime(delay, units = "secs")
  eval(expr)
}

var_helper2 <- function(expr, clear = TRUE, delay = 0, ...) {
  expr$.envir <- environment()
  id <- eval(expr, envir = new.env())
  bar <- clienv$progress[[id]]
  bar$start <- bar$start - delay
  bar$clear <- clear
  args <- list(...)
  for (i in seq_along(args)) bar[[names(args)[i]]] <- args[[i]]
  cli_progress_update(force = TRUE, id = id)
  cat("\n")
  suppressMessages(cli_progress_done(id = id))
}

# ------------------------------------------------------------------------

#' cli progress bar demo
#'
#' Useful for experimenting with format strings and for documentation.
#' It creates a progress bar, iterates it until it terminates and saves the
#' progress updates.
#'
#' @param name Passed to [cli_progress_bar()].
#' @param status Passed to [cli_progress_bar()].
#' @param type Passed to [cli_progress_bar()].
#' @param total Passed to [cli_progress_bar()].
#' @param .envir Passed to [cli_progress_bar()].
#' @param ... Passed to [cli_progress_bar()].
#' @param at The number of progress units to show and capture the progress
#'   bar at. If `NULL`, then a sequence of states is generated to show the
#'   progress from beginning to end.
#' @param show_after Delay to show the progress bar. Overrides the
#'   `cli.progress_show_after` option.
#' @param live Whether to show the progress bat on the screen, or just
#'   return the recorded updates. Defaults to the value of the
#'   `cli.progress_demo_live` options. If unset, then it is `TRUE` in
#'   interactive sessions.
#' @param delay Delay between progress bar updates.
#' @param start Time to subtract from the start time, to simulate a
#'   progress bar that takes longer to run.
#'
#' @return List with class `cli_progress_demo`, which has a print and a
#' format method for pretty printing. The `lines` entry contains the
#' output lines, each corresponding to one update.
#'
#' @export

# TODO: examples

cli_progress_demo <- function(name = NULL, status = NULL,
                              type = c("iterator", "tasks",
                                       "download", "custom"),
                              total = NA,
                              .envir = parent.frame(),
                              ...,
                              at = if (is_interactive()) NULL else 50,
                              show_after = 0,
                              live = NULL,
                              delay = 0,
                              start = as.difftime(5, units = "secs")) {

  opt <- options(cli.progress_show_after = show_after)
  on.exit(options(opt), add = TRUE)

  live <- live %||%
    getOption("cli.progress_demo_live") %||%
    is_interactive()

  id <- cli_progress_bar(
    name = name,
    status = status,
    type = type,
    total = total,
    ...,
    .envir = .envir,
    current = FALSE
  )
  bar <- clienv$progress[[id]]
  bar$start <- bar$start - as.double(start, units = "secs")

  last <- is.null(at)
  if (is.null(at)) {
    if (is.na(total)) {
      at <- 1:5
    } else {
      at <- seq_len(total)
    }
  }

  output <- file(open = "w+b")
  on.exit(close(output), add = TRUE)
  size <- 0L

  withCallingHandlers({
    for (crnt in at) {
      cli_progress_update(set = crnt, id = id, force = TRUE, .envir = .envir)
      if (delay > 0) Sys.sleep(delay)
    }
    if (last) {
      cli_progress_done(id = id, .envir = .envir)
    } else {
      suppressMessages(cli_progress_done(id = id, .envir = .envir))
    }
  }, cliMessage = function(msg) {
    cat(file = output, msg$message)
    size <<- size + nchar(msg$message, type = "bytes")
    if (!live) invokeRestart("muffleMessage")
  })

  lines <- readChar(output, size, useBytes = TRUE)
  lines <- sub_("^\r\r*", "", lines, useBytes = TRUE)
  lines <- sub_("\r\r*$", "", lines, useBytes = TRUE)
  lines <- gsub_("\r\r*", "\r", lines, useBytes = TRUE)
  lines <- strsplit_(lines, "[\r\n]", useBytes = TRUE)[[1]]

  res <- structure(
    list(lines = lines),
    class = "cli_progress_demo"
  )

  if (live) invisible(res) else res
}

#' @export

format.cli_progress_demo <- function(x, ...) {
  x$lines
}

#' @export

print.cli_progress_demo <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}
