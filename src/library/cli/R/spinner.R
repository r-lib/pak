
## See tools/spinners.R for how the RDS file is created

#' Character vector to put a spinner on the screen
#'
#' `cli` contains many different spinners, you choose one according to your
#' taste.
#'
#' ```{asciicast get-spinner}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' options(cli.spinner = "hearts")
#' fun <- function() {
#'   cli_progress_bar("Spinning")
#'   for (i in 1:100) {
#'     Sys.sleep(4/100)
#'     cli_progress_update()
#'   }
#' }
#' fun()
#' options(cli.spinner = NULL)
#' ```
#'
#' @param which The name of the chosen spinner. If `NULL`, then the default
#'   is used, which can be customized via the `cli.spinner_unicode`,
#'   `cli.spinner_ascii` and `cli.spinner` options. (The latter applies to
#'   both Unicode and ASCII displays. These options can be set to the name
#'   of a built-in spinner, or to a list that has an entry called `frames`,
#'   a character vector of frames.
#' @return A list with entries: `name`, `interval`: the suggested update
#'   interval in milliseconds and `frames`: the character vector of the
#'   spinner's frames.
#'
#' @family spinners
#' @export

get_spinner <- function(which = NULL) {
  stopifnot(is.null(which) || is_string(which) || is.list(which))

  if (is.null(which)) {
    if (is_utf8_output()) {
      which <-
        getOption("cli.spinner_unicode") %||%
        getOption("cli.spinner") %||%
        "dots"
    } else {
      which <-
        getOption("cli.spinner_ascii") %||%
        getOption("cli.spinner") %||%
        "line"
    }
  }

  if (is.character(which)) {
    row <- match(which, spinners$name)
    which <- list(
      name = which,
      interval = spinners$interval[[row]],
      frames = spinners$frames[[row]])
  }

  if (!is.character(which$frames)) {
    stop("Spinner frames must be a character vector")
  }

  which$name <- which$name %||% NA_character_
  which$interval <- which$interval %||% 100L

  which
}

#' List all available spinners
#'
#' @return Character vector of all available spinner names.
#'
#' @family spinners
#' @export
#' @examples
#' list_spinners()
#' get_spinner(list_spinners()[1])

list_spinners <- function() {
  spinners$name
}

#' Create a spinner
#'
#' @param template A template string, that will contain the spinner. The
#'   spinner itself will be substituted for `{spin}`. See example below.
#' @param stream The stream to use for the spinner. Typically this is
#'   standard error, or maybe the standard output stream.
#'   It can also be a string, one of `"auto"`, `"message"`, `"stdout"`,
#'   `"stderr"`. `"auto"` will select `stdout()` if the session is
#'   interactive and there are no sinks, otherwise it will select
#'   `stderr()`.
#' @param static What to do if the terminal does not support dynamic
#'   displays:
#'   * `"dots"`: show a dot for each `$spin()` call.
#'   * `"print"`: just print the frames of the spinner, one after another.
#'   * `"print_line"`: print the frames of the spinner, each on its own line.
#'   * `"silent"` do not print anything, just the `template`.
#' @inheritParams get_spinner
#' @return A `cli_spinner` object, which is a list of functions. See
#'   its methods below.
#'
#' `cli_spinner` methods:
#' * `$spin()`: output the next frame of the spinner.
#' * `$finish()`: terminate the spinner. Depending on terminal capabilities
#'   this removes the spinner from the screen. Spinners can be reused,
#'   you can start calling the `$spin()` method again.
#'
#' All methods return the spinner object itself, invisibly.
#'
#' The spinner is automatically throttled to its ideal update frequency.
#'
#' @section Examples:
#'
#' ## Default spinner
#'

#' ```{asciicast make-spinner-default}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE,
#' #| asciicast_end_wait = 0
#' sp1 <- make_spinner()
#' fun_with_spinner <- function() {
#'   lapply(1:100, function(x) { sp1$spin(); Sys.sleep(0.05) })
#'   sp1$finish()
#' }
#' ansi_with_hidden_cursor(fun_with_spinner())
#' ```
#'
#' ## Spinner with a template
#'
#' ```{asciicast make-spinner-template}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE,
#' #| asciicast_end_wait = 0
#' sp2 <- make_spinner(template = "Computing {spin}")
#' fun_with_spinner2 <- function() {
#'   lapply(1:100, function(x) { sp2$spin(); Sys.sleep(0.05) })
#'   sp2$finish()
#' }
#' ansi_with_hidden_cursor(fun_with_spinner2())
#' ```
#'
#' ## Custom spinner
#'
#' ```{asciicast make-spinner-custom}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' sp3 <- make_spinner("simpleDotsScrolling", template = "Downloading {spin}")
#' fun_with_spinner3 <- function() {
#'   lapply(1:100, function(x) { sp3$spin(); Sys.sleep(0.05) })
#'   sp3$finish()
#' }
#' ansi_with_hidden_cursor(fun_with_spinner3())
#' ```
#'
#' @family spinners
#' @export

make_spinner <- function(which = NULL, stream = "auto", template = "{spin}",
                         static = c("dots", "print", "print_line",
                                    "silent")) {

  stopifnot(
    inherits(stream, "connection") || is_string(stream),
    is_string(template))

  c_stream <- get_real_output(stream)
  c_spinner <- get_spinner(which)
  c_template <- template
  c_static <- match.arg(static)
  c_state <- 1L
  c_first <- TRUE
  c_col <- 1L
  c_width <- 0L
  c_last <- Sys.time() - as.difftime(1, units = "secs")
  c_int <- as.difftime(c_spinner$interval / 1000, units = "secs")

  c_res <- list()

  throttle <- function() Sys.time() - c_last < c_int
  clear_line <- function() {
    str <- paste0(c("\r", rep(" ", c_width), "\r"), collapse = "")
    cat(str, file = c_stream)
  }
  inc <- function() {
    c_state <<- c_state + 1L
    c_first <<- FALSE
    if (c_state > length(c_spinner$frames)) c_state <<- 1L
    c_last <<- Sys.time()
    invisible(c_res)
  }

  c_res$finish <- function() {
    c_state <<- 1L
    c_first <<- TRUE
    c_col <<- 1L
    c_last <<- Sys.time()
    if (is_dynamic_tty(c_stream)) clear_line() else cat("\n", file = c_stream)
    invisible(c_res)
  }

  if (is_dynamic_tty(c_stream)) {
    c_res$spin <- function(template = NULL) {
      if (!is.null(template)) c_template <<- template
      if (throttle()) return()
      line <- sub("{spin}", c_spinner$frames[[c_state]], c_template,
                  fixed = TRUE)
      line_width <- ansi_nchar(line)
      if (is_ansi_tty(c_stream)) {
        cat("\r", line, ANSI_EL, sep = "", file = c_stream)
      } else {
        # extra padding in case the line width has changed
        # so that we don't get any garbage in the output
        padding <- if (line_width < c_width) {
          paste0(rep(" ", line_width), collapse = "")
        } else {
          ""
        }
        cat("\r", line, padding, sep = "", file = c_stream)
      }
      # save the new line width
      c_width <<- line_width
      inc()
    }

  } else {
    if (c_static == "dots") {
      c_res$spin <- function(template = NULL) {
        if (!is.null(template)) c_template <<- template
        if (c_first) cat(template, "\n", sep = "", file = c_stream)
        if (throttle()) return()
        cat(".", file = c_stream)
        c_col <<- c_col + 1L
        if (c_col == console_width()) {
          cat("\n", file = c_stream)
          c_col <<- 1L
        }
        inc()
      }
    } else if (c_static == "print") {
      c_res$spin <- function(template = NULL) {
        if (!is.null(template)) c_template <<- template
        if (throttle()) return()
        line <- sub("{spin}", c_spinner$frames[[c_state]], c_template,
                    fixed = TRUE)
        cat(line, file = c_stream)
        inc()
      }
    } else if (c_static == "print_line") {
      c_res$spin <- function(template = NULL) {
        if (!is.null(template)) c_template <<- template
        if (throttle()) return()
        line <- sub("{spin}", c_spinner$frames[[c_state]], c_template,
                    fixed = TRUE)
        cat(line, "\n", sep = "", file = c_stream)
        inc()
      }
    } else if (c_static == "silent") {
      c_res$spin <- function(template = NULL) {
        if (!is.null(template)) c_template <<- template
        if (throttle()) return()
        inc()
      }
    }
  }

  class(c_res) <- "cli_spinner"
  c_res
}

#' @export

print.cli_spinner <- function(x, ...) {
  cat("<cli_spinner>\n")
  invisible(x)
}

## nocov start

#' Show a demo of some (by default all) spinners
#'
#' Each spinner is shown for about 2-3 seconds.
#'
#' @details
#'
#' ```{asciicast demo-spinners}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE,
#' #| asciicast_end_wait = 0
#' demo_spinners("clock")
#' ```
#'
#' @param which Character vector, which spinners to demo.
#'
#' @family spinners
#' @export

demo_spinners <- function(which = NULL) {
  stopifnot(is.null(which) || is.character(which))

  all <- list_spinners()
  which <- which %||% all

  if (length(bad <- setdiff(which, all))) {
    stop("Unknown spinners: ", paste(bad, collapse = ", "))
  }

  for (w in which) {
    sp <- get_spinner(w)
    interval <- sp$interval / 1000
    frames <- sp$frames
    cycles <- max(round(2.5 / ((length(frames) - 1) * interval)), 1)
    for (i in 1:(length(frames) * cycles) - 1) {
      fr <- unclass(frames[i %% length(frames) + 1])
      cat("\r", rpad(fr, width = 10), w, sep = "")
      Sys.sleep(interval)
    }
    cat("\n")
  }
}

demo_spinners_terminal <- function(ticks = 100 * 3000) {
  up <- function(n) cat(paste0("\u001B[", n, "A"))
  show <- function() cat("\u001b[?25h")
  hide <- function() cat("\u001b[?25l")

  on.exit(show(), add = TRUE)

  names <- unlist(spinners$name)
  frames <- spinners$frames
  intervals <- unlist(spinners$interval)
  num_frames <- viapply(frames, length)
  spin_width <- viapply(frames, function(x) max(nchar(x, type = "width")))
  name_width <- nchar(names, type = "width")
  col_width <- spin_width + max(name_width) + 3
  col1_width <- max(col_width[1:(length(col_width)/2)])

  frames <- mapply(
    frames,
    names,
    FUN = function(f, n) {
      rpad(paste(lpad(n, max(name_width) + 2), f), col1_width)
    }
  )

  hide()

  for (tick in 0:ticks) {
    tic <- Sys.time()
    wframe <- trunc(tick / intervals) %% num_frames + 1
    sp <- mapply(frames, wframe, FUN = "[")

    sp2 <- paste(
      sep = "  ",
      sp[1:(length(sp) / 2)],
      sp[(length(sp) / 2 + 1):length(sp)]
    )

    cat(sp2, sep = "\n")
    up(length(sp2))
    took <- Sys.time() - tic
    togo <- as.difftime(1/1000, units = "secs") - took
    if (togo > 0) Sys.sleep(togo)
  }

}

## nocov end
