is_interactive <- function() {
  opt <- getOption("rlib_interactive")
  if (isTRUE(opt)) {
    TRUE
  } else if (identical(opt, FALSE)) {
    FALSE
  } else if (tolower(getOption("knitr.in.progress", "false")) == "true") {
    FALSE
  } else if (
    tolower(getOption("rstudio.notebook.executing", "false")) == "true"
  ) {
    FALSE
  } else if (identical(Sys.getenv("TESTTHAT"), "true")) {
    FALSE
  } else {
    interactive()
  }
}

#' The connection option that cli would use
#'
#' Note that this only refers to the current R process. If the output
#' is produced in another process, then it is not relevant.
#'
#' In interactive sessions the standard output is chosen, otherwise the
#' standard error is used. This is to avoid painting output messages red
#' in the R GUIs.
#'
#' @return Connection object.
#'
#' @export

cli_output_connection <- function() {
  if ((is_interactive() || rstudio_stdout()) && no_sink()) stdout() else
    stderr()
}

no_sink <- function() {
  sink.number() == 0 && sink.number("message") == 2
}

rstudio_stdout <- function() {
  rstudio <- rstudio_detect()
  rstudio$type %in%
    c(
      "rstudio_console",
      "rstudio_console_starting",
      "rstudio_build_pane",
      "rstudio_job",
      "rstudio_render_pane"
    )
}

is_stdout <- function(stream) {
  identical(stream, stdout()) && sink.number() == 0
}

is_stderr <- function(stream) {
  identical(stream, stderr()) && sink.number("message") == 2
}

is_stdx <- function(stream) {
  is_stdout(stream) || is_stderr(stream)
}

is_rstudio_dynamic_tty <- function(stream) {
  rstudio$detect()[["dynamic_tty"]] &&
    (is_stdout(stream) || is_stderr(stream))
}

is_rapp <- function() {
  Sys.getenv("R_GUI_APP_VERSION") != ""
}

is_rapp_stdx <- function(stream) {
  interactive() &&
    is_rapp() &&
    (is_stdout(stream) || is_stderr(stream))
}

is_emacs <- function() {
  Sys.getenv("EMACS") != "" || Sys.getenv("INSIDE_EMACS") != ""
}

is_rkward <- function() {
  "rkward" %in% (.packages())
}

is_rkward_stdx <- function(stream) {
  interactive() &&
    is_rkward() &&
    (is_stdout(stream) || is_stderr(stream))
}

#' Detect whether a stream supports `\\r` (Carriage return)
#'
#' In a terminal, `\\r` moves the cursor to the first position of the
#' same line. It is also supported by most R IDEs. `\\r` is typically
#' used to achieve a more dynamic, less cluttered user interface, e.g.
#' to create progress bars.
#'
#' If the output is directed to a file, then `\\r` characters are typically
#' unwanted. This function detects if `\\r` can be used for the given
#' stream or not.
#'
#' The detection mechanism is as follows:
#' 1. If the `cli.dynamic` option is set to `TRUE`, `TRUE` is returned.
#' 2. If the `cli.dynamic` option is set to anything else, `FALSE` is
#'    returned.
#' 3. If the `R_CLI_DYNAMIC` environment variable is not empty and set to
#'    the string `"true"`, `"TRUE"` or `"True"`, `TRUE` is returned.
#' 4. If `R_CLI_DYNAMIC` is not empty and set to anything else, `FALSE` is
#'    returned.
#' 5. If the stream is a terminal, then `TRUE` is returned.
#' 6. If the stream is the standard output or error within RStudio,
#'    the macOS R app, or RKWard IDE, `TRUE` is returned.
#' 7. Otherwise `FALSE` is returned.
#'
#' @param stream The stream to inspect or manipulate, an R connection
#' object. It can also be a string, one of `"auto"`, `"message"`,
#' `"stdout"`, `"stderr"`. `"auto"` will select `stdout()` if the session is
#' interactive and there are no sinks, otherwise it will select `stderr()`.
#'
#' @family terminal capabilities
#' @export
#' @examples
#' is_dynamic_tty()
#' is_dynamic_tty(stdout())

is_dynamic_tty <- function(stream = "auto") {
  stream <- get_real_output(stream)

  ## Option?
  if (!is.null(x <- getOption("cli.dynamic"))) {
    return(isTRUE(x))
  }

  ## Env var?
  if ((x <- Sys.getenv("R_CLI_DYNAMIC", "")) != "") {
    return(isTRUE(as.logical(x)))
  }

  ## Autodetect...
  ## RGui has isatty(stdout()) and isatty(stderr()), so we don't need
  ## to check that explicitly
  isatty(stream) ||
    is_rstudio_dynamic_tty(stream) ||
    is_rapp_stdx(stream) ||
    is_rkward_stdx(stream)
}

ANSI_ESC <- "\u001B["
ANSI_HIDE_CURSOR <- paste0(ANSI_ESC, "?25l")
ANSI_SHOW_CURSOR <- paste0(ANSI_ESC, "?25h")
ANSI_EL <- paste0(ANSI_ESC, "K")

#' Detect if a stream support ANSI escape characters
#'
#' We check that all of the following hold:
#' * The stream is a terminal.
#' * The platform is Unix.
#' * R is not running inside R.app (the macOS GUI).
#' * R is not running inside RStudio.
#' * R is not running inside Emacs.
#' * The terminal is not "dumb".
#' * `stream` is either the standard output or the standard error stream.
#'
#' @inheritParams is_dynamic_tty
#' @return `TRUE` or `FALSE`.
#'
#' @family terminal capabilities
#' @export
#' @examples
#' is_ansi_tty()

is_ansi_tty <- function(stream = "auto") {
  stream <- get_real_output(stream)

  # Option takes precedence
  opt <- getOption("cli.ansi")
  if (isTRUE(opt)) {
    return(TRUE)
  } else if (identical(opt, FALSE)) {
    return(FALSE)
  }

  # RStudio is handled separately
  if (rstudio$detect()[["ansi_tty"]] && is_stdx(stream)) return(TRUE)

  isatty(stream) &&
    .Platform$OS.type == "unix" &&
    !is_rapp() &&
    !is_emacs() &&
    Sys.getenv("TERM", "") != "dumb" &&
    is_stdx(stream)
}

#' Hide/show cursor in a terminal
#'
#' This only works in terminal emulators. In other environments, it
#' does nothing.
#'
#' `ansi_hide_cursor()` hides the cursor.
#'
#' `ansi_show_cursor()` shows the cursor.
#'
#' `ansi_with_hidden_cursor()` temporarily hides the cursor for
#' evaluating an expression.
#'
#' @inheritParams is_dynamic_tty
#' @param expr R expression to evaluate.
#'
#' @family terminal capabilities
#' @family low level ANSI functions
#' @export

ansi_hide_cursor <- function(stream = "auto") {
  if (Sys.getenv("R_CLI_HIDE_CURSOR") == "false") return()
  stream <- get_real_output(stream)
  if (is_ansi_tty(stream)) cat(ANSI_HIDE_CURSOR, file = stream)
}

#' @export
#' @name ansi_hide_cursor

ansi_show_cursor <- function(stream = "auto") {
  if (Sys.getenv("R_CLI_HIDE_CURSOR") == "false") return()
  stream <- get_real_output(stream)
  if (is_ansi_tty(stream)) cat(ANSI_SHOW_CURSOR, file = stream)
}

#' @export
#' @name ansi_hide_cursor

ansi_with_hidden_cursor <- function(expr, stream = "auto") {
  stream <- get_real_output(stream)
  ansi_hide_cursor(stream)
  on.exit(ansi_show_cursor(), add = TRUE)
  expr
}

get_embedded_utf8 <- function() {
  .Call(clic_get_embedded_utf8)
}

set_embedded_utf8 <- function(value = TRUE) {
  .Call(clic_set_embedded_utf8, value)
}

r_utf8 <- function(func, args = list(), package = FALSE, timeout = 5000L) {
  out <- tempfile()
  on.exit(unlink(out), add = TRUE)
  opts <- callr::r_process_options(
    func = func,
    args = args,
    package = package,
    stdout = out,
    stderr = out
  )
  if (.Platform$OS.type == "windows") {
    opts$load_hook <- c(
      opts$load_hook,
      "invisible(cli:::set_embedded_utf8())"
    )
  }
  rp <- callr::r_process$new(opts)
  rp$wait(timeout)
  if (rp$is_alive()) {
    rp$kill()
    throw(cli_error("R subprocess timeout"))
  }
  list(
    status = rp$get_exit_status(),
    stdout = fix_r_utf8_output(readBin(out, "raw", file.size(out)))
  )
}

fix_r_utf8_output <- function(x) {
  beg <- grepRaw(as.raw(c(2, 255, 254)), x, fixed = TRUE, all = TRUE)
  end <- grepRaw(as.raw(c(3, 255, 254)), x, fixed = TRUE, all = TRUE)

  # In case the output is incomplete, and an UTF-8 tag is left open
  if (length(end) < length(beg)) end <- c(end, length(x) + 1L)

  if (length(beg) != length(end)) {
    throw(cli_error(
      "Invalid output from UTF-8 R",
      "i" = "Found {length(beg)} UTF-8 begin marker{?s} and {length(end)} end marker{?s}."
    ))
  }

  # Easier to handle corner cases with this
  beg <- c(beg, length(x) + 1L)
  end <- c(end, length(x) + 1L)

  out <- file(open = "w+b")
  size <- 0L
  on.exit(close(out), add = TRUE)

  doutf8 <- function(from, to) {
    if (from > to) return()
    writeBin(x[from:to], out)
    size <<- size + (to - from + 1L)
  }
  donati <- function(from, to) {
    if (from > to) return()
    xx <- iconv(list(x[from:to]), "", "UTF-8", toRaw = TRUE)[[1]]
    writeBin(xx, out)
    size <<- size + length(xx)
  }

  # Initial native part
  if (beg[1] > 1) donati(1, beg[1] - 1L)

  # UTF-8 chunk, and native part after them
  for (i in seq_along(beg)) {
    doutf8(beg[i] + 3L, end[i] - 1L)
    if (i < length(beg)) {
      donati(end[i] + 3L, beg[i + 1L] - 1L)
    }
  }

  chr <- readChar(out, size)
  Encoding(chr) <- "UTF-8"
  chr
}
