
#' Detect the number of ANSI colors to use
#'
#' @description
#' Certain Unix and Windows terminals, and also certain R GUIs, e.g.
#' RStudio, support styling terminal output using special control
#' sequences (ANSI sequences).
#'
#' `num_ansi_colors()` detects if the current R session supports ANSI
#' sequences, and if it does how many colors are supported.
#'
#' @param stream The stream that will be used for output, an R connection
#' object. It can also be a string, one of `"auto"`, `"message"`,
#' `"stdout"`, `"stderr"`. `"auto"` will select `stdout()` if the session is
#' interactive and there are no sinks, otherwise it will select `stderr()`.
#' @return Integer, the number of ANSI colors the current R session
#' supports for `stream`.
#'
#' @family ANSI styling
#' @export
#' @examples
#' num_ansi_colors()
#'
#' @details
#' The detection mechanism is quite involved and it is designed to work
#' out of the box on most systems. If it does not work on your system,
#' please report a bug. Setting options and environment variables to turn
#' on ANSI support is error prone, because they are inherited in other
#' environments, e.g. knitr, that might not have ANSI support.
#'
#' If you want to _turn off_ ANSI colors, set the `NO_COLOR` environment
#' variable to a non-empty value.
#'
#' The exact detection mechanism is as follows:

num_ansi_colors <- function(stream = "auto") {
  #' 1. If the `cli.num_colors` options is set, that is returned.

  opt <- getOption("cli.num_colors", NULL)
  if (!is.null(opt)) return(as.integer(opt))

  #' 1. If the `R_CLI_NUM_COLORS` environment variable is set to a
  #'    non-empty value, then it is used.

  if ((env <- Sys.getenv("R_CLI_NUM_COLORS", "")) != "") {
    return(as.integer(env))
  }

  #' 1. If the `crayon.enabled` option is set to `FALSE`, 1L is returned.
  #'    (This is for compatibility with code that uses the crayon package.)
  #' 1. If the `crayon.enabled` option is set to `TRUE` and the
  #'    `crayon.colors` option is not set, then the value of the
  #'    `cli.default_num_colors` option, or if it is unset, then 8L is
  #'    returned.
  #' 1. If the `crayon.enabled` option is set to `TRUE` and the
  #'    `crayon.colors` option is also set, then the latter is returned.
  #'    (This is for compatibility with code that uses the crayon package.)

  cray_opt_has <- getOption("crayon.enabled", NULL)
  cray_opt_num <- getOption("crayon.colors", NULL)
  if (!is.null(cray_opt_has) && !isTRUE(cray_opt_has)) return(1L)
  if (isTRUE(cray_opt_has) && !is.null(cray_opt_num)) {
    return(as.integer(cray_opt_num))
  }
  if (isTRUE(cray_opt_has) && is.null(cray_opt_num)) {
    default <- get_default_number_of_colors()
    return(default %||% 8L)
  }

  #' 1. If the `NO_COLOR` environment variable is set, then 1L is returned.

  if (!is.na(Sys.getenv("NO_COLOR", NA_character_))) return(1L)

  #' 1. If we are in knitr, then 1L is returned, to turn off colors in
  #'    `.Rmd` chunks.

  if (isTRUE(getOption("knitr.in.progress"))) return(1L)

  #' 1. If `stream` is `"auto"` (the default) and there is an active
  #'    sink (either for `"output"` or `"message"`), then we return 1L.
  #'    (In theory we would only need to check the stream that will be
  #'    be actually used, but there is no easy way to tell that.)
  if (stream == "auto" && !no_sink()) return(1L)

  # Defer computation on streams to speed up common case
  # when environment variables are set
  orig_stream <- stream
  stream <- get_real_output(stream)

  is_stdout <- is_stderr <- is_std <- FALSE
  std <- "nope"
  if (identical(stream, stdout())) {
    is_stdout <- is_std <- TRUE
    std <- "stdout"
  } else if (identical(stream, stderr())) {
    is_stderr <- is_std <- TRUE
    std <- "stderr"
  }

  #' 1. If `stream` is not `"auto"`, but it is `stderr()` and there is an
  #'    active sink for it, then 1L is returned.
  #'    (If a sink is active for "output", then R changes the `stdout()`
  #'    stream, so this check is not needed.)

  # If a sink is active for "message" (ie. stderr), then R does not update
  # the `stderr()` stream, so we need to catch this case.
  if (is_stderr && sink.number("message") != 2) return(1L)

  #' 1. If the `cli.default_num_colors` option is set, then we use that.

  dopt <- get_default_number_of_colors()
  if (!is.null(dopt)) return(as.integer(dopt))

  #' 1. If R is running inside RGui on Windows, or R.app on macOS, then we
  #'    return 1L.

  # RStudio sets GUI to RGui initially, so we'll handle that after RStudio.
  if (.Platform$GUI == "AQUA") return(1L)

  #' 1. If R is running inside RStudio, with color support, then the
  #'    appropriate number of colors is returned, usually 256L.

  rstudio <- rstudio$detect()
  rstudio_colors <- c(
    "rstudio_console",
    "rstudio_console_starting",
    "rstudio_build_pane",
    "rstudio_job"
  )
  if (is.na(rstudio$num_colors)) rstudio$num_colors <- 1L
  if (rstudio$type %in% rstudio_colors && is_std) {
    return(rstudio$num_colors)
  }

  # RGui? We need to do this after RStudio, because .Platform$GUI is
  # "Rgui" in RStudio when we are starting up
  if (.Platform$GUI == "Rgui") return(1L)

  #' 1. If R is running on Windows, inside an Emacs version that is recent
  #'    enough to support ANSI colors, then the value of the
  #'    `cli.default_num_colors` option, or if unset 8L is returned.
  #'    (On Windows, Emacs has `isatty(stdout()) == FALSE`, so we need to
  #'    check for this here before dealing with terminals.)

  # Windows Emacs? The top R process will have `--ess` in ESS, but the
  # subprocesses won't. (Without ESS subprocesses will also report 8L
  # colors, this is a problem, but we expect most people use ESS in Emacs.)
  if (os_type() == "windows" &&
      "--ess" %in% commandArgs() &&
      is_emacs_with_color()) {
    default <- get_default_number_of_colors()
    return(default %||% 8L)
  }

  #' 1. If `stream` is not the standard output or standard error  in a
  #'    terminal, then 1L is returned.

  if (!isatty(stream)) return(1L)
  if (!is_std) return(1L)

  #' 1. Otherwise we use and cache the result of the terminal color
  #'     detection (see below).

  # Otherwise use/set the cache
  if (is.null(clienv$num_colors)) clienv$num_colors <- list()
  clienv$num_colors[[std]] <- clienv$num_colors[[std]] %||% detect_tty_colors()
  clienv$num_colors[[std]]
}

#' @rdname num_ansi_colors
#' @details
#' The terminal color detection algorithm:

detect_tty_colors <- function() {

  default <- get_default_number_of_colors()

  #' 1. If the `COLORTERM` environment variable is set to `truecolor` or
  #'    `24bit`, then we return 16 million colors.
  #' 1. If the `COLORTERM` environment variable is set to anything else,
  #'    then we return the value of the `cli.num_default_colors` option,
  #'    8L if unset.

  ct <- Sys.getenv("COLORTERM", NA_character_)
  if (!is.na(ct)) {
    if (ct == "truecolor" || ct == "24bit") {
      return(truecolor)
    } else {
      return(default %||% 8L)
    }
  }

  #' 1. If R is running on Unix, inside an Emacs version that is recent
  #'    enough to support ANSI colors, then the value of the
  #'    `cli.default_num_colors` option is returned, or 8L if unset.

  if (os_type() == "unix" && is_emacs_with_color()) return(default %||% 8L)

  #' 1. If we are on Windows in an RStudio terminal, then apparently
  #'    we only have eight colors, but the `cli.default_num_colors` option
  #'    can be used to override this.

  win10 <- win10_build()
  if (os_type() == "windows" && win10 >= 10586 &&
      rstudio_detect()$type == "rstudio_terminal") {
    # this is rather weird, but echo turns on color support :D
    system2("cmd", c("/c", "echo 1 >NUL"))
    return(default %||% 8L)
  }

  #' 1. If we are in a recent enough Windows 10 terminal, then there
  #'    is either true color (from build 14931) or 256 color (from
  #'    build 10586) support. You can also use the `cli.default_num_colors`
  #'    option to override these.

  if (os_type() == "windows" && win10 >= 10586) {
    # this is rather weird, but echo turns on color support :D
    system2("cmd", c("/c", "echo 1 >NUL"))
    # https://devblogs.microsoft.com/commandline/24-bit-color-in-the-windows-console/
    if (win10 >= 14931) {
      return(default %||% truecolor)
    } else {
      return(default %||% 256L)
    }
  }

  if (os_type() == "windows") {

    #' 1. If we are on Windows, under ConEmu or cmder, or ANSICON is loaded,
    #'    then the value of `cli.default_num_colors`, or 8L if unset, is
    #'    returned.

    if (Sys.getenv("ConEmuANSI") == "ON" ||
        Sys.getenv("CMDER_ROOT") != "") {
      return(default %||% 8L)
    }
    if (Sys.getenv("ANSICON") != "") return(default %||% 8L)

    #' 1. Otherwise if we are on Windows, return 1L.

    return(1L)
  }

  #' 1. Otherwise we are on Unix and try to run `tput colors` to determine
  #'    the number of colors. If this succeeds, we return its return value.

  cols <- suppressWarnings(try(
    silent = TRUE,
    as.numeric(system("tput colors 2>/dev/null", intern = TRUE))[1]
  ))
  if (inherits(cols, "try-error") || !length(cols) || is.na(cols)) {
    return(guess_tty_colors())
  }
  if (cols %in% c(-1, 0, 1)) { return(1) }

  #'    If the `TERM` environment variable is `xterm` and `tput`
  #'    returned 8L, we return 256L, because xterm compatible terminals
  #'    tend to support 256 colors
  #'    (<https://github.com/r-lib/crayon/issues/17>)
  #'    You can override this with the `cli.default_num_colors` option.

  if (cols == 8 && identical(Sys.getenv("TERM"), "xterm")) {
    cols <- default %||% 256
  }

  #' 1. If `TERM` is set to `dumb`, we return 1L.
  #' 1. If `TERM` starts with `screen`, `xterm`, or `vt100`, we return 8L.
  #' 1. If `TERM` contains `color`, `ansi`, `cygwin` or `linux`, we return 8L.
  #' 1. Otherwise we return 1L.

  cols
}

get_default_number_of_colors <- function() {
  dft <- getOption("cli.default_num_colors")
  if (!is.null(dft)) {
    if (!is_count(dft)) {
      warning(
        "The `cli.default_num_colors` option must be an integer scalar"
      )
      dft <- NULL
    }
  }
  dft
}

guess_tty_colors <- function() {
  term <- Sys.getenv("TERM")
  if (term == "dumb") return (1L)

  if (grepl(
    "^screen|^xterm|^vt100|color|ansi|cygwin|linux",
    term,
    ignore.case = TRUE,
    perl = TRUE
  )) {
    8L
  } else {
    1L
  }
}

is_emacs_with_color <- function() {
  (Sys.getenv("EMACS") != "" || Sys.getenv("INSIDE_EMACS") != "") &&
    ! is.na(emacs_version()[1]) && emacs_version()[1] >= 23
}

emacs_version <- function() {
  ver <- Sys.getenv("INSIDE_EMACS")
  if (ver == "") return(NA_integer_)

  ver <- gsub("'", "", ver, fixed = TRUE)

  ver <- strsplit(ver, ",", fixed = TRUE)[[1]]
  ver <- strsplit(ver, ".", fixed = TRUE)[[1]]
  suppressWarnings(as.numeric(ver))
}

win10_build <- function() {
  os <- utils::sessionInfo()$running %||% ""
  if (!grepl("^Windows 10 ", os)) return(0L)
  mch <- re_match(os, "[(]build (?<build>[0-9]+)[)]")
  mch <- suppressWarnings(as.integer(mch))
  if (is.na(mch)) return(0L)
  mch
}
