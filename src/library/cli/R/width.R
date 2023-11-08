
#' Determine the width of the console
#'
#' It uses the `cli.width` option, if set. Otherwise it tries to
#' determine the size of the terminal or console window.
#'
#' These are the exact rules:
#' * If the `cli.width` option is set to a positive integer, it is used.
#' * If the `cli.width` option is set, but it is not a positive integer,
#'   and error is thrown.
#'
#' Then we try to determine the size of the terminal or console window:
#' * If we are not in RStudio, or we are in an RStudio terminal,
#'   then we try to use the `tty_size()` function to query the
#'   terminal size. This might fail if R is not running in a terminal,
#'   but failures are ignored.
#' * If we are in the RStudio build pane, then the `RSTUDIO_CONSOLE_WIDTH`
#'   environment variable is used. If the build pane is resized, then this
#'   environment variable is not accurate any more, and the output might
#'   get garbled.
#' * We are _not_ using the `RSTUDIO_CONSOLE_WIDTH` environment variable
#'   if we are in the RStudio console.
#'
#' If we cannot determine the size of the terminal or console window, then
#' we use the `width` option. If the `width` option is not set, then
#' we return 80L.
#'
#' @return Integer scalar, the console with, in number of characters.
#'
#' @export
#' @examples
#' console_width()

console_width <- function() {
  # cli.width option always takes priotity
  cwopt <- getOption("cli.width")
  if (!is.null(cwopt)) {
    if (!is.numeric(cwopt) || length(cwopt) != 1) {
      opt <- options(cli.width = 60)
      on.exit(options(opt), add = TRUE)
      throw(cli_error(
        "{.code options(\"cli.width\")} must be an integer scalar.",
        "i" = "{.code options(\"cli.width\")} is {.type {cwopt}}."
      ))
    }
    if (is.na(cwopt)) {
      opt <- options(cli.width = 60)
      on.exit(options(opt), add = TRUE)
      throw(cli_error("{.code options(\"cli.width\")} cannot be {.code NA}."))
    }
    if (cwopt == Inf) {
      cwopti <- .Machine$integer.max
    } else {
      cwopti <- as.integer(cwopt)
    }
    if (cwopti <= 0) {
      opt <- options(cli.width = 60)
      on.exit(options(opt), add = TRUE)
      throw(cli_error(
        "{.code options(\"cli.width\")} must be a positive integer and not {.val {cwopti}}."
      ))
    }
    return(cwopti)
  }

  # detect if in RStudio
  rs <- rstudio$detect()
  if (rs$type == "not_rstudio") {
    # maybe a terminal?
    width <- terminal_width()

  } else if (rs$type == "rstudio_console_starting") {
    # there isn't much we can do here, options and env vars are not set
    width <- NULL

  } else if (rs$type == "rstudio_console") {
    # will just use getOption("width"), in case the user changed it,
    # and ignore the RSTUDIO_CONSOLE_WIDTH env var
    width <- NULL

  } else if (rs$type == "rstudio_build_pane") {
    # RStudio explicitly sets this for build pane processes
    # It is only good when the build starts, but we cannot do better
    width <- rs_console_width()

  } else if (rs$type == "rstudio_terminal") {
    # Can also be a subprocess of the terminal, with a pty,
    # but that's fine, the pty should have a width set up.
    # We do not fall back to the RSTUDIO_CONSOLE_WIDTH env var,
    # because the user might have changed options("width") and the env
    # var is only good when the terminal starts, anyway.
    width <- terminal_width()

  } else { # rstudio_subprocess
    width <- NULL
  }

  # If not set, then use the option
  width <- width %||% getOption("width") %||% 80L

  width
}

tty_size <- function() {
  ret <- .Call(clic_tty_size)
  c(width = ret[1], height = ret[2])
}

terminal_width <- function() {
  if (isTRUE(clienv$notaconsole)) return(NULL)
  w <- tryCatch(
    tty_size()[["width"]],
    error = function(e) {
      clienv$notaconsole <- TRUE
      NULL
    }
  )

  # this is probably a pty that does not set the width, use st sensible
  if (!is.null(w) && w == 0) w <- 80L
  w
}

rs_console_width <- function() {
  ev <- as.integer(Sys.getenv("RSTUDIO_CONSOLE_WIDTH", ""))[1]
  if (!is.na(ev)) ev else NULL
}
