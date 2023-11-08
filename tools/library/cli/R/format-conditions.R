
#' Format an error, warning or diagnostic message
#'
#' You can then throw this message with [stop()] or `rlang::abort()`.
#'
#' The messages can use inline styling, pluralization and glue
#' substitutions.
#'
#' ```{asciicast format-error}
#' n <- "boo"
#' stop(format_error(c(
#'         "{.var n} must be a numeric vector",
#'   "x" = "You've supplied a {.cls {class(n)}} vector."
#' )))
#' ```
#'
#' ```{asciicast format-error-2}
#' len <- 26
#' idx <- 100
#' stop(format_error(c(
#'         "Must index an existing element:",
#'   "i" = "There {?is/are} {len} element{?s}.",
#'   "x" = "You've tried to subset element {idx}."
#' )))
#' ```
#'
#' @param message It is formatted via a call to [cli_bullets()].
#' @param .envir Environment to evaluate the glue expressions in.
#'
#' @seealso These functions support [inline markup][inline-markup].
#' @family functions supporting inline markup
#' @export

format_error <- function(message, .envir = parent.frame()) {
  if (length(message) > 0 &&
      (is.null(names(message)) || names(message)[1] == "")) {
    # The default theme will make this bold
    names(message)[1] <- "1"
  }

  if (length(message) > 0) {
    message[1] <- paste0("Error: ", message[1])
  }

  rsconsole <- c("rstudio_console", "rstudio_console_starting")
  oldopt <- options(
    cli.width = getOption("cli.condition_width") %||% getOption("cli.width")
  )
  on.exit(options(oldopt), add =TRUE)

  # We need to create a frame here, so cli_div() is closed.
  # Cannot use local(), it does not work in snapshot tests, it potentially
  # has issues elsewhere as well.
  formatted1 <- cli_fmt((function() {
    cli_div(class = "cli_rlang cli_abort", theme = cnd_theme())
    cli_bullets(message, .envir = .envir)
  })(), collapse = TRUE, strip_newline = TRUE)

  # remove "Error: " that was only needed for the wrapping
  formatted1[1] <- sub("Error:[ ]?", "", formatted1[1])

  update_rstudio_color(formatted1)
}

#' @rdname format_error
#' @export

format_warning <- function(message, .envir = parent.frame()) {
  if (length(message) > 0 &&
      (is.null(names(message)) || names(message)[1] == "")) {
    # The default theme will make this bold
    names(message)[1] <- "1"
  }

  oldopt <- options(
    cli.width = getOption("cli.condition_width") %||% getOption("cli.width")
  )
  on.exit(options(oldopt), add = TRUE)

  formatted1 <- cli_fmt((function() {
    cli_div(class = "cli_rlang cli_warn", theme = cnd_theme())
    cli_bullets(message, .envir = .envir)
  })(), collapse = TRUE, strip_newline = TRUE)

  update_rstudio_color(formatted1)
}

#' @rdname format_error
#' @export

format_message <- function(message, .envir = parent.frame()) {
  oldopt <- options(
    cli.width = getOption("cli.condition_width") %||% getOption("cli.width")
  )
  on.exit(options(oldopt), add = TRUE)
  formatted1 <- cli_fmt((function() {
    cli_div(class = "cli_rlang cli_inform", theme = cnd_theme())
    cli_bullets(message, .envir = .envir)
  })(), collapse = TRUE, strip_newline = TRUE)
  update_rstudio_color(formatted1)
}

update_rstudio_color <- function(message) {
  rscol <- get_rstudio_fg_color()
  if (!is.null(rscol)) {
    # in RStudio we only need to change the color
    message[] <- rscol(message)
  } else {
    # in a terminal we need to undo the bold
    message <- paste0(style_bold(""), message)
  }
  message
}

get_rstudio_fg_color <- function() {
  tryCatch(
    get_rstudio_fg_color0(),
    error = function(e) NULL
  )
}

get_rstudio_fg_color0 <- function() {
  rs <- rstudio_detect()
  oktypes <- c("rstudio_console", "rstudio_console_starting")
  if (! rs$type %in% oktypes) return(NULL)
  if (rs$num_colors == 1) return(NULL)
  colstr <- get_rstudio_theme()$foreground
  if (is.null(colstr)) return(NULL)
  colstr0 <- substr(colstr, 5, nchar(colstr) - 1)
  rgbnum <- scan(text = colstr0, sep = ",", quiet = TRUE)
  rgb <- grDevices::rgb(rgbnum[1]/255, rgbnum[2]/255, rgbnum[3]/255)
  make_ansi_style(rgb)
}

rstudio_detect <- function() {
  rstudio$detect()
}

cnd_theme <- function() {
  list(
    ".cli_rlang .bullets .bullet-v" = list(
      before = function(x) paste0(col_green(cnd_symb("tick")), " ")
    ),
    ".bullets .bullet-x" = list(
      before = function(x) paste0(col_red(cnd_symb("cross")), " ")
    ),
    ".bullets .bullet-i" = list(
      before = function(x) paste0(col_cyan(cnd_symb("info")), " ")
    ),
    ".bullets .bullet-*" = list(
      before = function(x) paste0(col_cyan(cnd_symb("bullet")), " ")
    ),
    ".bullets .bullet->" = list(
      before = function(x) paste0(cnd_symb("arrow_right"), " ")
    )
  )
}

cnd_symb <- function(name) {
  opt <- getOption("cli.condition_unicode_bullets", NULL)
  if (isTRUE(opt)) {
    symbol_utf8[[name]]
  } else if (isFALSE(opt)) {
    symbol_ascii[[name]]
  } else {
    symbol[[name]]
  }
}
