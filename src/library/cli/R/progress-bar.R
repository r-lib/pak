
make_progress_bar <- function(percent, width = 30, style = list()) {
  complete_len <- round(width * percent)

  def <- default_progress_style()
  chr_complete <- style[["progress-complete"]] %||% def[["complete"]]
  chr_incomplete <- style[["progress-incomplete"]] %||% def[["incomplete"]]
  chr_current <- style[["progress-current"]] %||% def[["current"]]

  complete <- paste(rep(chr_complete, complete_len), collapse = "")
  current <- if (percent == 100) chr_complete else chr_current
  incomplete <- paste(rep(chr_incomplete, width - complete_len), collapse = "")
  paste0(complete, current, incomplete, " ")
}

default_progress_style <- function() {
  opt <- progress_style(getOption("cli.progress_bar_style"))
  if (is_utf8_output()) {
    opu <- progress_style(getOption("cli.progress_bar_style_unicode"))
    list(
      complete = opu$complete %||% opt$complete %||% "\u25A0",
      current = opu$current %||% opt$current %||% opu$complete %||%
        opt$complete %||% "\u25A0",
      incomplete = opu$incomplete %||% opt$incomplete %||% "\u00a0"
    )
  } else {
    opa <- progress_style(getOption("cli.progress_bar_style_ascii"))
    list(
      complete = opa$complete %||% opt$complete %||% "=",
      current = opa$current %||% opt$current %||% opa$complete %||%
        opt$complete %||% ">",
      incomplete = opa$incomplete %||% opt$incomplete %||% "-"
    )
  }
}

progress_style <- function(x) {
  if (is.null(x)) return(x)
  if (is_string(x)) return(cli_progress_styles()[[x]])
  x
}

#' List of built-in cli progress styles
#'
#' The following options are used to select a style:
#' * `cli_progress_bar_style`
#' * `cli_progress_bar_style_ascii`
#' * `cli_progress_bar_style_unicode`
#'
#' On Unicode terminals (if [is_utf8_output()] is `TRUE`), the
#' `cli_progress_bar_style_unicode` and `cli_progress_bar_style`
#' options are used.
#'
#' On ASCII terminals (if [is_utf8_output()] is `FALSE`), the
#' `cli_pgoress_bar_style_ascii` and `cli_progress_bar_style` options
#' are are used.
#'
#' ```{asciicast progress-style}
#' for (style in names(cli_progress_styles())) {
#'   options(cli.progress_bar_style = style)
#'   label <- ansi_align(paste0("Style '", style, "'"), 20)
#'   print(cli_progress_demo(label, live = FALSE, at = 66, total = 100))
#' }
#' options(cli.progress_var_style = NULL)
#' ```
#'
#' @return A named list with sublists containing elements
#' `complete`, `incomplete` and potentially `current`.
#'
#' @family progress bar functions
#' @export

cli_progress_styles <- function() {
  list(
    classic = list(
      complete = "#",
      incomplete = "\u00a0"
    ),
    squares = list(
      complete = "\u25a0",
      incomplete = "\u00a0"
    ),
    dot = list(
      complete = col_grey("\u2500"),
      incomplete = col_grey("\u2500"),
      current = col_red(symbol$record)
    ),
    fillsquares = list(
      complete = "\u25a0",
      incomplete = col_grey("\u25a1")
    ),
    bar = list(
      complete = "\u2588",
      incomplete = col_grey("\u2588")
    )
  )
}
