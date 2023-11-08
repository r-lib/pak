
#' cli situation report
#'
#' Contains currently:
#' * `cli_unicode_option`: whether the `cli.unicode` option is set and its
#'   value. See [is_utf8_output()].
#' * `symbol_charset`: the selected character set for [symbol], UTF-8,
#'   Windows, or ASCII.
#' * `console_utf8`: whether the console supports UTF-8. See
#'   [base::l10n_info()].
#' * `latex_active`: whether we are inside knitr, creating a LaTeX
#'   document.
#' * `num_colors`: number of ANSI colors. See [num_ansi_colors()].
#' * `console_with`: detected console width.
#'
#' @return Named list with entries listed above. It has a `cli_sitrep`
#' class, with a `print()` and `format()` method.
#'
#' @export
#' @examples
#' cli_sitrep()

cli_sitrep <- function() {
  structure(
    list(
      cli_unicode_option = getOption("cli.unicode", NULL),
      symbol_charset = get_active_symbol_set(),
      console_utf8 = l10n_info()$`UTF-8`,
      latex_active = is_latex_output(),
      num_colors = num_ansi_colors(),
      console_width = console_width()),
    class = "cli_sitrep")
}

#' @export

print.cli_sitrep <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

get_active_symbol_set <- function() {
  if (identical(symbol, symbol_utf8)) {
    "UTF-8"
  } else {
    "ASCII (non UTF-8)"
  }
}

#' @export

format.cli_sitrep <- function(x, ...) {
  fmt_names <- format(names(x))
  fmt_vals  <- vapply(x, format, character(1))
  paste0("- ", fmt_names, " : ", fmt_vals)
}

#' @export

as.character.cli_sitrep <- function(x, ...) {
  "<cli sitrep>"
}
