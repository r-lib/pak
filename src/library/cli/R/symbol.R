#' Various handy symbols to use in a command line UI
#'
#' @usage
#' symbol
#'
#' @format A named list, see \code{names(symbol)} for all sign names.
#'
#' @details
#'
#' On Windows they have a fallback to less fancy symbols.
#'
#' `list_symbols()` prints a table with all symbols to the screen.
#'
#' @name symbol
#' @aliases symbol
#' @export symbol
#'
#' @examples
#' cat(symbol$tick, " SUCCESS\n", symbol$cross, " FAILURE\n", sep = "")
#'
#' ## All symbols
#' cat(paste(format(names(symbol), width = 20),
#'   unlist(symbol)), sep = "\n")
NULL

symbol_utf8 <- list(
  "tick" = '\u2714',
  "cross" = '\u2716',
  "star" = '\u2605',
  "square" = '\u2587',
  "square_small" = '\u25FB',
  "square_small_filled" = '\u25FC',
  "circle" = '\u25EF',
  "circle_filled" = '\u25C9',
  "circle_dotted" = '\u25CC',
  "circle_double" = '\u25CE',
  "circle_circle" = '\u24DE',
  "circle_cross" = '\u24E7',
  "circle_pipe" = '\u24be',
  "circle_question_mark" = '?\u20DD',
  "bullet" = '\u2022',
  "dot" = '\u2024',
  "line" = '\u2500',
  "double_line" = "\u2550",
  "ellipsis" = '\u2026',
  "continue" = '\u2026',
  "pointer" = '\u276F',
  "info" = '\u2139',
  "warning" = '\u26A0',
  "menu" = '\u2630',
  "smiley" = '\u263A',
  "mustache" = '\u0DF4',
  "heart" = '\u2665',
  "arrow_up" = '\u2191',
  "arrow_down" = '\u2193',
  "arrow_left" = '\u2190',
  "arrow_right" = '\u2192',
  "radio_on" = '\u25C9',
  "radio_off" = '\u25EF',
  "checkbox_on" = '\u2612',
  "checkbox_off" = '\u2610',
  "checkbox_circle_on" = '\u24E7',
  "checkbox_circle_off" = '\u24BE',
  "fancy_question_mark" = '\u2753',
  "neq" = "\u2260",
  "geq" = "\u2265",
  "leq" = "\u2264",
  "times" = "\u00d7",

  "upper_block_1" = "\u2594",
  "upper_block_4" = "\u2580",

  "lower_block_1" = "\u2581",
  "lower_block_2" = "\u2582",
  "lower_block_3" = "\u2583",
  "lower_block_4" = "\u2584",
  "lower_block_5" = "\u2585",
  "lower_block_6" = "\u2586",
  "lower_block_7" = "\u2587",
  "lower_block_8" = "\u2588",

  "full_block" = "\u2588",

  "sup_0" = "\u2070",
  "sup_1" = "\u00b9",
  "sup_2" = "\u00b2",
  "sup_3" = "\u00b3",
  "sup_4" = "\u2074",
  "sup_5" = "\u2075",
  "sup_6" = "\u2076",
  "sup_7" = "\u2077",
  "sup_8" = "\u2078",
  "sup_9" = "\u2079",

  "sup_minus" = "\u207b",
  "sup_plus" = "\u207a",

  "play" = "\u25b6",
  "stop" = "\u25a0",
  "record" = "\u25cf",

  "figure_dash" = "\u2012",
  "en_dash" = "\u2013",
  "em_dash" = "\u2014",

  "dquote_left" = "\u201c",
  "dquote_right" = "\u201d",
  "squote_left" = "\u2018",
  "squote_right" = "\u2019"
)

symbol_ascii <- list(
  "tick" = 'v',
  "cross" = 'x',
  "star" = '*',
  "square" = '[ ]',
  "square_small" = '[ ]',
  "square_small_filled" = '[x]',
  "circle" = '( )',
  "circle_filled" = '(*)',
  "circle_dotted" = '( )',
  "circle_double" = '(o)',
  "circle_circle" = '(o)',
  "circle_cross" = '(x)',
  "circle_pipe" = '(|)',
  "circle_question_mark" = '(?)',
  "bullet" = '*',
  "dot" = '.',
  "line" = '-',
  "double_line" = "=",
  "ellipsis" = '...',
  "continue" = '~',
  "pointer" = '>',
  "info" = 'i',
  "warning" = '!',
  "menu" = '=',
  "smiley" = ':)',
  "mustache" = '/\\/',
  "heart" = '<3',
  "arrow_up" = '^',
  "arrow_down" = 'v',
  "arrow_left" = '<',
  "arrow_right" = '>',
  "radio_on" = '(*)',
  "radio_off" = '( )',
  "checkbox_on" = '[x]',
  "checkbox_off" = '[ ]',
  "checkbox_circle_on" = '(x)',
  "checkbox_circle_off" = '( )',
  "fancy_question_mark" = "(?)",
  "neq" = "!=",
  "geq" = ">=",
  "leq" = "<=",
  "times" = "x",

  "upper_block_1" = "^",
  "upper_block_4" = "^",

  "lower_block_1" = ".",
  "lower_block_2" = "_",
  "lower_block_3" = "_",
  "lower_block_4" = "=",
  "lower_block_5" = "=",
  "lower_block_6" = "*",
  "lower_block_7" = "#",
  "lower_block_8" = "#",

  "full_block" = "#",

  "sup_0" = "0",
  "sup_1" = "1",
  "sup_2" = "2",
  "sup_3" = "3",
  "sup_4" = "4",
  "sup_5" = "5",
  "sup_6" = "6",
  "sup_7" = "7",
  "sup_8" = "8",
  "sup_9" = "9",

  "sup_minus" = "-",
  "sup_plus" = "+",

  "play" = ">",
  "stop" = "#",
  "record" = "o",

  "figure_dash" = "-",
  "en_dash" = "--",
  "em_dash" = "---",

  "dquote_left" = "\"",
  "dquote_right" = "\"",
  "squote_left" = "'",
  "squote_right" = "'"
)

#' @export
#' @rdname symbol

list_symbols <- function() {
  rpad <- function(x, width) {
    w <- nchar(x, type = "width")
    paste0(x, strrep(" ", width - w))
  }
  chars <- rpad(paste0(symbol, "\t", names(symbol)), 25)
  if (length(chars) %% 2) chars <- c(chars, "")
  chars <- paste(
    sep = "   ",
    chars[1:(length(chars) / 2)],
    chars[(length(chars) / 2 + 1):length(chars)]
  )
  cat(chars, sep = "\n")
}
