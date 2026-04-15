#' Whether cli is emitting UTF-8 characters
#'
#' UTF-8 cli characters can be turned on by setting the `cli.unicode`
#' option to `TRUE`. They can be turned off by setting if to `FALSE`.
#' If this option is not set, then [base::l10n_info()] is used to detect
#' UTF-8 support.
#'
#' @return Flag, whether cli uses UTF-8 characters.
#'
#' @export

is_utf8_output <- function() {
  opt <- getOption("cli.unicode", NULL)
  if (!is.null(opt)) {
    isTRUE(opt)
  } else {
    l10n_info()$`UTF-8` && !is_latex_output()
  }
}

#' Count the number of characters in a character vector
#'
#' By default it counts Unicode grapheme clusters, instead of code points.
#'
#' @param x Character vector, it is converted to UTF-8.
#' @param type Whether to count graphemes (characters), code points,
#'   bytes, or calculate the display width of the string.
#' @return Numeric vector, the length of the strings in the character
#'   vector.
#'
#' @family UTF-8 string manipulation
#' @export
#' @examples
#' # Grapheme example, emoji with combining characters. This is a single
#' # grapheme, consisting of five Unicode code points:
#' # * `\U0001f477` is the construction worker emoji
#' # * `\U0001f3fb` is emoji modifier that changes the skin color
#' # * `\u200d` is the zero width joiner
#' # * `\u2640` is the female sign
#' # * `\ufe0f` is variation selector 16, requesting an emoji style glyph
#' emo <- "\U0001f477\U0001f3fb\u200d\u2640\ufe0f"
#' cat(emo)
#'
#' utf8_nchar(emo, "chars") # = graphemes
#' utf8_nchar(emo, "bytes")
#' utf8_nchar(emo, "width")
#' utf8_nchar(emo, "codepoints")
#'
#' # For comparison, the output for width depends on the R version used:
#' nchar(emo, "chars")
#' nchar(emo, "bytes")
#' nchar(emo, "width")

utf8_nchar <- function(
  x,
  type = c("chars", "bytes", "width", "graphemes", "codepoints")
) {
  type <- match.arg(type)
  if (type == "chars") type <- "graphemes"

  x <- enc2utf8(x)

  if (type == "width") {
    .Call(clic_utf8_display_width, x)
  } else if (type == "graphemes") {
    .Call(clic_utf8_nchar_graphemes, x)
  } else if (type == "codepoints") {
    base::nchar(x, allowNA = FALSE, keepNA = TRUE, type = "chars")
  } else {
    # bytes
    base::nchar(x, allowNA = FALSE, keepNA = TRUE, type = "bytes")
  }
}

#' Substring of an UTF-8 string
#'
#' This function uses grapheme clusters instead of Unicode code points in
#' UTF-8 strings.
#'
#' @param x Character vector.
#' @param start Starting index or indices, recycled to match the length
#'   of `x`.
#' @param stop Ending index or indices, recycled to match the length of
#'   `x`.
#' @return Character vector of the same length as `x`, containing
#'   the requested substrings.
#'
#' @family UTF-8 string manipulation
#' @export
#' @examples
#' # Five grapheme clusters, select the middle three
#' str <- paste0(
#'   "\U0001f477\U0001f3ff\u200d\u2640\ufe0f",
#'   "\U0001f477\U0001f3ff",
#'   "\U0001f477\u200d\u2640\ufe0f",
#'   "\U0001f477\U0001f3fb",
#'   "\U0001f477\U0001f3ff")
#' cat(str)
#' str24 <- utf8_substr(str, 2, 4)
#' cat(str24)

utf8_substr <- function(x, start, stop) {
  if (!is.character(x)) x <- as.character(x)
  if (!is.numeric(start) || !is.numeric(stop)) {
    throw(cli_error(
      "{.arg start} and {.arg stop} must be numeric vectors",
      "i" = if (!is.numeric(start)) "{.arg start} is {.typeof {start}}",
      "i" = if (!is.numeric(stop)) "{.arg stop}  is {.typeof {stop}}"
    ))
  }
  start2 <- suppressWarnings(as.integer(start))
  stop2 <- suppressWarnings(as.integer(stop))
  if (!length(start2) || !length(stop2)) {
    throw(cli_error(
      "{.arg start} and {.arg stop} must have at least length 1",
      "i" = if (!length(start2)) "{.arg start} has length 0",
      "i" = if (!length(stop2)) "{.arg stop}  has length 0"
    ))
  }
  x <- enc2utf8(x)

  # TODO: better recycling
  start2 <- rep_len(start2, length(x))
  stop2 <- rep_len(stop2, length(x))
  .Call(clic_utf8_substr, x, start2, stop2)
}

#' Break an UTF-8 character vector into grapheme clusters
#'
#' @param x Character vector.
#' @return List of characters vectors, the grapheme clusters of the input
#'   string.
#'
#' @family UTF-8 string manipulation
#' @export
#' @examples
#' # Five grapheme clusters
#' str <- paste0(
#'   "\U0001f477\U0001f3ff\u200d\u2640\ufe0f",
#'   "\U0001f477\U0001f3ff",
#'   "\U0001f477\u200d\u2640\ufe0f",
#'   "\U0001f477\U0001f3fb",
#'   "\U0001f477\U0001f3ff")
#' cat(str, "\n")
#' chrs <- utf8_graphemes(str)

utf8_graphemes <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  x <- enc2utf8(x)
  .Call(clic_utf8_graphemes, x)
}
