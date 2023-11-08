
#' Working around the bad Unicode character widths
#'
#' R 3.6.2 and also the coming 3.6.3 and 4.0.0 versions use the Unicode 8
#' standard to calculate the display width of Unicode characters.
#' Unfortunately the widths of most emojis are incorrect in this standard,
#' and width 1 is reported instead of the correct 2 value.
#'
#' cli implements a workaround for this. The package contains a table that
#' contains all Unicode ranges that have wide characters (display width 2).
#'
#' On first use of one of the workaround wrappers (in `ansi_nchar()`, etc.)
#' we check what the current version of R thinks about the width of these
#' characters, and then create a regex that matches the ones that R
#' is wrong about (`re_bad_char_width`).
#'
#' Then we use this regex to duplicate all of the problematic characters
#' in the input string to the wrapper function, before calling the real
#' string manipulation function (`nchar()`, `strwrap()`) etc. At end we
#' undo the duplication before we return the result.
#'
#' This workaround is fine for `nchar()` and `strwrap()`, and consequently
#' `ansi_align()` and `ansi_strtrim()` as well.
#'
#' The rest of the `ansi_*()` functions work on characters, and do not
#' deal with character width.
#'
#' @keywords internal
#' @name unicode-width-workaround
NULL

setup_unicode_width_fix <- function() {
  bad <- base::nchar(wide_chars$test, type = "width") == 1
  re <- paste0(wide_chars$regex[bad], collapse = "")
  clienv$re_bad_char_width <- paste0("([", re, "])")
  clienv$re_bad_char_width_fix <- paste0("([", re, "])\\1")
}

unicode_pre <- function(x) {
  if (is.null(clienv$re_bad_char_width)) setup_unicode_width_fix()
  if (clienv$re_bad_char_width != "([])") {
    x <- gsub(clienv$re_bad_char_width, "\\1\\1", x, perl = TRUE)
  }
  x
}

unicode_post <- function(x) {
  if (is.null(clienv$re_bad_char_width)) setup_unicode_width_fix()
  if (clienv$re_bad_char_width != "([])") {
    x <- gsub(clienv$re_bad_char_width_fix, "\\1", x, perl = TRUE)
  }
  x
}
