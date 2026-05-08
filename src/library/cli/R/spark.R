# from pillar

#' Draw a sparkline bar graph with unicode block characters
#'
#' @description
#' Rendered using [block elements](https://en.wikipedia.org/wiki/Block_Elements).
#' In most common fixed width fonts these are rendered wider than regular
#' characters which means they are not suitable if you need precise alignment.
#'
#' You might want to avoid sparklines on non-UTF-8 systems, because they
#' do not look good. You can use [is_utf8_output()] to test for support
#' for them.
#'
#' @details
#'
#' ```{asciicast spark-bar-1}
#' x <- seq(0, 1, length = 6)
#' spark_bar(x)
#' ```
#'
#' ```{asciicast spark-bar-2}
#' x <- seq(0, 1, length = 6)
#' spark_bar(sample(x))
#' ```
#'
#' ```{asciicast spark-bar-3}
#' spark_bar(seq(0, 1, length = 8))
#' ```
#'
#' `NA`s are left out:
#'
#' ```{asciicast spark-bar-na}
#' spark_bar(c(0, NA, 0.5, NA, 1))
#' ```
#'
#' @param x A numeric vector between 0 and 1
#' @export
#' @seealso [spark_line()]

spark_bar <- function(x) {
  stopifnot(is.numeric(x))

  chars <- spark_bar_chars(x)

  structure(
    paste0(chars, collapse = ""),
    class = c("cli_spark_bar", "cli_spark")
  )
}

#' @export

format.cli_spark <- function(x, ...) {
  unclass(x)
}

#' @export

print.cli_spark <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

spark_bar_chars <- function(x, bars = NULL) {
  if (is.null(bars)) {
    if (is_utf8_output()) {
      bars <- vapply(0x2581:0x2588, intToUtf8, character(1))
    } else {
      bars <- c("_", ",", "*", "#")
    }
  }

  factor <- cut(
    x,
    breaks = seq(0, 1, length.out = length(bars) + 1),
    labels = bars,
    include.lowest = TRUE
  )
  chars <- as.character(factor)
  chars[is.na(chars)] <- " "

  chars
}

#' Draw a sparkline line graph with Braille characters.
#'
#' You might want to avoid sparklines on non-UTF-8 systems, because they
#' do not look good. You can use [is_utf8_output()] to test for support
#' for them.
#'
#' @details
#'
#' ```{asciicast spark-line}
#' x <- seq(0, 1, length = 10)
#' spark_line(x)
#' ```
#'
#' @inheritParams spark_bar
#' @export
#' @seealso [spark_bar()]

spark_line <- function(x) {
  stopifnot(is.numeric(x))

  if (length(x) %% 2 == 1) {
    x <- c(x, NA)
  }

  if (is_utf8_output()) {
    y <- findInterval(x, seq(0, 1, length.out = 5), all.inside = TRUE)
    ind <- matrix(y, ncol = 2, byrow = TRUE)
    ind[, 2] <- ind[, 2] + 4
    chars <- apply(ind, 1, braille)
  } else {
    ind <- matrix(x, ncol = 2, byrow = TRUE)
    bars <- c("_", ",", "-", "^")
    chars <- spark_bar_chars(apply(ind, 1, mean), bars)
  }

  structure(
    paste0(chars, collapse = ""),
    class = c("cli_spark_line", "cli_spark")
  )
}

# https://en.wikipedia.org/wiki/Braille_Patterns
braille <- function(x) {
  # remap to braille sequence
  x <- c(7L, 3L, 2L, 1L, 8L, 6L, 5L, 4L)[x]

  raised <- 1:8 %in% x
  binary <- raised * 2^(0:7)

  # offset in hex is 2800
  val <- 10240 + sum(raised * 2^(0:7))

  intToUtf8(val)
}
