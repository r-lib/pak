
## Create a mapping between the string and its style-less version.
## This is useful to work with the colored string.

#' @importFrom utils tail

map_to_ansi <- function(x, text = NULL) {

  if (is.null(text)) {
    text <- non_matching(re_table(ansi_regex, x), x, empty=TRUE)
  }

  map <- lapply(
    text,
    function(text) {
      cbind(
        pos = cumsum(c(1, text[, "length"], Inf)),
        offset = c(text[, "start"] - 1, tail(text[, "end"], 1), NA)
      )
    })

  function(pos) {
    pos <- rep(pos, length.out = length(map))
    mapply(pos, map, FUN = function(pos, table) {
      if (pos < 1) {
        pos
      } else {
        slot <- which(pos < table[, "pos"])[1] - 1
        table[slot, "offset"] + pos - table[slot, "pos"] + 1
      }
    })
  }
}


#' Count number of characters in an ANSI colored string
#'
#' This is a color-aware counterpart of [base::nchar()],
#' which does not do well, since it also counts the ANSI control
#' characters.
#'
#' @param x Character vector, potentially ANSO styled, or a vector to be
#'   coarced to character.
#' @param ... Additional arguments, passed on to `base::nchar()`
#'   after removing ANSI escape sequences.
#' @return Numeric vector, the length of the strings in the character
#'   vector.
#'
#' @family ANSI string operations
#' @export
#' @examples
#' str <- paste(
#'   red("red"),
#'   "default",
#'   green("green")
#' )
#'
#' cat(str, "\n")
#' nchar(str)
#' col_nchar(str)
#' nchar(strip_style(str))

col_nchar <- function(x, ...) {
  base::nchar(strip_style(x), ...)
}


#' Substring(s) of an ANSI colored string
#'
#' This is a color-aware counterpart of [base::substr()].
#' It works exactly like the original, but keeps the colors
#' in the substrings. The ANSI escape sequences are ignored when
#' calculating the positions within the string.
#'
#' @param x Character vector, potentially ANSI styled, or a vector to
#'   coarced to character.
#' @param start Starting index or indices, recycled to match the length
#'   of `x`.
#' @param stop Ending index or indices, recycled to match the length
#'   of `x`.
#' @return Character vector of the same length as `x`, containing
#'   the requested substrings. ANSI styles are retained.
#'
#' @family ANSI string operations
#' @export
#' @examples
#' str <- paste(
#'   red("red"),
#'   "default",
#'   green("green")
#' )
#'
#' cat(str, "\n")
#' cat(col_substr(str, 1, 5), "\n")
#' cat(col_substr(str, 1, 15), "\n")
#' cat(col_substr(str, 3, 7), "\n")
#'
#' substr(strip_style(str), 1, 5)
#' substr(strip_style(str), 1, 15)
#' substr(strip_style(str), 3, 7)
#'
#' str2 <- "another " %+%
#'   red("multi-", sep = "", underline("style")) %+%
#'   " text"
#'
#' cat(str2, "\n")
#' cat(col_substr(c(str, str2), c(3,5), c(7, 18)), sep = "\n")
#' substr(strip_style(c(str, str2)), c(3,5), c(7, 18))

col_substr <- function(x, start, stop) {
  if(!is.character(x)) x <- as.character(x)
  if(!length(x)) return(x)
  start <- as.integer(start)
  stop <- as.integer(stop)
  if(!length(start) || !length(stop))
    stop("invalid substring arguments")
  if(anyNA(start) || anyNA(stop))
    stop("non-numeric substring arguments not supported")
  ansi <- re_table(ansi_regex, x)
  text <- non_matching(ansi, x, empty=TRUE)
  mapper <- map_to_ansi(x, text = text)
  nstart <- mapper(start)
  nstop  <- mapper(stop)

  bef <- base::substr(x, 1, nstart - 1)
  aft <- base::substr(x, nstop + 1, base::nchar(x))
  ansi_bef <- vapply(regmatches(bef, gregexpr(ansi_regex, bef)),
                     paste, collapse = "", FUN.VALUE = "")
  ansi_aft <- vapply(regmatches(aft, gregexpr(ansi_regex, aft)),
                     paste, collapse = "", FUN.VALUE = "")

  paste(sep = "", ansi_bef, base::substr(x, nstart, nstop), ansi_aft)
}

#' Substring(s) of an ANSI colored string
#'
#' This is the color-aware counterpart of [base::substring()].
#' It works exactly like the original, but keeps the colors in the
#' substrings. The ANSI escape sequences are ignored when
#' calculating the positions within the string.
#'
#' @param text Character vector, potentially ANSI styled, or a vector to
#'   coarced to character. It is recycled to the longest of `first`
#'   and `last`.
#' @param first Starting index or indices, recycled to match the length
#'   of `x`.
#' @param last Ending index or indices, recycled to match the length
#'   of `x`.
#' @return Character vector of the same length as `x`, containing
#'   the requested substrings. ANSI styles are retained.
#'
#' @family ANSI string operations
#' @export
#' @examples
#' str <- paste(
#'   red("red"),
#'   "default",
#'   green("green")
#' )
#'
#' cat(str, "\n")
#' cat(col_substring(str, 1, 5), "\n")
#' cat(col_substring(str, 1, 15), "\n")
#' cat(col_substring(str, 3, 7), "\n")
#'
#' substring(strip_style(str), 1, 5)
#' substring(strip_style(str), 1, 15)
#' substring(strip_style(str), 3, 7)
#'
#' str2 <- "another " %+%
#'   red("multi-", sep = "", underline("style")) %+%
#'   " text"
#'
#' cat(str2, "\n")
#' cat(col_substring(str2, c(3,5), c(7, 18)), sep = "\n")
#' substring(strip_style(str2), c(3,5), c(7, 18))

col_substring <- function(text, first, last = 1000000L) {
  if (!is.character(text)) text <- as.character(text)
  n <- max(lt <- length(text), length(first), length(last))
  if (lt && lt < n) text <- rep_len(text, length.out = n)
  col_substr(text, as.integer(first), as.integer(last))
}


#' Split an ANSI colored string
#'
#' This is the color-aware counterpart of [base::strsplit()].
#' It works almost exactly like the original, but keeps the colors in the
#' substrings.
#'
#' @param x Character vector, potentially ANSI styled, or a vector to
#'   coarced to character.
#' @param split Character vector of length 1 (or object which can be coerced to
#'   such) containing regular expression(s) (unless `fixed = TRUE`) to use
#'   for splitting.  If empty matches occur, in particular if `split` has
#'   zero characters, `x` is split into single characters.
#' @param ... Extra arguments are passed to `base::strsplit()`.
#' @return A list of the same length as `x`, the \eqn{i}-th element of
#'   which contains the vector of splits of `x[i]`. ANSI styles are
#'   retained.
#'
#' @family ANSI string operations
#' @export
#' @importFrom utils head
#' @examples
#' str <- red("I am red---") %+%
#'   green("and I am green-") %+%
#'   underline("I underlined")
#'
#' cat(str, "\n")
#'
#' # split at dashes, keep color
#' cat(col_strsplit(str, "[-]+")[[1]], sep = "\n")
#' strsplit(strip_style(str), "[-]+")
#'
#' # split to characters, keep color
#' cat(col_strsplit(str, "")[[1]], "\n", sep = " ")
#' strsplit(strip_style(str), "")

col_strsplit <- function(x, split, ...) {
  split <- try(as.character(split), silent=TRUE)
  if(inherits(split, "try-error") || !is.character(split) || length(split) > 1L)
    stop("`split` must be character of length <= 1, or must coerce to that")
  if(!length(split)) split <- ""
  plain <- strip_style(x)
  splits <- re_table(split, plain, ...)
  chunks <- non_matching(splits, plain, empty = TRUE)
  # silently recycle `split`; doesn't matter currently since we don't support
  # split longer than 1, but might in future
  split.r <- rep(split, length.out=length(x))
  # Drop empty chunks to align with `substr` behavior
  chunks <- lapply(
    seq_along(chunks),
    function(i) {
      y <- chunks[[i]]
      # empty split means drop empty first match
      if(nrow(y) && !nzchar(split.r[[i]]) && !head(y, 1L)[, "length"]) {
        y <- y[-1L, , drop=FALSE]
      }
      # drop empty last matches
      if(nrow(y) && !tail(y, 1L)[, "length"]) y[-nrow(y), , drop=FALSE] else y
    }
  )
  zero.chunks <- !vapply(chunks, nrow, integer(1L))
  # Pull out zero chunks from colored string b/c col_substring won't work
  # with them
  res <- vector("list", length(chunks))
  res[zero.chunks] <- list(character(0L))
  res[!zero.chunks] <- mapply(
    chunks[!zero.chunks], x[!zero.chunks], SIMPLIFY = FALSE,
    FUN = function(tab, xx) col_substring(xx, tab[, "start"], tab[, "end"])
  )
  res
}

#' Align an ANSI colored string
#'
#' @param text The character vector to align.
#' @param width Width of the field to align in.
#' @param align Whether to align `"left"`, `"center"` or `"right"`.
#' @param type Passed on to [col_nchar()] and there to [nchar()]
#' @return The aligned character vector.
#'
#' @family ANSI string operations
#' @export
#' @examples
#' col_align(red("foobar"), 20, "left")
#' col_align(red("foobar"), 20, "center")
#' col_align(red("foobar"), 20, "right")

col_align <- function(text, width = getOption("width"),
                      align = c("left", "center", "right"),
                      type = "width") {

  align <- match.arg(align)
  nc <- col_nchar(text, type = type)

  if (!length(text)) return(text)

  if (align == "left") {
    paste0(text, make_space(width - nc))

  } else if (align == "center") {
    paste0(make_space(ceiling((width - nc) / 2)),
           text,
           make_space(floor((width - nc) / 2)))

  } else {
    paste0(make_space(width - nc), text)
  }
}

make_space <- function(num, filling = " ") {
  num <- pmax(0, num)
  res <- strrep(filling, num)
  Encoding(res) <- Encoding(filling)
  res
}

strrep <- function (x, times) {
  x = as.character(x)
  if (length(x) == 0L) return(x)

  mapply(
    function(x, times) {
      if (is.na(x) || is.na(times)) {
        NA_character_
      } else if (times <= 0L) {
        ""
      } else {
        paste0(rep(x, times), collapse = "")
      }
    },
    x, times,
    USE.NAMES = FALSE
  )
}
