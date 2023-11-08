
# Compared to glue::glue(), these are fixed:
# - .sep = ""
# - .trim = TRUE
# - .null = character()
# - .literal = TRUE
# - .comment = ""
#
# we also don't allow passing in data as arguments, and `text` is
# a single argument, no need to `paste()` etc.

glue <- function(text, .envir = parent.frame(),
                 .transformer = identity_transformer,
                 .open = "{", .close = "}", .cli = FALSE, .trim = TRUE) {

  text <- paste0(text, collapse = "")

  if (length(text) < 1) {
    return(text)
  }

  if (is.na(text)) {
    return(text)
  }

  if (.trim) {
    text <- trim(text)
  }

  f <- function(expr) {
    eval_func <- as.character(.transformer(expr, .envir) %||% character())
  }

  res <- .Call(glue_, text, f, .open, .close, .cli)

  res <- drop_null(res)
  if (any(lengths(res) == 0)) {
    return(character(0))
  }

  res[] <- lapply(res, function(x) replace(x, is.na(x), "NA"))

  do.call(paste0, res)
}

count_brace_exp <- function(text, .open = "{", .close = "}") {
  cnt <- 0
  trans <- function(text, envir) {
    cnt <<- cnt + 1L
    ""
  }
  glue(text, .transformer = trans, .open = .open, .close = .close)
  cnt
}

identity_transformer <- function (text, envir) {
  eval(parse(text = text, keep.source = FALSE), envir)
}

drop_null <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

#' Collapse a vector into a string scalar
#'
#' @description
#' Features:
#'
#' - custom separator,
#' - custom last separator: `last` argument,
#' - adds ellipsis to truncated strings,
#' - uses Unicode ellipsis character on UTF-8 console,
#' - can collapse "from both ends", with `style = "both-ends"`,
#' - can consider a limit for the display width of the result, in characters,
#' - handles ANSI control sequences correctly when measuring display width.
#'
#' @param x Character vector, or an object with an `as.character()` method
#' to collapse.
#' @param sep Character string, separator.
#' @param last Last separator, if there is no truncation. E.g. use
#' `", and "` for the Oxford comma.
#' @param trunc Maximum number of elements to show. For `style = "head"`
#' at least `trunc = 1` is used. For `style = "both-ends"` at least
#' `trunc = 5` is used, even if a smaller number is specified.
#' @param width Limit for the display width of the result, in characters.
#' This is a hard limit, and the output will never exceed it.
#' This argument is not implemented for the `"both-ends"` style, which
#' always uses `Inf`, with a warning if a fininte `width` value is set.
#' @param ellipsis Character string to use at the place of the truncation.
#' By default, the Unicode ellipsis character is used if the console is
#' UTF-8, and three dots otherwise.
#' @param style Truncation style:
#' * `both-ends`: the default, shows the beginning and end of the vector,
#'   and skips elements in the middle if needed.
#' * `head`: shows the beginning of the vector, and skips elements at the
#'   end, if needed.
#' @return Character scalar. It is `NA_character_` if any elements in the
#' vector are `NA`.
#'
#' @seealso `glue_collapse` in the glue package inspired this function
#' @export
#' @examples
#' ansi_collapse(letters)
#'
#' # truncate
#' ansi_collapse(letters, trunc = 5)
#'
#' # head style
#' ansi_collapse(letters, trunc = 5, style = "head")

ansi_collapse <- function(x, sep = ", ", last = ", and ", trunc = Inf,
                          width = Inf, ellipsis = symbol$ellipsis,
                          style = c("both-ends", "head")) {

  # does not make sense to show ... instead of an element
  if (trunc == length(x) - 1L) trunc <- trunc + 1L

  style <- match.arg(style)
  switch(
    style,
    "both-ends" = collapse_both_ends(x, sep, last, trunc, width, ellipsis),
    "head" = collapse_head(x, sep, last, trunc, width, ellipsis)
  )
}

collapse_head_notrim <- function(x, trunc, sep, last, ellipsis) {
  lnx <- length(x)
  if (lnx == 1) return(x)
  if (lnx <= trunc) {
    # no truncation
    return(paste0(
      paste(x[1:(lnx - 1)], collapse = sep),
      last,
      x[lnx]
    ))
  } else {
    # truncation, no need for 'last'
    return(paste0(
      paste(x[1:trunc], collapse = sep),
      sep,
      ellipsis
    ))
  }
}

collapse_head <- function(x, sep = ", ", last = ", and ", trunc = Inf,
                          width = Inf, ellipsis = symbol$ellipsis) {

  trunc <- max(trunc, 1L)
  x <- as.character(x)
  lnx <- length(x)

  # skepcial cases that do not need trimming
  if (lnx == 0) {
    return("")
  } else if (any(is.na(x))) {
    return(NA_character_)
  }

  # easier case, no width trimming
  if (width == Inf) {
    return(collapse_head_notrim(x, trunc, sep, last, ellipsis))
  }

  # complex case, with width wrapping
  # first we truncate
  tcd <- lnx > trunc
  if (tcd) x <- x[1:trunc]

  # then we calculate the width w/o trimming
  wx <- ansi_nchar(x)
  wsep <- ansi_nchar(sep, "width")
  wlst <- ansi_nchar(last, "width")
  well <- ansi_nchar(ellipsis, "width")
  if (!tcd) {
    # x[1]
    # x[1], and x[2]
    # x[1], x[2], and x[3]
    nsep <- if (lnx >= 2) lnx - 2L else 0L
    nlst <- if (lnx >= 2) 1L else 0L
    wtot <- sum(wx) + nsep * wsep + nlst * wlst
    if (wtot <= width) {
      return(paste0(
        paste(x[1:(lnx - 1L)], collapse = sep),
        last,
        x[lnx]
      ))
    }

  } else {
    # x[1], x[2], x[trunc], ...
    wtot <- sum(wx) + trunc * wsep + well
    if (wtot <= width) {
      return(paste0(
        paste(x, collapse = sep),
        sep,
        ellipsis
      ))
    }
  }

  # we need to find the longest possible truncation for the form
  # x[1], x[2], x[trunc], ...
  # each item is wx + wsep wide, so we search how many fits, with ellipsis
  last <- function(x) if (length(x) >= 1) x[length(x)] else x[NA_integer_]
  trunc <- last(which(cumsum(wx + wsep) + well <= width))

  # not even one element fits
  if (is.na(trunc)) {
    if (well > width) {
      return(ansi_strtrim(ellipsis, width, ellipsis = ""))
    } else if (well == width) {
      return(ellipsis)
    } else if (well + wsep >= width) {
      return(paste0(ansi_strtrim(x[1], width, ellipsis = ""), ellipsis))
    } else {
      return(paste0(
        ansi_strtrim(x[1], max(width - well - wsep, 0L), ellipsis = ellipsis),
        sep,
        ellipsis
      ))
    }
  }

  return(paste0(
    paste(x[1:trunc], collapse = sep),
    sep,
    ellipsis
  ))
}

collapse_both_ends <- function(x, sep = ", ", last = ", and ", trunc = Inf,
                               width = Inf, ellipsis = symbol$ellipsis) {

  if (width != Inf) {
    warning(format_warning(c(
      "!" = "finite {.arg width} is not implemented in {.fun cli::ansi_collapse}.",
      "i" = "{.code width = Inf} is used instead."
    )))
    width <- Inf
  }

  # we always list at least 5 elements
  trunc <- max(trunc, 5L)
  trunc <- min(trunc, length(x))
  if (length(x) <= 5 || length(x) <= trunc) {
    return(collapse_head(x, sep, last, trunc = trunc, width, ellipsis))
  }

  # we have at list six elements in the vector
  # 1, 2, 3, ..., 9, and 10
  x <- as.character(c(x[1:(trunc-2)], x[length(x)-1], x[length(x)]))
  paste0(
    c(x[1:(trunc-2)], ellipsis, paste0(x[trunc-1], last, x[trunc])),
    collapse = sep
  )
}

trim <- function (x) {
  has_newline <- function(x) any(grepl("\\n", x))
  if (length(x) == 0 || !has_newline(x)) {
    return(x)
  }
  .Call(trim_, x)
}
