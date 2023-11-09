
#' Simulate (a subset of) a VT-5xx ANSI terminal
#'
#' This is utility function that calculates the state of a VT-5xx screen
#' after a certain set of output.
#'
#' Currently it supports:
#'
#' - configurable terminal width and height
#' - ASCII printable characters.
#' - `\n`, `\r`.
#' - ANSI SGR colors, 8 color mode, 256 color mode and true color mode.
#' - Other ANSI SGR features: bold, italic, underline, strikethrough,
#'   blink, inverse.
#'
#' It does _not_ currently supports other features, mode notably:
#'
#' - Other ANSI control sequences and features. Other control sequences
#'   are silently ignored.
#' - Wide Unicode characters. Their width is not taken into account
#'   correctly.
#' - Unicode graphemes.
#'
#' @param output Character vector or raw vector. Character vectors are
#' collapsed (without a separater), and converted to a raw vector using
#' [base::charToRaw()].
#' @param width Terminal width.
#' @param height Terminal height.
#' @return Data frame with columns `lineno`, `segmentno`, `segment`,
#' `attributes`.
#'
#' @note
#' This function is experimental, and the virtual temrinal API will
#' likely change in future versions of cli.
#'
#' @export

vt_output <- function(output, width = 80L, height = 25L) {
  if (is.character(output)) {
    output <- charToRaw(paste(output, collapse = ""))
  }

  res <- .Call(
    clic_vt_output,
    output,
    as.integer(width),
    as.integer(height)
  )

  linksx <- vapply(res$links, intToUtf8, character(1))
  links <- sub("^[^;]*;", "", linksx)
  links_params <- sub(";[^;]*$", "", linksx)

  df <- data.frame(
    stringsAsFactors = FALSE,
    lineno = integer(),
    segmentno = integer(),
    segment = character(),
    attributes = character()
  )

  segments <- lapply(seq_along(res$lines), function(i) {
    line <- intToUtf8(res$lines[[i]])
    attr <- res$attrs[[i]]
    lgs <- rle(attr)
    clgs <- cumsum(c(0, lgs$lengths))
    segs <- mapply(clgs[-length(clgs)], clgs[-1], FUN = function(s, e) {
      utf8_substr(line, s + 1, e)
    })

    fg <- re_match(lgs$values, "fg:([0-9]+|#[0-9a-f]+);")[,1]
    bg <- re_match(lgs$values, "bg:([0-9]+|#[0-9a-f]+);")[,1]
    linkno <- as.integer(re_match(lgs$values, "link:([0-9]+);")[,1])
    link <- links[linkno]
    link_params <- links_params[linkno]

    data.frame(
      stringsAsFactors = FALSE,
      lineno = i,
      segmentno = seq_along(segments),
      segment = segs,
      bold = grepl("bold;", lgs$values),
      italic = grepl("italic;", lgs$values),
      underline = grepl("underline;", lgs$values),
      strikethrough = grepl("strikethrough;", lgs$values),
      blink = grepl("blink;", lgs$values),
      inverse = grepl("inverse;", lgs$values),
      color= fg,
      background_color = bg,
      link = link,
      link_params = link_params
    )
  })

  do.call(rbind, c(list(df), segments))
}
