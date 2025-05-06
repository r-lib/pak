#' Prettify adds indentation to a JSON string; minify removes all indentation/whitespace.
#'
#' @rdname prettify
#' @title Prettify or minify a JSON string
#' @name prettify, minify
#' @aliases minify prettify
#' @export prettify minify
#' @param txt JSON string
#' @param indent number of spaces to indent. Use a negative number for tabs instead of spaces.
#' @useDynLib jsonlite R_reformat
#' @examples myjson <- toJSON(cars)
#' cat(myjson)
#' prettify(myjson)
#' minify(myjson)
prettify <- function(txt, indent = 4) {
  stopifnot(is.numeric(indent))
  txt <- paste(txt, collapse = "\n")
  indent_char <- ifelse(indent > 0, " ", "\t")
  indent_string <- paste(rep(indent_char, as.integer(abs(indent))), collapse = "")
  reformat(txt, TRUE, indent_string)
}

#' @rdname prettify
minify <- function(txt) {
  txt <- paste(txt, collapse = "\n")
  reformat(txt, FALSE)
}

reformat <- function(x, pretty, indent_string = "") {
  out <- .Call(R_reformat, x, pretty, indent_string = indent_string)
  if (out[[1]] == 0) {
    return(out[[2]])
  } else {
    stop(out[[2]], call. = FALSE)
  }
}
