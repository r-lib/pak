parseJSON <- function(txt, bigint_as_char = FALSE) {
  if (inherits(txt, "connection")) {
    parse_con(txt, bigint_as_char)
  } else {
    parse_string(txt, bigint_as_char)
  }
}

#' @useDynLib jsonlite R_parse
parse_string <- function(txt, bigint_as_char) {
  if (length(txt) > 1) {
    txt <- paste(txt, collapse = "\n")
  }
  .Call(R_parse, txt, bigint_as_char)
}
