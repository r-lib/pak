`%||%` <- function(l, r) {
  if (is.null(l)) {
    r
  } else {
    l
  }
}

`%&&%` <- function(l, r) {
  if (is.null(l)) {
    NULL
  } else {
    r
  }
}

map_int <- function(.x, .f, ...) {
  vapply(.x, .f, integer(1), ...)
}

map_chr <- function(.x, .f, ...) {
  vapply(.x, .f, character(1), ...)
}

map_lgl <- function(.x, .f, ...) {
  vapply(.x, .f, logical(1), ...)
}

pmap <- function(.l, .f) {
  do.call(mapply, c(list(FUN = .f, SIMPLIFY = FALSE, USE.NAMES = FALSE), .l))
}

imap <- function(.x, .f) {
  idx <- names(.x) %||% seq_along(.x)
  mapply(.f, .x, idx, SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

get_tree_lang <- function(tree) {
  cls <- grep("^ts_tree_", class(tree), value = TRUE)
  if (length(cls) == 0) {
    "<unknown>"
  } else {
    sub("^ts_tree_", "", cls[1])
  }
}

middle <- function(x) {
  if (length(x) <= 2) {
    x[numeric()]
  } else {
    x[-c(1, length(x))]
  }
}

is_named <- function(x) {
  nms <- names(x)
  length(x) == length(nms) && !anyNA(nms) && all(nms != "")
}

get_env <- function(x) {
  ev <- Sys.getenv(x, unset = NA_character_)
  if (is.na(ev)) {
    NULL
  } else {
    ev
  }
}

read_char <- function(fn) {
  paste0(readLines(fn, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
