`%||%` <- function(l, r) {
  if (is.null(l)) {
    r
  } else {
    l
  }
}

is_false <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

named_list <- function(n = 0) {
  structure(vector("list", n), names = character(n))
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

na_omit <- function(x) {
  x[!is.na(x)]
}

middle <- function(x) {
  if (length(x) <= 2) {
    x[numeric()]
  } else {
    x[-c(1, length(x))]
  }
}

max_or_na <- function(x, na.rm = FALSE) {
  if (length(x)) {
    max(x, na.rm = na.rm)
  } else if (is.integer(x)) {
    NA_integer_
  } else {
    NA_real_
  }
}

is_named <- function(x, null = FALSE) {
  nms <- names(x)
  (null || !is.null(nms)) &&
    length(x) == length(nms) &&
    !anyNA(nms) &&
    all(nms != "")
}

last <- function(x) {
  x[[length(x)]]
}

plural <- function(x) {
  if (x != 1) {
    "s"
  } else {
    ""
  }
}

# For roxygen2 -----------------------------------------------------------------

# nocov start
doclist <- function(x) {
  paste0("`", x, "`", collapse = ", ")
}
#nocov end
