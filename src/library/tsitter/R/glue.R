# Compared to glue::glue(), these are fixed:
# - .sep = ""
# - .trim = TRUE
# - .null = character()
# - .literal = TRUE
# - .comment = ""
#
# we also don't allow passing in data as arguments, and `text` is
# a single argument, no need to `paste()` etc.

glue <- function(
  text,
  .envir = parent.frame(),
  .transformer = identity_transformer,
  .open = "{",
  .close = "}",
  .cli = FALSE,
  .trim = TRUE
) {
  text <- paste0(text, collapse = "")

  if (.trim) {
    text <- trim(text)
  }

  f <- function(expr) {
    eval_func <- as.character(.transformer(expr, .envir) %||% character())
  }

  res <- call_with_cleanup(c_glue, text, f, .open, .close, .cli)

  paste0(unlist(res), collapse = "")
}

identity_transformer <- function(text, envir) {
  eval(parse(text = text, keep.source = FALSE), envir)
}
