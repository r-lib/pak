
#' @importFrom assertthat on_failure<- assert_that has_name

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string (length 1 character)")
}

is_string_or_null <- function(x) {
  is.null(x) || is_string(x)
}

on_failure(is_string_or_null) <- function(call, env) {
  paste0(deparse(call$x), " must be a string (length 1 character) or NULL")
}

is_non_empty_string <- function(x) {
  is_string(x) && x != ""
}

on_failure(is_non_empty_string) <- function(call, env) {
  paste0(deparse(call$x), " must be a non-empty string (length 1 character)")
}

is_non_empty_string_or_null <- function(x) {
  is.null(x) || is_non_empty_string(x)
}

on_failure(is_non_empty_string_or_null) <- function(call, env) {
  paste0(
    deparse(call$x),
    " must be a non-empty string (length 1 character) or NULL"
  )
}

is_string_or_raw <- function(x) {
  is.raw(x) || is_string(x)
}

on_failure(is_string_or_raw) <- function(call, env) {
  paste0(
    deparse(call$x),
    " must be a string (length 1 character) or raw vector"
  )
}

is_list_with_names <- function(x, names) {
  is.list(x) &&
    length(x) == length(names) &&
    all(vapply(names, function(name) has_name(x, name), logical(1L)))
}

on_failure(is_list_with_names) <- function(call, env) {
  paste0(
    deparse(call$x),
    " must be a named list of length ", length(call$names), " with entries ",
    paste(vapply(call$names, sQuote, character(1L)), collapse = ", ")
  )
}
