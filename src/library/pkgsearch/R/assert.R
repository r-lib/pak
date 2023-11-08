
is_package_name <- function(string) {
  assert_that(is_string(string))
  grepl("^[0-9a-zA-Z._]*$", string)
}

on_failure(is_package_name) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid package name")
}

is_package_version <- function(string) {
  assert_that(is_string(string))
  grepl("^[0-9a-zA-Z._]*$", string)
}

on_failure(is_package_version) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid (package or R) version")
}

is_integerish <- function(x) {
  is.integer(x) || (is.numeric(x) && all(x == trunc(x)) && !is.na(x))
}

is_positive_count <- function(x) {
  is_integerish(x) && length(x) == 1 && !is.na(x) && x > 0
}

on_failure(is_positive_count) <- function(call, env) {
  paste0(deparse(call$x), " is not a count (a single positive integer)")
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " is not a flag (a length one logical vector).")
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string (a length one character vector).")
}
