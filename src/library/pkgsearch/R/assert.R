
is_package_name <- function(string) {
  assert_that(is_string(string))
  grepl("^[0-9a-zA-Z._]*$", string)
}

on_failure(is_package_name) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid package name")
}

# via tools/badversions.R
bad_versions <- list(
  dse = c("R2000.4-1", "R2000.6-1"),
  lme = c(
    "3.0-0 (1999/06/26)",
    "3.0b8a-2 (1999/06/07)",
    "3.1-0 (1999/08/03)"
  ),
  tframe = "R2000.6-1",
  VR = c(
    "5.3pl037-1 (1999/02/08)",
    "5.3pl037-2 (1999/02/08)",
    "6.1-4 (1999/08/15)"
  )
)

# base::.standard_regexps()$valid_package_version
version_regex <- "^([[:digit:]]+[.-]){1,}[[:digit:]]+$"

is_package_version <- function(string) {
  assert_that(is_string(string))
  grep(version_regex, string) || string %in% unlist(bad_versions)
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
