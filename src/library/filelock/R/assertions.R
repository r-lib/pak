
is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

is_timeout <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0
}
