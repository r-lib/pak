is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_character <- function(x) {
  is.character(x) && !anyNA(x)
}

is_character_or_null <- function(x) {
  is.null(x) || is_character(x)
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && as.integer(x) == x
}
