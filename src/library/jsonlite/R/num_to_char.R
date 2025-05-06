#' @useDynLib jsonlite R_num_to_char
num_to_char <- function(x, digits = NA, na_as_string = NA, use_signif = FALSE, always_decimal = FALSE) {
  if (is.na(digits)) digits <- NA_integer_
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(digits))
  stopifnot(is.logical(na_as_string))
  .Call(R_num_to_char, x, digits, na_as_string, use_signif, always_decimal)
}

#' @useDynLib jsonlite R_integer64_to_char
integer64_to_char <- function(x, na_as_string = TRUE) {
  .Call(R_integer64_to_char, x, na_as_string)
}

num_to_char_R <- function(x, digits = NA, na_as_string = NA) {
  if (is.na(digits)) digits <- NA_integer_
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(digits))
  stopifnot(is.logical(na_as_string))
  if (!is.integer(x) && !is.null(digits) && !is.na(digits)) {
    x <- round(x, digits)
  }

  #convert to strings
  tmp <- as.character(x)

  # in numeric variables, NA, NaN, Inf are replaced by character strings
  if (any(missings <- which(!is.finite(x)))) {
    if (is.na(na_as_string)) {
      tmp[missings] <- NA_character_
    } else if (na_as_string) {
      tmp[missings] <- wrapinquotes(x[missings])
    } else {
      tmp[missings] <- "null"
    }
  }

  #returns a character vector
  return(tmp)
}
