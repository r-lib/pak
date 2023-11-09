
ansi_regex <- paste0("(?:(?:\\x{001b}\\[)|\\x{009b})",
                     "(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])",
                     "|\\x{001b}[A-M]")

#' Check if a string has some ANSI styling
#'
#' @param string The string to check. It can also be a character
#'   vector.
#' @return Logical vector, `TRUE` for the strings that have some
#'   ANSI styling.
#'
#' @export
#' @examples
#' ## The second one has style if crayon is enabled
#' has_style("foobar")
#' has_style(red("foobar"))

has_style <- function(string) {
  grepl(ansi_regex, string, perl = TRUE)
}

#' Remove ANSI escape sequences from a string
#'
#' @param string The input string.
#' @return The cleaned up string.
#'
#' @export
#' @examples
#' strip_style(red("foobar")) == "foobar"

strip_style <- function(string) {
  gsub_(ansi_regex, "", string, perl = TRUE, useBytes = TRUE)
}
