#' Safely interpolate strings
#'
#' `glue_safe()` and `glue_data_safe()` differ from [glue()] and [glue_data()]
#' in that the safe versions only look up symbols from an environment using
#' [get()]. They do not execute any R code. This makes them suitable for use
#' with untrusted input, such as inputs in a Shiny application, where using the
#' normal functions would allow an attacker to execute arbitrary code.
#' @inheritParams glue
#' @export
#' @examples
#' "1 + 1" <- 5
#' # glue actually executes the code
#' glue("{1 + 1}")
#'
#' # glue_safe just looks up the value
#' glue_safe("{1 + 1}")
#'
#' rm("1 + 1")
glue_safe <- function(..., .envir = parent.frame()) {
  glue(..., .envir = .envir, .transformer = get_transformer)
}

#' @rdname glue_safe
#' @export
glue_data_safe <- function(.x, ..., .envir = parent.frame()) {
  glue_data(.x, ..., .envir = .envir, .transformer = get_transformer)
}

get_transformer <- function(text, envir) {
  if (!exists(text, envir = envir)) {
    stop("object '", text, "' not found", call. = FALSE)
  } else {
    get(text, envir = envir)
  }
}
