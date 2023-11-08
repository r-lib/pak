
#' Format a value for printing
#'
#' This function can be used directly, or via the `{.val ...}` inline
#' style. `{.val {expr}}` calls `cli_format()` automatically on the value
#' of `expr`, before styling and collapsing it.
#'
#' ## Default style
#'
#' ```{asciicast cli-format-default}
#' months <- month.name[1:3]
#' cli_text("{.val {months}}")
#' ```
#'
#' ```{asciicast cli-format-num}
#' nums <- 1:5 / 7
#' cli_text("{.val {nums}}")
#' ```
#'
#' ## Styling with themes
#'
#' ```{asciicast cli-format-theme}
#' nums <- 1:5 / 7
#' divid <- cli_div(theme = list(.val = list(digits = 3)))
#' cli_text("{.val {nums}}")
#' cli_end(divid)
#' ```
#'
#' It is possible to define new S3 methods for `cli_format` and then
#' these will be used automatically for `{.val ...}` expressions.
#'
#' ```{asciicast cli-format-class}
#' cli_format.month <- function(x, style = NULL, ...) {
#'   x <- encodeString(substr(x, 1, 3), quote = "\"")
#'   NextMethod("cli_format")
#' }
#' registerS3method("cli_format", "month", cli_format.month)
#' months <- structure(month.name[1:3], class = "month")
#' cli_text("{.val {months}}")
#' ```
#'
#' @param x The object to format.
#' @param style List of formatting options, see the individual methods
#'   for the style options they support.
#' @param ... Additional arguments for methods.
#'
#' @export
#' @seealso [cli_vec()]

cli_format <- function(x, style = NULL, ...) {
  if (is.null(style) && !is.null(default_app())) {
    style <- default_app()$get_current_style()
    cli_format(x, style, ...)
  } else {
    UseMethod("cli_format")
  }
}

#' @rdname cli_format
#' @export

cli_format.default <- function(x, style = NULL, ...) {
  x
}

#' * Styles for character vectors:
#'   - `string-quote` is the quoting character for [encodeString()].
#'
#' @rdname cli_format
#' @export

cli_format.character <- function(x, style = NULL, ...) {
  quote <- style$`string-quote` %||% style$string_quote %||% "\""
  encodeString(x, quote = quote)
}

#' * Styles for numeric vectors:
#'   - `digits` is the number of digits to print after the decimal point.
#'
#' @rdname cli_format
#' @export

cli_format.numeric <- function(x, style = NULL, ...) {
  digits <- style$digits
  if (!is.null(digits)) x <- round(x, digits)
  x
}

#' Add custom cli style to a vector
#'
#' @details
#' You can use this function to change the default parameters of
#' collapsing the vector into a string, see an example below.
#'
#' The style is added as an attribute, so operations that remove
#' attributes will remove the style as well.
#'
#' ## Custom collapsing separator
#'
#' ```{asciicast cli-vec}
#' v <- cli_vec(
#'   c("foo", "bar", "foobar"),
#'   style = list("vec-sep" = " & ", "vec-last" = " & ")
#' )
#' cli_text("My list: {v}.")
#' ```
#'
#' ## Custom truncation
#'
#' ```{asciicast cli-vec-2}
#' x <- cli_vec(names(mtcars), list("vec-trunc" = 3))
#' cli_text("Column names: {x}.")
#' ```
#'
#' @param x Vector that will be collapsed by cli.
#' @param style Style to apply to the vector. It is used as a theme on
#' a `span` element that is created for the vector. You can set `vec-sep`
#' and `vec-last` to modify the separator and the last separator.
#'
#' @export
#' @seealso [cli_format()]

cli_vec <- function(x, style = list()) {
  attr(x, "cli_style") <- style
  x
}
