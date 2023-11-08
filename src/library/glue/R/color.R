#' Construct strings with color
#'
#' @description
#' The [crayon][crayon::crayon] package defines a number of functions used to
#' color terminal output. `glue_col()` and `glue_data_col()` functions provide
#' additional syntax to make using these functions in glue strings easier.
#'
#' Using the following syntax will apply the function [crayon::blue()] to the text 'foo bar'.
#'
#' ```
#' {blue foo bar}
#' ```
#'
#' If you want an expression to be evaluated, simply place that in a normal brace
#' expression (these can be nested).
#'
#' ```
#' {blue 1 + 1 = {1 + 1}}
#' ```
#'
#' If the text you want to color contains, e.g., an unpaired quote or a comment
#' character, specify `.literal = TRUE`.
#'
#' @inheritParams glue
#' @export
#' @examplesIf require(crayon)
#' library(crayon)
#'
#' glue_col("{blue foo bar}")
#'
#' glue_col("{blue 1 + 1 = {1 + 1}}")
#'
#' glue_col("{blue 2 + 2 = {green {2 + 2}}}")
#'
#' white_on_black <- bgBlack $ white
#' glue_col("{white_on_black
#'   Roses are {red {colors()[[552]]}},
#'   Violets are {blue {colors()[[26]]}},
#'   `glue_col()` can show \\
#'   {red c}{yellow o}{green l}{cyan o}{blue r}{magenta s}
#'   and {bold bold} and {underline underline} too!
#' }")
#'
#' # this would error due to an unterminated quote, if we did not specify
#' # `.literal = TRUE`
#' glue_col("{yellow It's} happening!", .literal = TRUE)
#'
#' # `.literal = TRUE` also prevents an error here due to the `#` comment
#' glue_col(
#'   "A URL: {magenta https://github.com/tidyverse/glue#readme}",
#'   .literal = TRUE
#' )
#'
#' # `.literal = TRUE` does NOT prevent evaluation
#' x <- "world"
#' y <- "day"
#' glue_col("hello {x}! {green it's a new {y}!}", .literal = TRUE)
glue_col <- function(..., .envir = parent.frame(), .na = "NA", .literal = FALSE) {
  glue(..., .envir = .envir, .na = .na, .literal = .literal, .transformer = color_transformer)
}

#' @rdname glue_col
#' @export
glue_data_col <- function(.x, ..., .envir = parent.frame(), .na = "NA", .literal = FALSE) {
  glue_data(.x, ..., .envir = .envir, .na = .na, .literal = .literal, .transformer = color_transformer)
}

color_transformer <- function(code, envir) {
  res <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) e)
  if (!inherits(res, "error")) {
    return(eval(res, envir = envir))
  }

  code <- glue_collapse(code, "\n")
  m <- regexpr("(?s)^([[:alnum:]_]+)[[:space:]]+(.+)", code, perl = TRUE)
  has_match <- m != -1
  if (!has_match) {
    stop(res)
  }
  starts <- attr(m, "capture.start")
  ends <- starts + attr(m, "capture.length") - 1L
  captures <- substring(code, starts, ends)
  fun <- captures[[1]]
  text <- captures[[2]]
  out <- glue(text, .envir = envir, .transformer = color_transformer)

  color_fun <- get0(fun, envir = envir, mode = "function")
  if (is.null(color_fun) && requireNamespace("crayon", quietly = TRUE)) {
    color_fun <- get0(fun, envir = asNamespace("crayon"), mode = "function")
  }

  if (is.null(color_fun)) {
    # let nature take its course, i.e. throw the usual error
    get(fun, envir = envir, mode = "function")
  } else {
    color_fun(out)
  }
}
