#' Parse and Evaluate R code
#'
#' This is a simple wrapper around `eval(parse())`, used as the default
#' transformer.
#' @param text Text (typically) R code to parse and evaluate.
#' @param envir environment to evaluate the code in
#' @seealso `vignette("transformers", "glue")` for documentation on creating
#'   custom glue transformers and some common use cases.
#' @export
identity_transformer <- function(text, envir) {
  eval(parse(text = text, keep.source = FALSE), envir)
}
