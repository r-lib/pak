#' Format a TOML file
#'
#' @param file Path to a TOML file. Use one of `file` or `text`.
#' @param text TOML content as a raw vector or a character vector. Use one
#'  of `file` or `text`.
#' @param options Named list of parsing and formatting options, see
#'  [tstoml options][tstoml_options].
#' @export

ts_format_toml <- function(
  file = NULL,
  text = NULL,
  options = NULL
) {
  if (!missing(options)) {
    ts_check_named_arg(options)
  }
  options <- as_tstoml_options(options)

  # parse file/text
  tree <- ts_tree_new(
    ts_language_toml(),
    file = file,
    text = text,
    options = options
  )
  format_element(tree, 1L, options = options)
}
