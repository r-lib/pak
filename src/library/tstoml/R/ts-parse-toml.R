#' Parse a TOML file or string into a ts_tree_toml object
#'
#' @inheritParams ts::ts_tree_new
#' @param options Named list of parsing options, see
#'   [tstoml options][tstoml_options].
#' @return A `ts_tree_toml` object.
#' @export

ts_parse_toml <- function(
  text,
  ranges = NULL,
  fail_on_parse_error = TRUE,
  options = NULL
) {
  text <- as_character(text)
  ts_tree_new(
    language = ts_language_toml(),
    file = NULL,
    text = text,
    ranges = ranges,
    fail_on_parse_error = fail_on_parse_error,
    options = options
  )
}

#' @rdname ts_parse_toml
#' @export

ts_read_toml <- function(
  file,
  ranges = NULL,
  fail_on_parse_error = TRUE,
  options = NULL
) {
  file <- as_existing_file(file)
  ts_tree_new(
    language = ts_language_toml(),
    text = NULL,
    file = file,
    ranges = ranges,
    fail_on_parse_error = fail_on_parse_error,
    options = options
  )
}
