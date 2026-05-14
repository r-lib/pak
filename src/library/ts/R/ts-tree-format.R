#' Format the selected elements of a tree sitter tree for printing
#'
#' @ts ts_tree_format_description
#' (Re)format the selected elements of the document represented by a
#' tree-sitter tree, if the tree-sitter parser supports formatting.
#'
#' @description
#' \eval{ts:::doc_insert("ts_tree_format_description")}
#'
#' \eval{ts:::format_rd_parser_list(ts:::ts_list_parsers(), "ts_tree_format")}
#'
#' @ts ts_tree_format_details
#'
#' If `tree` does not have a selection, then the whole document is
#' formatted.
#'
#' \eval{ts:::doc_tabs("ts_tree_format_details_no_selection")}
#'
#' If `tree` has an empty selection, then it is returned unchanged.
#'
#' \eval{ts:::doc_tabs("ts_tree_format_details_empty_selection")}
#'
#' Some parsers support options to customize the formatting.
#' See details in the manual of the specific parser.
#'
#' \eval{ts:::doc_tabs("ts_tree_format_details_options")}
#'
#' @details
#' \eval{ts:::doc_insert("ts::ts_tree_format_details")}
#' \eval{ts:::doc_extra()}
#'
#' @ts ts_tree_format_param_tree
#' A `ts_tree` object.
#'
#' @ts ts_tree_format_param_options
#' A list of options for the formatting.
#'
#' @param tree
#' \eval{ts:::doc_insert("ts::ts_tree_format_param_tree")}
#' @param options
#' \eval{ts:::doc_insert("ts::ts_tree_format_param_options")}
#'
#' See details in the manual of the specific parser.
#'
#' @param ... Extra arguments for methods.
#'
#' @ts ts_tree_format_return
#' The `ts_tree` object of the reformatted document.
#' @return \eval{ts:::doc_insert("ts::ts_tree_format_return")}
#'
#' @family ts_tree generics
#' @seealso \eval{ts:::doc_seealso("ts_tree_format")}
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc('{ "a":true, "b": [1,2,3] }')
#' tree
#'
#' # Format whole document
#' tree |> ts_tree_format()
#'
#' # Format each top element under the document node in one line
#' tree |> ts_tree_format() |>
#'   ts_tree_select(TRUE) |>
#'   ts_tree_format(options = list(format = "oneline"))
#'
#'
#' @examplesIf requireNamespace("tstoml", quietly = TRUE)
#' # Create a parse tree with tstoml --------------------------------------
#' tree <- tstoml::ts_parse_toml(r"(
#'   [servers]
#'   alpha = { ip = "127.0.0.1", dc = "eqdc10" }
#'   beta = { ip = "127.0.0.2", dc = "eqdc20" }
#' )")
#'
#' tree
#'
#' tree |> ts_tree_format()

ts_tree_format <- function(tree, options, ...) {
  UseMethod("ts_tree_format")
}
