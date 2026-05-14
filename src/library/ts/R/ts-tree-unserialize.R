#' Unserialize selected elements of a tree-sitter tree
#'
#' @ts ts_tree_unserialize_description
#' Unserialize the selected elements of a `ts_tree` object, i.e. convert
#' them to R objects.
#' @description
#' \eval{ts:::doc_insert("ts_tree_unserialize_description")}
#'
#' \eval{ts:::format_rd_parser_list(ts:::ts_list_parsers(), "ts_tree_unserialize")}
#'
#' @ts ts_tree_unserialize_details
#' If no elements are selected in the tree, then the whole document is
#' unserialized.
#'
#' \eval{ts:::doc_tabs("ts_tree_unserialize_details_no_selection")}
#'
#' If the tree has an empty selection, then an empty list is returned.
#'
#' \eval{ts:::doc_tabs("ts_tree_unserialize_details_empty_selection")}
#'
#' ## The `[[` operator
#'
#' The `[[` operator works similarly to the combination of
#' \code{\link[ts:ts_tree_select]{ts_tree_select()}} and
#' `ts_tree_unserialize()`, but it might be more readable.
#'
#' \eval{ts:::doc_tabs("ts_tree_select_brackets")}
#'
#' @details
#' \eval{ts:::doc_insert("ts::ts_tree_unserialize_details")}
#' \eval{ts:::doc_extra()}
#' For the details on how the selected elements are mapped to R objects,
#' see the documentation of the methods in the parser packages. The methods
#' in the installed parser packages are linked below.
#'
#' @ts ts_tree_unserialize_param_tree
#' A `ts_tree` object.
#'
#' @param tree \eval{ts:::doc_insert("ts::ts_tree_unserialize_param_tree")}
#'
#' @ts ts_tree_unserialize_return
#' List of R objects, with one entry for each selected element.
#' @return \eval{ts:::doc_insert("ts::ts_tree_unserialize_return")}
#'
#' @family ts_tree generics
#' @family serialization functions
#' @seealso \eval{ts:::doc_seealso("ts_tree_unserialize")}
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc('{"a": 13, "b": [1, 2, 3], "c": "x"}')
#'
#' tree
#'
#' tree |> ts_tree_select(c("b", "c")) |> ts_tree_unserialize()
#'
#' tree |> ts_tree_select("b") |> ts_tree_unserialize()

ts_tree_unserialize <- function(tree) {
  UseMethod("ts_tree_unserialize")
}
