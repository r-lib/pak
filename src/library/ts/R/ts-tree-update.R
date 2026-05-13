#' Replace selected elements with a new element in a tree-sitter tree
#'
#' @ts ts_tree_update_description
#' Replace all selected elements with a new element.
#' @description
#' \eval{ts:::doc_insert("ts_tree_update_description")}
#'
#' @ts ts_tree_update_details
#' If the tree does not have a selection, the new element replaces the
#' whole document.
#'
#' \eval{ts:::doc_tabs("ts_tree_update_details_no_selection")}
#'
#' If the tree has an empty selection, the new element is inserted at the
#' position of where the selected elements would be.
#'
#' \eval{ts:::doc_tabs("ts_tree_update_details_empty_selection")}
#'
#' @details
#' \eval{ts:::doc_insert("ts::ts_tree_update_details")}
#' \eval{ts:::doc_extra()}
#'
#' @ts ts_tree_update_param_tree
#' A `ts_tree` object.
#' @ts ts_tree_update_param_new
#' The new element to replace the selected elements with.
#' @ts ts_tree_update_param_options
#' A list of options for the update.
#'
#' @param tree \eval{ts:::doc_insert("ts::ts_tree_update_param_tree")}
#' @param new \eval{ts:::doc_insert("ts::ts_tree_update_param_new")}
#'
#' The type of `new` depends on the parser and the method that implements
#' the insertion. See details in the manual of the specific parser.
#' @param options \eval{ts:::doc_insert("ts::ts_tree_update_param_options")}
#'
#' See details in the manual of the specific parser.
#' @param ... Extra arguments for methods.
#'
#' @ts ts_tree_update_return
#' The modified `ts_tree` object with the selected elements replaced
#' by the new element.
#'
#' @return \eval{ts:::doc_insert("ts::ts_tree_update_return")}
#'
#' @family ts_tree generics
#' @seealso \eval{ts:::doc_seealso("ts_tree_update")}
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc(r"(
#'   {
#'     "name": "example",
#'     "version": "1.0.0",
#'     "dependencies": {
#'       "tsjsonc": "^0.1.0"
#'     }
#'   }
#' )")
#'
#' tree |> ts_tree_select("version") |> ts_tree_update("2.0.0")
#'
#' @examplesIf requireNamespace("tstoml", quietly = TRUE)
#' # Create a parse tree with tstoml --------------------------------------
#' tree <- tstoml::ts_parse_toml(r"(
#'   [package]
#'   name = "example"
#'   version = "1.0.0"
#'   depdendencies = { tstoml = "0.1.0" }
#' )")
#'
#' tree |> ts_tree_select("package", "version") |> ts_tree_update("2.0.0")

ts_tree_update <- function(tree, new, options, ...) {
  UseMethod("ts_tree_update")
}
