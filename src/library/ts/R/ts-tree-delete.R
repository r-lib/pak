#' Delete selected elements from a tree-sitter tree
#'
#' @ts ts_tree_delete_description
#' Use \code{\link[ts:ts_tree_select]{ts_tree_select()}} to select the
#' elements to be deleted, and then call `ts_tree_delete()` to remove them
#' from the tree.
#'
#' @ts ts_tree_delete_details
#' The formatting of the rest of the document is left as is.
#'
#' \eval{ts:::doc_tabs("ts_tree_delete_details_formatting")}
#'
#' If the tree does not have a selection, the tree corresponding to the
#' empty document is returned, i.e. the whole content is deleted.
#'
#' \eval{ts:::doc_tabs("ts_tree_delete_details_no_selection")}
#'
#' If the tree has a selection, but it is the empty selection, then
#' the tree is returned unchanged.
#'
#' \eval{ts:::doc_tabs("ts_tree_delete_details_empty_selection")}
#'
#' For parsers that support comments, deleting elements that include
#' comments typically delete the comments as well. Other comments are
#' kept as is. See details in the manual of the specific parser.
#'
#' \eval{ts:::doc_tabs("ts_tree_delete_details_comments")}
#'
#' @description
#' \eval{ts:::doc_insert("ts_tree_delete_description")}
#'
#' \eval{ts:::format_rd_parser_list(ts:::ts_list_parsers(), "ts_tree_delete")}
#'
#' @details
#' \eval{ts:::doc_insert("ts::ts_tree_delete_details")}
#' \eval{ts:::doc_extra()}
#'
#' @ts ts_tree_delete_param_tree
#' A `ts_tree` object.
#'
#' @param tree
#' \eval{ts:::doc_insert("ts::ts_tree_delete_param_tree")}
#' @param ... Extra arguments for methods.
#'
#' @ts ts_tree_delete_return
#' The modified `ts_tree` object with the selected elements removed.
#' @return \eval{ts:::doc_insert("ts::ts_tree_delete_return")}
#'
#' @family ts_tree generics
#' @seealso \eval{ts:::doc_seealso("ts_tree_delete")}
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc(
#'   "{ \"a\": //comment\ntrue, \"b\": [1, 2, 3] }"
#' )
#'
#' tree
#'
#' tree |> ts_tree_select("a")
#'
#' tree |> ts_tree_select("a") |> ts_tree_delete()
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
#' tree |> ts_tree_select("servers", TRUE, "dc")
#' tree |> ts_tree_select("servers", TRUE, "dc") |> ts_tree_delete()

ts_tree_delete <- function(tree, ...) {
  UseMethod("ts_tree_delete")
}
