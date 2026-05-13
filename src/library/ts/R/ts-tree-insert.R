#' Insert a new element into a tree-sitter tree
#'
#' @ts ts_tree_insert_description
#' Insert a new element into each selected element.
#' @description
#' \eval{ts:::doc_insert("ts_tree_insert_description")}
#'
#' \eval{ts:::format_rd_parser_list(ts:::ts_list_parsers(), "ts_tree_insert")}
#'
#' @ts ts_tree_insert_details
#' It is not always possible to insert a new element into a selected
#' element. For example in a JSONC document you can only insert a new
#' element into an array or an object, but not into scalar elements.
#' If the insertion is not possible, an error is raised.
#'
#' \eval{ts:::doc_tabs("ts_tree_insert_details_errors")}
#'
#' If `tree` does not have a selection, the new element is inserted into
#' at the top level.
#'
#' \eval{ts:::doc_tabs("ts_tree_insert_details_no_selection")}
#'
#' If `tree` has an empty selection, then it is returned unchanged, i.e.
#' no new element is inserted.
#'
#' \eval{ts:::doc_tabs("ts_tree_insert_details_empty_selection")}
#' @details
#' \eval{ts:::doc_insert("ts::ts_tree_insert_details")}
#' \eval{ts:::doc_extra()}
#'
#' @ts ts_tree_insert_param_tree
#' A `ts_tree` object.
#' @ts ts_tree_insert_param_new
#' The new element to insert.
#' @ts ts_tree_insert_param_key
#' The key of the new element, if inserting into a keyed element.
#' @ts ts_tree_insert_param_at
#' The position to insert the new element at.
#' @ts ts_tree_insert_param_options
#' A list of options for the insertion.
#'
#' @param tree \eval{ts:::doc_insert("ts::ts_tree_insert_param_tree")}
#' @param new \eval{ts:::doc_insert("ts::ts_tree_insert_param_new")}
#'
#' The type of `new` depends on the parser and the method that implements
#' the insertion. See details in the manual of the specific parser.
#' @param key \eval{ts:::doc_insert("ts::ts_tree_insert_param_key")}
#'
#' For example a JSON(C) object or a TOML table are keyed elements.
#' @param at \eval{ts:::doc_insert("ts::ts_tree_insert_param_at")}
#'
#' The interpretation of this argument depends on the method that
#' implements the insertion. Typically the followings are supported:
#' - `0` inserts at the beginning.
#' - `Inf` inserts at the end.
#' - A positive integer `n` inserts _after_ the `n`-th element.
#' - A character scalar inserts _after_ the element with the given key,
#'   in keyed elements.
#'
#' See the details in the manual of the specific parser.
#' @param options \eval{ts:::doc_insert("ts::ts_tree_insert_param_options")}
#'
#' See details in the manual of the specific parser.
#' @param ... Extra arguments for methods.
#'
#' @ts ts_tree_insert_return
#' A `ts_tree` object representing the modified parse tree.
#' @return \eval{ts:::doc_insert("ts::ts_tree_insert_return")}
#'
#' @family ts_tree generics
#' @seealso \eval{ts:::doc_seealso("ts_tree_insert")}
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc('{ "a": true, "b": [1, 2, 3] }')
#'
#' tree |> ts_tree_select("b") |> ts_tree_insert(4, at = Inf)
#'
#' @examplesIf requireNamespace("tstoml", quietly = TRUE)
#'
#' # Create a parse tree with tstoml --------------------------------------
#' tree <- tstoml::ts_parse_toml(r"(
#'   [servers]
#'   alpha = { ip = "127.0.0.1", dc = "eqdc10" }
#'   beta = { ip = "127.0.0.2", dc = "eqdc20" }
#' )")
#'
#' tree |>
#'   ts_tree_select("servers", TRUE) |>
#'   ts_tree_insert(key = "active", TRUE)

ts_tree_insert <- function(tree, new, key, at, options, ...) {
  UseMethod("ts_tree_insert")
}
