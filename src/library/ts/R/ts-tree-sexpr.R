#' Show the syntax tree of a tree-sitter tree
#'
#' @ts ts_tree_sexpr_description
#' Show the structure of a tree-sitter tree as an S-expression.
#' @description \eval{ts:::doc_insert("ts_tree_sexpr_description")}
#' \eval{ts:::format_rd_parser_list(ts:::ts_list_parsers(), "ts_tree_sexpr")}
#'
#' @ts ts_tree_sexpr_details
#' This function returns a nested list representation of the syntax tree,
#' where each node is represented as a list with its type and children.
#' @details \eval{ts:::doc_insert("ts_tree_sexpr_details")}
#'
#' @ts ts_tree_sexpr_param_tree
#' A `ts_tree` object.
#'
#' @param tree \eval{ts:::doc_insert("ts::ts_tree_sexpr_param_tree")}
#'
#' @ts ts_tree_sexpr_return
#' A string representing the S-expression of the syntax tree.
#' @return \eval{ts:::doc_insert("ts::ts_tree_sexpr_return")}
#'
#' @export
#' @family ts_tree exploration
#' @family ts_tree generics
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc(
#'   "{ \"a\": //comment\ntrue, \"b\": [1, 2, 3] }"
#' )
#'
#' ts_tree_sexpr(tree)
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
#' ts_tree_sexpr(tree)

ts_tree_sexpr <- function(tree) {
  UseMethod("ts_tree_sexpr")
}

#' @export

ts_tree_sexpr.default <- function(tree) {
  call_with_cleanup(c_s_expr, tree)
}
