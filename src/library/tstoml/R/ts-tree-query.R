#' Run tree-sitter queries on TOML tree-sitter trees
#'
#' @usage
#' \method{ts_tree_query}{ts_tree_toml}(tree, query)
#'
#' @description
#' \eval{tsitter:::doc_insert("ts_tree_query_description", "tsjtoml")}
#'
#' @details
#' \eval{tsitter:::doc_insert("ts_tree_query_details", "tsjtoml")}
#' \eval{tsitter:::doc_extra()}
#'
#' @ts ts_tree_query_details_examples
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml(
#'   'a = 1\nb = [10.0, 20, 30]\nc = { c1 = true, c2 = 100 }'
#' )
#' toml |> ts_tree_query("[(float) (integer)] @number")
#' ```
#'
#' @param tree
#' \eval{tsitter:::doc_insert("ts_tree_query_param_tree", "tsjtoml")}
#' @param query
#' \eval{tsitter:::doc_insert("ts_tree_query_param_query", "tsjtoml")}
#'
#' @return \eval{tsitter:::doc_insert("ts_tree_query_return", "tsjtoml")}
#'
#' @export
#' @examples
#' # Select all numbers in a TOML document ------------------------------------
#' library(tsitter)
#' toml <- tstoml::ts_parse_toml(
#'   'a = 1\nb = [10.0, 20, 30]\nc = { c1 = true, c2 = 100 }'
#' )
#' toml |> ts_tree_query("[(float) (integer)] @number")

ts_tree_query.ts_tree_toml <- function(tree, query) {
  NextMethod()
}
