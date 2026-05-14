#' @ts ts_tree_unserialize_details_no_selection
#'
#' ```{asciicast}
#' #| results = "hide"
#' tree <- tstoml::ts_parse_toml(
#' "a = 1\nb = [10.0, 20, 30]\nc = { c1 = true, c2 = [] }"
#' )
#' ```
#'
#' ```{asciicast}
#' ts_tree_unserialize(tree)
#' ```
#'
#' @ts ts_tree_unserialize_details_empty_selection
#'
#' ```{asciicast}
#' #| results = "hide"
#' tree <- tstoml::ts_parse_toml(
#'   "a = 1\nb = [10.0, 20, 30]\nc = { c1 = true, c2 = [] }"
#' )
#' ```
#'
#' ```{asciicast}
#' tree |> ts_tree_select("nope") |> ts_tree_unserialize()
#' ```
#'
#' @export

ts_tree_unserialize.ts_tree_toml <- function(tree) {
  sel <- ts_tree_selected_nodes(tree)
  as.list(lapply(sel, unserialize_element, tree = tree))
}
