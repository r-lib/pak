#' @ts ts_tree_ast_details_syntax_vs_dom
#'
#' ```{asciicast}
#' #| results = "hide"
#' tree <- ts_parse_toml("
#'   [package]
#'   name = 'tstoml'
#'   version = '0.1.0'"
#' )
#' ```
#'
#' ```{asciicast}
#' ts_tree_ast(tree)
#' ```
#'
#' ```{asciicast}
#' ts_tree_dom(tree)
#' ```
#'
#' @export

ts_tree_ast.ts_tree_toml <- function(tree) {
  NextMethod()
}
