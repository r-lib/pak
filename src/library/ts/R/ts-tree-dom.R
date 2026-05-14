#' Print the document object model (DOM) of a tree-sitter tree
#'
#' @ts ts_tree_dom_description
#' `ts_tree_dom()` prints the document object model (DOM) tree of a ts_tree
#' object. This tree only includes semantic elements. E.g. for a JSON(C)
#' document it includes objects, arrays and various value types, but not
#' the syntax elements like brackets, commas or colons.
#' @description
#' \eval{ts:::doc_insert("ts_tree_dom_description")}
#'
#' \eval{ts:::format_rd_parser_list(ts:::ts_list_parsers(), "ts_tree_dom")}
#'
#' @ts ts_tree_dom_details
#' ## The syntax tree and the DOM tree
#'
#' See \code{\link[ts:ts_tree_ast]{ts_tree_ast()}} for the complete
#' tree-sitter syntax tree that includes all nodes, including syntax
#' elements like brackets and commas.
#'
#' \eval{ts:::doc_tabs("ts_tree_ast_details_syntax_vs_dom")}
#'
#' @details
#' \eval{ts:::doc_insert("ts::ts_tree_dom_details")}
#' \eval{ts:::doc_extra()}
#'
#' @ts ts_tree_dom_param_tree
#' A `ts_tree` object.
#' @param tree \eval{ts:::doc_insert("ts::ts_tree_dom_param_tree")}
#'
#' @ts ts_tree_dom_return
#' Character vector, the formatted annotated syntax tree, line by
#' line. It has class [cli_tree][cli::tree()], from the cli package. It
#' may contain ANSI escape sequences for coloring and hyperlinks.
#' @return
#' \eval{ts:::doc_insert("ts::ts_tree_dom_return")}
#'
#' @family ts_tree exploration
#' @family ts_tree generics
#' @seealso [ts_tree_ast()] to show the annotated syntax tree of a
#'   ts_tree object.
#'
#'   \eval{ts:::doc_seealso("ts_tree_dom")}
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc('{"foo": 42, "bar": [1, 2, 3]}')
#'
#' tree
#'
#' ts_tree_ast(tree)
#'
#' ts_tree_dom(tree)
#'
#' @examplesIf requireNamespace("tstoml", quietly = TRUE)
#'
#' # Create a parse tree with tstoml --------------------------------------
#' tree <- tstoml::ts_parse_toml(r"(
#'   title = "TOML Example"
#'   [owner]
#'   name = "Tom Preston-Werner"
#'   dob = 1979-05-27T07:32:00-08:00
#' )")
#'
#' tree
#'
#' ts_tree_ast(tree)
#'
#' ts_tree_dom(tree)

ts_tree_dom <- function(tree) {
  UseMethod("ts_tree_dom")
}

#' @export

ts_tree_dom.default <- function(tree) {
  is_dom_node <- c(1L, which(!is.na(tree$dom_parent)))
  dom <- tree[is_dom_node, ]

  treetab <- data_frame(
    id = as.character(dom$id),
    children = lapply(dom$dom_children, as.character),
    label = paste0(
      dom$dom_type,
      " (",
      dom$id,
      ")",
      ifelse(
        is.na(dom$dom_name),
        "",
        cli::col_grey(paste0(" # ", dom$dom_name))
      )
    )
  )

  tree <- cli::tree(treetab)
  tree
}
