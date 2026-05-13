#' Convert ts_tree object to a data frame
#'
#' @ts ts_tree_brackets_description
#' Create a data frame for the syntax tree of a JSON document, by indexing
#' a ts_tree object with single brackets. This is occasionally useful for
#' exploration and debugging.
#' @description \eval{ts:::doc_insert("ts_tree_brackets_description")}
#'
#' @ts ts_tree_brackets_details
#' A tree-sitter tree object has at least four classes:
#' * `ts_tree_<parser_name>`, e.g. `ts_tree_tsjsonc`,
#' * `ts_tree`,
#' * `tbl`, from the pillar package, for better printing when converted
#'   to a data frame, and
#' * `data.frame`, since it is a data frame internally.
#'
#' The `ts_tree` class has custom [format()] and [print()] methods, that
#' show (part of) the underlying document, and also the selected elements,
#' if any.
#'
#' </p><p><!-- ------------------------------------------------------>
#'
#' It is sometimes useful to treat a `tree` `ts_tree` object as a data
#' frame, and drop the `ts_tree` classes. This can be done by indexing with
#' single brackets, e.g. `tree[]`. This returns a data frame with one
#' row per token, and various columns with information about the tokens.
#' See details in the 'Value' section or this page.
#'
#' @details \eval{ts:::doc_insert("ts::ts_tree_brackets_details")}
#'
#' @ts ts_tree_brackets_param_x
#' A `ts_tree` object.
#' @ts ts_tree_brackets_param_ij
#' Incides, passed to the regular data.frame indexing method, see
#' \code{\link[base:Extract]{'Extract'}}.
#' @ts ts_tree_brackets_param_drop
#' Passed to the regular data.frame indexing method, see
#' \code{\link[base:Extract]{'Extract'}}.
#'
#' @param x \eval{ts:::doc_insert("ts::ts_tree_brackets_param_x")}
#' @param i,j \eval{ts:::doc_insert("ts::ts_tree_brackets_param_ij")}
#' @param drop \eval{ts:::doc_insert("ts::ts_tree_brackets_param_drop")}
#'
#' @ts ts_tree_brackets_return
#' A data frame with one row per token, and columns:
#' * `id`: integer, the id of the token. The (root) document node has id 1.
#' * `parent`: integer, the id of the parent token. The root token has
#'   parent `NA`
#' * `field_name`: character, the field name of the token in its parent.
#' * `type`: character, the type of the token.
#' * `code`: character, the actual code of the token.
#' * `start_byte`, `end_byte`: integer, the byte positions of the token
#'   in the input.
#' * `start_row`, `start_column`, `end_row`, `end_column`: integer, the
#'   position of the token in the input.
#' * `is_missing`: logical, whether the token is a missing token added by
#'   the parser to recover from errors.
#' * `has_error`: logical, whether the token has a parse error.
#' * `children`: list of integer vectors, the ids of the children tokens.
#' * `dom_type`: character, the type of the node in the DOM tree. See
#'   \code{\link[ts:ts_tree_dom]{ts_tree_dom()}}. Nodes that are not part
#'   of the DOM tree have `NA_character_` here.
#' * `dom_children`: list of integer vectors, the ids of the children in the
#'   DOM tree. See \code{\link[ts:ts_tree_dom]{ts_tree_dom()}}.
#' * `dom_parent`: integer, the parent of the node in the DOM tree. See
#'   \code{\link[ts:ts_tree_dom]{ts_tree_dom()}}. Nodes that are not part
#'   of the DOM tree and the document node have have `NA_integer_` here.
#'
#' Other, undocumented columns may also be present, these are considered
#' internal and may change without notice.
#' @return \eval{ts:::doc_insert("ts::ts_tree_brackets_return")}
#'
#' @name ts_tree-brackets
#' @family ts_tree exploration
#' @seealso \eval{ts:::doc_seealso("[")}
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc('{"foo": 42, "bar": [1, 2, 3]}')
#'
#' tree
#'
#' tree[]

`[.ts_tree` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "ts_tree")
  requireNamespace("pillar", quietly = TRUE)
  NextMethod("[")
}

#' @export

`$.ts_tree` <- function(x, name) {
  class(x) <- setdiff(class(x), "ts_tree")
  NextMethod(`$`)
}
