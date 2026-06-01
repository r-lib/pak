#' Raw bytes of a document of a tree-sitter tree
#'
#' @param x A `ts_tree` object.
#' @return A raw vector containing the bytes of the document of the tree.
#'
#' @export
#' @seealso [as.character.ts_tree()] to get the document as a character scalar.
#' @examples
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc('{"foo": 42, "bar": [1, 2, 3]}')
#'
#' tree
#' as.raw(tree)

as.raw.ts_tree <- function(x) {
  attr(x, "text")
}

#' The document of a tree-sitter tree as a character scalar
#'
#' @param x A `ts_tree` object.
#' @param ... Ignored.
#' @return A character scalar containing the document of the tree.
#'
#' @export
#' @seealso [as.raw.ts_tree()] to get the document as a raw vector.
#' @examples
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc('{"foo": 42, "bar": [1, 2, 3]}')
#'
#' tree
#' as.character(tree)

as.character.ts_tree <- function(x, ...) {
  rawToChar(as.raw(x))
}
