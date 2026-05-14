#' Show the annotated syntax tree of a tree-sitter tree
#'
#' @ts ts_tree_ast_description
#' `ts_tree_ast()` prints the annotated syntax tree of a ts_tree object.
#' This syntax tree contains all tree-sitter nodes, and it shows the
#' source code associated with each node, along with line numbers.
#' @description
#' \eval{ts:::doc_insert("ts_tree_ast_description")}
#'
#' \eval{ts:::format_rd_parser_list(ts:::ts_list_parsers(), "ts_tree_ast")}
#'
#' @ts ts_tree_ast_details
#' ## The syntax tree and the DOM tree
#'
#' This syntax tree contains all nodes of the tree-sitter parse tree,
#' including both named and unnamed nodes and comments. E.g. for a JSON(C)
#' document it includes the pairs, brackets, braces, commas, colons,
#' double quotes and string escape sequences as separate nodes.
#'
#' </p><p><!-- ------------------------------------------------------>
#'
#' See \code{\link[ts:ts_tree_dom]{ts::ts_tree_dom()}} for a tree that
#' shows the semantic structure of the parsed document, which may be
#' different from the syntax tree.
#'
#' \eval{ts:::doc_tabs("ts_tree_ast_details_syntax_vs_dom")}
#'
#' @details
#' \eval{ts:::doc_insert("ts::ts_tree_ast_details")}
#' \eval{ts:::doc_extra()}
#'
#' @ts ts_tree_ast_param_tree
#' A `ts_tree` object.
#'
#' @param tree \eval{ts:::doc_insert("ts::ts_tree_ast_param_tree")}
#'
#' @ts ts_tree_ast_return
#' Character vector, the formatted annotated syntax tree, line by
#' line. It has class [cli_tree][cli::tree()], from the cli package. It
#' may contain ANSI escape sequences for coloring and hyperlinks.
#' @return \eval{ts:::doc_insert("ts::ts_tree_ast_return")}
#'
#' @family ts_tree exploration
#' @family ts_tree generics
#' @seealso [ts_tree_dom()] to show the document object model (DOM) of a
#'   ts_tree object.
#'
#'  \eval{ts:::doc_seealso("ts_tree_ast")}
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

ts_tree_ast <- function(tree) {
  UseMethod("ts_tree_ast")
}

#' @export

ts_tree_ast.default <- function(tree) {
  type <- tree$type
  fn <- attr(tree, "file")
  if (cli::ansi_has_hyperlink_support() && !is.null(fn)) {
    type <- cli::style_hyperlink(
      type,
      sprintf(
        "file://%s:%d:%d",
        normalizePath(fn, mustWork = NA),
        tree$start_row + 1L,
        tree$start_column + 1
      )
    )
  }

  linum <- tree$start_row + 1
  linum <- ifelse(duplicated(linum), "", as.character(linum))
  linum <- format(linum, justify = "right")
  # this is the spacer we need to put in for multi-line tokens
  nlspc <- paste0("\n\t", strrep(" ", nchar(linum[1])), "|")
  code <- ifelse(
    is.na(tree$code),
    "",
    paste0(strrep(" ", tree$start_column), tree$code)
  )

  # we put in a \t, and later use it to align the lines vertically
  treetab <- data_frame(
    id = as.character(tree$id),
    children = lapply(tree$children, as.character),
    label = paste0(
      type,
      " (",
      tree$id,
      ")",
      "\t",
      linum,
      "|",
      gsub("\n", nlspc, code, fixed = TRUE)
    )
  )
  tree <- cli::tree(treetab)

  # align lines vertically. the size of the alignment is measured
  # without the ANSI sequences, but then the substitution uses the
  # full ANSI string
  tabpos <- regexpr("\t", cli::ansi_strip(tree), fixed = TRUE)
  maxtab <- max(tabpos)
  tabpos2 <- regexpr("\t", tree, fixed = TRUE)
  regmatches(tree, tabpos2) <- strrep(" ", maxtab - tabpos + 4)

  tree
}
