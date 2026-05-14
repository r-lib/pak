#' Run tree-sitter queries on tree-sitter trees
#'
#' @ts ts_tree_query_description
#' Use \href{https://tree-sitter.github.io/tree-sitter/}{
#' tree-sitter's query language} to find nodes in a tree-sitter tree.
#' @description
#' \eval{ts:::doc_insert("ts_tree_query_description")}
#' \eval{ts:::format_rd_parser_list(ts:::ts_list_parsers(), "ts_tree_query")}
#'
#' @ts ts_tree_query_details
#' You probably need to know some details about the specific tree-sitter
#' parser you are using, to write effective queries. See the documentation
#' of the parser package you are using for details about the node types
#' and the query language support. See links below.
#'
#' @details
#' \eval{ts:::doc_insert("ts::ts_tree_query_details")}
#' \eval{ts:::doc_tabs("ts_tree_query_details_examples")}
#' \eval{ts:::doc_extra()}
#'
#' @ts ts_tree_query_param_tree
#' A `ts_tree` object.
#' @ts ts_tree_query_param_query
#' Character string, the tree-sitter query to run.
#'
#' @param tree \eval{ts:::doc_insert("ts::ts_tree_query_param_tree")}
#' @param query \eval{ts:::doc_insert("ts::ts_tree_query_param_query")}
#'
#' @ts ts_tree_query_return
#' A list with entries `patterns` and `matched_captures`.
#'
#' </p><p><!-- ------------------------------------------------------>
#'
#' `patterns` contains information about all patterns in the queries and
#' it is a data frame with columns: `id`, `name`, `pattern`, `match_count`.
#'
#' </p><p><!-- ------------------------------------------------------>
#'
#' `matched_captures` contains information about all matches, and it has
#' columns `id`, `pattern`, `match`, `start_byte`, `end_byte`, `start_row`,
#' `start_column`, `end_row`, `end_column`, `name`, `code`.
#'
#' </p><p><!-- ------------------------------------------------------>
#'
#' The `pattern` column of `matched_captured` refers to the `id` column of
#' `patterns`.
#' @return \eval{ts:::doc_insert("ts::ts_tree_query_return")}
#'
#' @seealso [ts_tree_select()] to select the nodes matching a query.
#'
#'  \eval{ts:::doc_seealso("ts_tree_query")}
#' @export
#' @family ts_tree exploration
#' @family ts_tree generics
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Select all numbers in a JSONC document ------------------------------------
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": 100 } }'
#' )
#' json |> ts_tree_query("(number) @number")

ts_tree_query <- function(tree, query) {
  UseMethod("ts_tree_query")
}

#' @export

ts_tree_query.default <- function(tree, query) {
  qlen <- nchar(query, type = "bytes") + 1L # + \n
  qbeg <- c(1L, cumsum(qlen))
  qnms <- names(query) %||% rep(NA_character_, length(query))
  query1 <- paste0(query, "\n", collapse = "")

  res <- call_with_cleanup(c_code_query, tree, query1)

  qorig <- as.integer(cut(res[[1]][[3]], breaks = qbeg, include.lowest = TRUE))
  list(
    patterns = data_frame(
      id = seq_along(res[[1]][[1]]),
      name = qnms[qorig],
      pattern = res[[1]][[1]],
      match_count = res[[1]][[2]]
    ),
    captures = data_frame(
      id = seq_along(res[[2]]),
      name = res[[2]]
    ),
    matched_captures = data_frame(
      id = map_int(res[[3]], "[[", 3L),
      pattern = map_int(res[[3]], "[[", 1L),
      match = map_int(res[[3]], "[[", 2L),
      type = map_chr(res[[3]], "[[", 12L),
      start_byte = map_int(res[[3]], "[[", 6L),
      end_byte = map_int(res[[3]], "[[", 7L),
      start_row = map_int(res[[3]], "[[", 8L),
      start_column = map_int(res[[3]], "[[", 9L),
      end_row = map_int(res[[3]], "[[", 10L),
      end_column = map_int(res[[3]], "[[", 11L),
      name = map_chr(res[[3]], "[[", 4L),
      code = map_chr(res[[3]], "[[", 5L)
    )
  )
}
