#' Create tree-sitter tree from file or string
#'
#' @ts ts_tree_new_description
#' The result is a `ts_tree` object. A `ts_tree` object may be queried,
#' edited, formatted, written to file, etc. using `ts_tree` methods.
#' @description
#' This is the main function to create a tree-sitter parse tree, using a
#' ts parser implemented in another package.
#' \eval{ts:::doc_insert("ts_tree_new_description")}
#'
#' \eval{ts:::format_rd_parser_list(ts:::ts_list_parsers())}
#'
#' @ts ts_tree_new_details
#' \eval{ts:::doc_tabs("ts_tree_new_examples")}
#'
#' @details
#' A package that implements a tree-sitter parser provides a function that
#' creates a `ts_language` object for that parser. E.g.
#' [tsjsonc][tsjsonc::tsjsonc-package] has [tsjsonc::ts_language_jsonc()].
#' You need to use the returned `ts_language` object as the `language`
#' argument of \code{\link[ts:ts_tree_new]{ts_tree_new()}}.
#'
#' \eval{ts:::doc_insert("ts_tree_new_details")}
#' \eval{ts:::doc_extra()}
#'
#' @ts ts_tree_new_param_language
#' Language of the file or string, a `ts_language` object,e.g. the return
#' value of [tsjsonc::ts_language_jsonc()].
#' @ts ts_tree_new_param_file
#' Path of a file to parse. Use either `file` or `text`, but not both.
#' @ts ts_tree_new_param_text
#' String to parse. Use either `file` or `text`, but not both.
#' @ts ts_tree_new_param_ranges
#' Can be used to parse part(s) of the input. It must be a data frame with
#' integer columns `start_row`, `start_col`, `end_row`, `end_col`,
#' `start_byte`, `end_byte`, in this order.
#' @ts ts_tree_new_param_fail_on_parse_error
#' Logical, whether to error if there are parse errors in the document.
#' Default is `TRUE`.
#'
#' @param language
#' \eval{ts:::doc_insert("ts::ts_tree_new_param_language", "ts")}
#' @param file
#' \eval{ts:::doc_insert("ts::ts_tree_new_param_file", "ts")}
#' @param text
#' \eval{ts:::doc_insert("ts::ts_tree_new_param_text", "ts")}
#' @param ranges
#' \eval{ts:::doc_insert("ts::ts_tree_new_param_ranges", "ts")}
#' @param fail_on_parse_error
#' \eval{ts:::doc_insert("ts::ts_tree_new_param_fail_on_parse_error", "ts")}
#' @param ... Additional arguments for methods.
#'
#' @ts ts_tree_new_return
#' A `ts_tree` object representing the parse tree of the input. You can
#' use the single bracket \code{\link[ts::ts_tree-brackets]{`[`}}
#' operator to convert it to a data frame.
#' @return
#' \eval{ts:::doc_insert("ts::ts_tree_new_return", "ts")}
#'
#' @export
#' @family ts_tree generics
#' @seealso The tree-sitter parser packages typically include shortcuts to
#'   create parse trees from strings and file, e.g.
#'   [tsjsonc::ts_parse_jsonc()] and [tsjsonc::ts_read_jsonc()].
#'
#'   \eval{ts:::doc_seealso("ts_tree_new")}
#'
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#'
#' # JSONC example, needs the tsjsonc package -----------------------------
#' json <- ts_tree_new(
#'   tsjsonc::ts_language_jsonc(),
#'   text = '{ "a": 1, "b": 2 }'
#' )
#'
#' json
#'
#' json |> ts_tree_format()
#'
#' @examplesIf requireNamespace("tstoml", quietly = TRUE)
#'
#' # TOML example, needs the tstoml package -------------------------------
#' toml <- ts_tree_new(
#'   tstoml::ts_language_toml(),
#'   text = '[section]\nkey = "value"\nnumber = 42\n'
#' )
#'
#' toml
#'
#' toml |> ts_tree_format()

ts_tree_new <- function(
  language,
  file = NULL,
  text = NULL,
  ranges = NULL,
  fail_on_parse_error = TRUE,
  ...
) {
  UseMethod("ts_tree_new")
}

#' @export

ts_tree_new.ts_language <- function(
  language,
  file = NULL,
  text = NULL,
  ranges = NULL,
  fail_on_parse_error = TRUE,
  ...
) {
  if (is.null(text) + is.null(file) != 1) {
    stop(ts_cnd(
      "Invalid arguments in `ts_tree_new()`: exactly one of `file` \\
       and `text` must be given."
    ))
  }
  if (is.null(text)) {
    text <- readBin(file, "raw", n = file.size(file))
  }
  if (is.character(text)) {
    text <- charToRaw(paste(text, collapse = "\n"))
  }

  tree <- call_with_cleanup(c_parse, text, language, ranges)

  lvls <- seq_len(nrow(tree))
  tree$children <- I(unname(split(
    lvls,
    factor(tree$parent, levels = lvls)
  )))

  # trailing whitespace for each token
  # first we add the leading whitespace to the document token
  # this way printing $code and $tws will print the whole document
  tree$tws <- rep("", nrow(tree))
  if ((lead <- tree$start_byte[1]) > 0) {
    tree$tws[1] <- rawToChar(text[1:lead])
  }

  # then the whitespace of the terminal nodes
  term <- which(!is.na(tree$code))
  from <- tree$end_byte[term] + 1L
  to <- c(tree$start_byte[term][-1], tree$end_byte[1])
  for (i in seq_along(term)) {
    if (from[i] <= to[i]) {
      tree$tws[term[i]] <- rawToChar(text[from[i]:to[i]])
    }
  }

  attr(tree, "text") <- text
  attr(tree, "file") <- if (!is.null(file)) normalizePath(file)
  cls <- sub("^ts_language_", "ts_tree_", class(language)[1])
  class(tree) <- c(cls, "ts_tree", class(tree))

  if (fail_on_parse_error && (tree$has_error[1] || any(tree$is_missing))) {
    stop(ts_parse_error_cnd(tree = tree, text = text))
  }

  language <- structure(
    list(language = language, tree = tree),
    class = class(tree)
  )

  tree
}
