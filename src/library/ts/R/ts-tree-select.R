#' @ts ts_tree_select_details
#' The selection process is iterative. Selection expressions (selectors)
#' are applied one by one, and each selector selects nodes from the
#' currently selected nodes. For each selector, it is applied individually
#' to each currently selected node, and the results are concatenated.
#'
#' </p><p><!-- ------------------------------------------------------>
#'
#' The selection process starts from the root of the DOM tree, the document
#' node (see \code{\link[ts:ts_tree_dom]{ts_tree_dom()}}), unless
#' `refine = TRUE` is set, in which case it starts from the current
#' selection.
#'
#' </p><p><!-- ------------------------------------------------------>
#'
#' See the various types of selection expressions below.
#'
#' ## Selectors
#'
#' ### All elements: `TRUE`
#'
#' Selects all child nodes of the current nodes.
#'
#' \eval{ts:::doc_tabs("ts_tree_select_true")}
#'
#' ### Specific keys: character vector
#'
#' Selects child nodes with the given names from nodes with named children.
#' If a node has no named children, it selects nothing from that node.
#'
#' \eval{ts:::doc_tabs("ts_tree_select_character")}
#'
#' ### By position: integer vector
#'
#' Selects child nodes by position. Positive indices count from the start,
#' negative indices count from the end. Zero indices are not allowed.
#'
#' \eval{ts:::doc_tabs("ts_tree_select_integer")}
#'
#' ### Matching keys: regular expression
#'
#' A character scalar named `regex` can be used to select child nodes
#' whose names match the given regular expression, from nodes with named
#' children. If a node has no named children, it selects nothing from that
#' node.
#'
#' \eval{ts:::doc_tabs("ts_tree_select_regex")}
#'
#' ### Tree sitter query matches
#'
#' A character scalar named `query` can be used to select nodes matching
#' a tree-sitter query. See \code{\link[ts:ts_tree_query]{ts_tree_query()}}
#' for details on tree-sitter queries.
#'
#' </p><p><!-- ------------------------------------------------------>
#'
#' Instead of a character scalar this can also be a two-element list, where
#' the first element is the query string and the second element is a
#' character vector of capture names to select. In this case only nodes
#' matching the given capture names will be selected.
#'
#' \eval{ts:::doc_tabs("ts_tree_select_tsquery")}
#'
#' ### Explicit node ids
#'
#' You can use `I(c(...))` to select nodes by their ids directly. This is
#' for advanced use cases only.
#'
#' \eval{ts:::doc_tabs("ts_tree_select_ids")}
#'
#' ## Refining selections
#'
#' If the `refine` argument of
#' \code{\link[ts:ts_tree_select]{ts_tree_select()}} is `TRUE`, then
#' the selection starts from the already selected elements (all of them
#' simultanously), instead of starting from the document element.
#'
#' \eval{ts:::doc_tabs("ts_tree_select_refine")}
#'
#' ## The `ts_tree_select<-()` replacement function
#'
#' The \code{\link[ts:ts_tree_select<-]{ts_tree_select<-()}} replacement
#' function works similarly to the combination of
#' \code{\link[ts:ts_tree_select]{ts_tree_select()}} and
#' \code{\link[ts:ts_tree_update]{ts_tree_update()}}, but it might be more
#' readable.
#'
#' \eval{ts:::doc_tabs("ts_tree_select_set")}
#'
#' ## The `[[` and `[[<-` operators
#
#' The `[[` operator works similarly to the combination of
#' \code{\link[ts:ts_tree_select]{ts_tree_select()}} and
#' \code{\link[ts:ts_tree_unserialize]{ts_tree_unserialize()}}, but it
#' might be more readable.
#'
#' \eval{ts:::doc_tabs("ts_tree_select_brackets")}
#'
#' The `[[<-` operator works similarly to the combination of
#' \code{\link[ts:ts_tree_select]{ts_tree_select()}} and
#' \code{\link[ts:ts_tree_update]{ts_tree_update()}}, (and also to the
#' replacement function \code{\link[ts:ts_tree_select<-]{ts_tree_select<-()}}),
#' but it might be more readable.
#'
#' \eval{ts:::doc_tabs("ts_tree_select_brackets_set")}
NULL

#' Select elements of a tree-sitter tree
#'
#' @ts ts_tree_select_description
#' This function is the heart of ts. To edit a tree-sitter tree, you first
#' need to select the parts you want to delete or update.
#' @description
#' \eval{ts:::doc_insert("ts::ts_tree_select_description")}
#'
#' \eval{ts:::format_rd_parser_list(ts:::ts_list_parsers(), "ts_tree_select")}
#'
#' @details
#' \eval{ts:::doc_insert("ts::ts_tree_select_details")}
#' \eval{ts:::doc_extra()}
#'
#' @ts ts_tree_select_param_tree
#' A `ts_tree` object as returned by
#' \code{\link[ts:ts_tree_new]{ts_tree_new()}}.
#' @ts ts_tree_select_param_dots
#' Selection expressions, see details.
#' @ts ts_tree_select_param_refine
#' Logical, whether to refine the current selection or start
#' a new selection.
#' @param tree \eval{ts:::doc_insert("ts_tree_select_param_tree")}
#' @param ... \eval{ts:::doc_insert("ts_tree_select_param_dots")}
#' @param refine \eval{ts:::doc_insert("ts_tree_select_param_refine")}
#' @ts ts_tree_select_return
#' A `ts_tree` object with the selected parts.
#' @return \eval{ts:::doc_insert("ts::ts_tree_select_return")}
#'
#' @family ts_tree generics
#' @seealso \eval{ts:::doc_seealso("ts_tree_select")}
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # ----------------------------------------------------------------------
#' # Create a JSONC tree, needs the tsjsonc package
#' json <- ts_tree_new(
#'   tsjsonc::ts_language_jsonc(),
#'   text = '{ "a": 1, "b": 2, "c": { "d": 3, "e": 4 } }'
#' )
#'
#' json |> ts_tree_select("c", "d")
#'
#' @examplesIf requireNamespace("tstoml", quietly = TRUE)
#'
#' # ----------------------------------------------------------------------
#' # Create a TOML tree, needs the tstoml package
#' toml <- ts_tree_new(
#'   tstoml::ts_language_toml(),
#'   text = tstoml::toml_example_text()
#' )
#'
#' toml |> ts_tree_select("servers", TRUE, "ip")

ts_tree_select <- function(tree, ..., refine = FALSE) {
  slts <- normalize_selectors(tree, list(...))
  if (length(slts) == 1 && is.null(slts[[1]])) {
    attr(tree, "selection") <- NULL
    return(tree)
  }
  current <- if (refine) {
    ts_tree_selection(tree)
  } else {
    ts_tree_selector_default(tree)
  }
  cnodes <- current[[length(current)]]$nodes

  for (idx in seq_along(slts)) {
    slt <- slts[[idx]]
    nxt <- integer()
    for (cur in cnodes) {
      nxt <- unique(c(nxt, ts_tree_select1(tree, cur, slt)))
    }
    current[[length(current) + 1L]] <- list(
      selector = slt,
      nodes = sort(nxt)
    )
    cnodes <- current[[length(current)]]$nodes
  }
  attr(tree, "selection") <- current
  tree
}

normalize_selectors <- function(tree, slts) {
  names(slts) <- names(slts) %||% rep("", length(slts))
  slts <- imap(slts, function(x, nm) {
    if (nm == "regex") {
      x <- structure(
        list(pattern = x),
        class = c("ts_tree_selector_regex", "ts_tree_selector", "list")
      )
    } else if (nm == "query") {
      # we do the query up front, so we don't rerun it for every node
      x <- structure(
        list(
          query = if (is.character(x)) x else x[[1]],
          captures = if (!is.character(x)) x[[2]] else NULL
        ),
        class = c("ts_tree_selector_tsquery", "ts_tree_selector", "list")
      )
      x$nodes <- select_query(tree, x$query, x$captures)
    } else if (inherits(x, "AsIs")) {
      x <- structure(
        list(ids = unclass(x)),
        class = c("ts_tree_selector_ids", "ts_tree_selector", "list")
      )
    }
    x <- if (inherits(x, "ts_tree_selector") || !is.list(x)) list(x) else x
    x
  })
  unlist(slts, recursive = FALSE, use.names = FALSE)
}

#' Select nodes from a tree-sitter tree (internal)
#'
#' This function is for packages implementing new parsers based on the ts
#' package. It is very unlikely that you will need to call this function
#' directly.
#'
#' A parser package may implement methods for this generic to change the
#' behavior of \code{\link[ts:ts_tree_select]{ts_tree_select()}} for a
#' certain selector type, or even add new selector types.
#'
#' Each new method should be named as
#' ```
#' ts_tree_select.<ts_tree_class>.<selector_class>
#' ```
#'
#' The ts package implement deault methods for the selector types described
#' in the \code{\link[ts:ts_tree_select]{ts_tree_select()}} manual page.
#'
#' @param tree A `ts_tree` object as returned by [ts_tree_new()].
#' @param node Integer scalar, the node id to select from.
#' @param slt A selector object, see details in [ts_tree_select()].
#'
#' @return Must return an integer vector of selected node ids.
#'
#' @export
#' @examples
#' # This is an internal generic for parser implementations, see the
#' # tsjsonc and tstoml packages for examples of methods implementing
#' # selector types.

ts_tree_select1 <- function(tree, node, slt) {
  treesel <- structure(
    list(tree = tree, slt = slt),
    class = paste0(class(tree), ".", class(slt)[1])
  )
  UseMethod("ts_tree_select1", treesel)
}

#' @export

ts_tree_select1.default <- function(tree, node, slt) {
  lang <- toupper(get_tree_lang(tree))
  stop(ts_cnd(
    "Don't know how to select nodes from a `ts_tree` ({lang}) object \\
     using selector of class `{class(slt)[1]}`."
  ))
}

#' @rdname ts_tree_select1
#' @details
#' ## `ts_tree_selector_default` selector
#'
#' Method: `ts_tree_select1.ts_tree.ts_tree_selector_default`
#'
#' This method is used to select the default element(s), when there is no
#' selected element. E.g. when starting a new selection from the root of
#' the DOM tree.
#'
#' The default implementation returns the ids of all children of the
#' document root in the AST, except comments. If there are no such
#' children, it returns the id of the document root of the AST itself
#' (always id 1).
#' @export

ts_tree_select1.ts_tree.ts_tree_selector_default <- function(tree, node, slt) {
  top <- tree$children[[1]]
  top <- top[tree$type[top] != "comment"]
  if (length(top) > 0) top else 1L
}

#' @rdname ts_tree_select1
#' @details
#' ## `NULL` selector
#'
#' Method: `ts_tree_select1.ts_tree.NULL`
#'
#' This method is used for the  `NULL` selector, that is supposed to
#' clear the selection. You probably do not need to override this method.
#' The default implementation returns an empty integer vector.
#' @export

ts_tree_select1.ts_tree.NULL <- function(tree, node, slt) {
  integer()
}

#' @rdname ts_tree_select1
#' @details
#' ## `ts_tree_selector_ids` selector
#'
#' Method: `ts_tree_select1.ts_tree.ts_tree_selector_ids`
#'
#' This method is used to select nodes by their ids directly.
#' You probably do not need to override this method.
#' The default implementation returns the ids stored in the selector.
#'
#' ### Note
#'
#' This behaviour may change in the future to select only nodes
#' in the subtree of the current node.
#' @export

ts_tree_select1.ts_tree.ts_tree_selector_ids <- function(tree, node, slt) {
  # TODO: should we select in subtree of node? Probably.
  slt$ids
}

#' @rdname ts_tree_select1
#' @details
#' ## `ts_tree_selector_tsquery` selector
#'
#' Method: `ts_tree_select1.ts_tree.ts_tree_selector_tsquery`
#'
#' This method is used to select nodes matching a tree-sitter query.
#' You probably do not need to override this method.
#' The default implementation returns the ids stored in the selector.
#'
#' ### Note
#'
#' This behaviour may change in the future to select only nodes
#' in the subtree of the current node.
#' @export

ts_tree_select1.ts_tree.ts_tree_selector_tsquery <- function(tree, node, slt) {
  # TODO: should we select in subtree of node? Probably.
  slt$nodes
}

#' @rdname ts_tree_select1
#' @details
#' ## `character` (character vector) selector
#'
#' Method: `ts_tree_select1.ts_tree.character`
#'
#' This method is used when the selector is a character vector.
#' The default implementation selects DOM children of `node` whose names
#' are in the character vector. If not all children o `node` are named,
#' it returns an empty integer vector. (E.g. in a JSONC document it returns
#' an empty integer vector when nodes is an array.)
#' @export

ts_tree_select1.ts_tree.character <- function(tree, node, slt) {
  chdn <- tree$dom_children[[node]]
  if (!is_named(chdn)) {
    return(integer())
  }
  chdn[names(chdn) %in% slt]
}

#' @rdname ts_tree_select1
#' @details
#' ## `integer` (integer vector) selector
#'
#' Method: `ts_tree_select1.ts_tree.integer`
#'
#' This method is used when the selector is an integer vector.
#' The default implementation selects DOM children of `node` by position.
#' Positive indices count from the start, negative indices count from the
#' end. Zero indices are not allowed and an error is raised if any are
#' used.
#' @export

ts_tree_select1.ts_tree.integer <- function(tree, node, slt) {
  if (any(slt == 0)) {
    stop(ts_cnd("Zero indices are not allowed in ts selectors."))
  }
  chdn <- tree$dom_children[[node]]
  slt <- slt[slt <= length(chdn) & slt >= -length(chdn)]

  res <- integer(length(slt))
  pos <- slt >= 0
  if (any(pos)) {
    res[pos] <- chdn[slt[pos]]
  }
  if (any(!pos)) {
    res[!pos] <- rev(rev(chdn)[abs(slt[!pos])])
  }
  res
}

#' @rdname ts_tree_select1
#' @details
#' ## `numeric` (numeric, double vector) selector
#'
#' Method: `ts_tree_select1.ts_tree.numeric`
#'
#' This method is used when the selector is a numeric (double) vector.
#' It currrently coerces the numeric vector to integer and calls the
#' integer method.
#' @export

ts_tree_select1.ts_tree.numeric <- function(tree, node, slt) {
  ts_tree_select1.ts_tree.integer(tree, node, as.integer(slt))
}

#' @rdname ts_tree_select1
#' @details
#' ## `ts_tree_selector_regex` (regular expression) selector
#'
#' Method: `ts_tree_select1.ts_tree.ts_tree_selector_regex`
#'
#' This method is used when the selector is a regular expression.
#' The default implementation selects DOM children of `node` whose names
#' match the regular expression. If not all children o `node` are named,
#' it returns an empty integer vector. (E.g. in a JSONC document it
#' returns an empty integer vector when nodes is an array.)
#' @export

ts_tree_select1.ts_tree.ts_tree_selector_regex <- function(tree, node, slt) {
  chdn <- tree$dom_children[[node]]
  if (!is_named(chdn)) {
    return(integer())
  }
  chdn[grepl(slt$pattern, names(chdn))]
}

#' @rdname ts_tree_select1
#' @details
#' ## `logical` (logical vector) selector
#'
#' Method: `ts_tree_select1.ts_tree.logical`
#'
#' This method is used when the selector is a logical vector.
#' The default implementation only supports scalar `TRUE`, which selects
#' all DOM children of `node`. Other values raise an error.
#' @export

ts_tree_select1.ts_tree.logical <- function(tree, node, slt) {
  if (isTRUE(slt)) {
    tree$dom_children[[node]]
  } else {
    stop(ts_cnd(
      "Invalid logical selector in `ts_tree_select()`: only scalar `TRUE` is \\
       supported."
    ))
  }
}

#' Unserialize parts of a tree-sitter tree
#'
#' @ts ts_tree_double_bracket_description
#' The `[[` operator works similarly to the combination of
#' \code{\link[ts:ts_tree_select]{ts_tree_select()}} and
#' \code{\link[ts:ts_tree_unserialize]{ts_tree_unserialize()}}, but it
#' might be more readable.
#' @description
#' \eval{ts:::doc_insert("ts_tree_double_bracket_description")}
#'
#' @ts ts_tree_double_bracket_details
#' The following two expressions are equivalent:
#' ```r
#' ts_tree_select(tree, <selectors>) |> ts_tree_unserialize()
#' ```
#' and
#' ```r
#' tree[[list(<selectors>)]]
#' ```
#'
#' \eval{ts:::doc_tabs("ts_tree_select_brackets")}
#'
#' ## The `[[<-` replacement operator
#'
#' The `[[<-` operator works similarly to the combination of
#' \code{\link[ts:ts_tree_select]{ts_tree_select()}} and
#' \code{\link[ts:ts_tree_update]{ts_tree_update()}}, (and also to the
#' replacement function \code{\link[ts:ts_tree_select<-]{ts_tree_select<-()}}),
#' but it might be more readable.
#'
#' \eval{ts:::doc_tabs("ts_tree_select_brackets_set")}
#'
#' @details
#' \eval{ts:::doc_insert("ts::ts_tree_double_bracket_details")}
#' \eval{ts:::doc_extra()}
#'
#' @ts ts_tree_double_bracket_param_x
#' A `ts_tree` object.
#' @ts ts_tree_double_bracket_param_i
#' Selection expressions in a list, see details in
#' \code{\link[ts:ts_tree_select]{ts_tree_select()}}.
#' @ts ts_tree_double_bracket_param_dots
#' Additional arguments, passed to
#' \code{\link[ts:ts_tree_select]{ts_tree_select()}}.
#'
#'
#' @param x \eval{ts:::doc_insert("ts::ts_tree_double_bracket_param_x")}
#' @param i \eval{ts:::doc_insert("ts::ts_tree_double_bracket_param_i")}
#' @param ...
#' \eval{ts:::doc_insert("ts::ts_tree_double_bracket_param_dots")}
#'
#' @ts ts_tree_double_bracket_return
#' List of R objects, with one entry for each selected element.
#' @return \eval{ts:::doc_insert("ts::ts_tree_double_bracket_return")}
#'
#' @family ts_tree generics
#' @family serialization functions
#' @rdname double-bracket-ts-tree
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc('{"a": 13, "b": [1, 2, 3], "c": "x"}')
#'
#' tree
#'
#' tree[[list("a")]]
#'
#' # Last two elements of "b"
#' tree[[list("b", -(1:2))]]

`[[.ts_tree` <- function(x, i, ...) {
  if (missing(i)) {
    i <- list()
  }
  ts_tree_unserialize(ts_tree_select(x, i, ...))
}

select_query <- function(tree, query, captures = NULL) {
  mch <- ts_tree_query(tree, query)

  if (!is.null(captures)) {
    bad <- !captures %in% mch$captures$name
    if (any(bad)) {
      stop(ts_cnd(
        "Invalid capture names in `select_query()`: \\
         {ts_collapse(captures[bad])}."
      ))
    }
    mc <- mch$matched_captures[
      mch$matched_captures$name %in% captures,
    ]
  } else {
    mc <- mch$matched_captures
  }

  ids <- if (nrow(mc) == 0) {
    integer()
  } else {
    toml0 <- tree[
      tree$start_byte %in% mc$start_byte & tree$end_byte %in% mc$end_byte,
    ]
    mkeys <- paste0(mc$type, ":", mc$start_byte, ":", mc$end_byte)
    jkeys <- paste0(toml0$type, ":", toml0$start_byte, ":", toml0$end_byte)
    toml0$id[match(mkeys, jkeys)]
  }
  # TODO: should we do this?
  minimize_selection(tree, ids)
}

# remove nodes that are in the subtree of other selected nodes
# start from the last nodes and go up

minimize_selection <- function(tree, ids) {
  ids <- sort(unique(ids))
  sel <- logical(nrow(tree))
  sel[ids] <- TRUE
  for (id in rev(ids)) {
    parent <- tree$parent[id]
    while (!is.na(parent)) {
      if (sel[parent]) {
        sel[id] <- FALSE
        break
      }
      parent <- tree$parent[parent]
    }
  }
  which(sel)
}

# A section is a list of records. Each record has a selector
# and a list of selected nodes.
#
# 1. there is an explicit selection
# 2. otherwise the top element is selected (or elements if many)
# 3. otherwise the document node is selected

#' Helper functions for tree-sitter tree selections (internal)
#'
#' These functions are for packages implementing new parsers based on the ts
#' package. It is very unlikely that you will need to call these functions
#' directly.
#'
#' @param tree A `ts_tree` object as returned by [ts_tree_new()].
#' @param default Logical, whether to return the default selection if there
#'   is no explicit selection, or `NULL`.
#' @return `ts_tree_selection()` returns a list of selection records.
#'
#' @details `ts_tree_selection()` returns the current selection, as a list
#'   of selectors.
#'
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc('{"a": 13, "b": [1, 2, 3], "c": "x"}')
#' tree <- ts_tree_select(tree, "b", -1)
#' ts_tree_selection(tree)

ts_tree_selection <- function(tree, default = TRUE) {
  sel <- attr(tree, "selection")
  if (!is.null(sel)) {
    sel
  } else if (default) {
    ts_tree_selector_default(tree)
  } else {
    NULL
  }
}

#' @rdname ts_tree_selection
#' @return `ts_tree_selected_nodes()` returns the ids of the currently
#'   selected nodes.
#' @details `ts_tree_selected_nodes()` returns the ids of the currently
#'   selected nodes.
#' @export

ts_tree_selected_nodes <- function(tree, default = TRUE) {
  sel <- ts_tree_selection(tree, default = default)
  if (is.null(sel)) {
    return(integer())
  } else {
    sel[[length(sel)]]$nodes
  }
}

ts_tree_selector_default <- function(tree) {
  slt <- structure(
    list(),
    class = c("ts_tree_selector_default", "ts_tree_selector", "list")
  )
  list(list(
    selector = slt,
    nodes = ts_tree_select1(tree, NULL, slt)
  ))
}

#' Edit parts of a tree-sitter tree
#'
#' @ts ts_tree_select_set_description
#' The \code{\link[ts:ts_tree_select<-]{ts_tree_select<-()}} replacement
#' function works similarly to the combination of
#' \code{\link[ts:ts_tree_select]{ts_tree_select()}} and
#' \code{\link[ts:ts_tree_update]{ts_tree_update()}}, but
#' it might be more readable.
#' @description \eval{ts:::doc_insert("ts_tree_select_set_description")}
#'
#' @ts ts_tree_select_set_details
#' The following two expressions are equivalent:
#' ```r
#' tree <- ts_tree_select(tree, <selectors>) |> ts_tree_update(value)
#' ```
#' and
#' ```r
#' ts_tree_select(tree, <selectors>) <- value
#' ```
#' \eval{ts:::doc_tabs("ts_tree_select_set")}
#' @details \eval{ts:::doc_insert("ts::ts_tree_select_set_details")}
#' \eval{ts:::doc_extra()}
#'
#' @ts ts_tree_select_set_param_tree
#' A `ts_tree` object as returned by
#' \code{\link[ts:ts_tree_new]{ts_tree_new()}}.
#' @ts ts_tree_select_set_param_dots
#' Selection expressions, see \code{\link[ts:ts_tree_select]{ts_tree_select()}}.
#' @ts ts_tree_select_set_param_value
#' An R expression to serialize or
#' \code{\link[ts:ts_tree_deleted]{ts_tree_deleted()}}.
#' @param tree \eval{ts:::doc_insert("ts_tree_select_set_param_tree")}
#' @param ... \eval{ts:::doc_insert("ts_tree_select_set_param_dots")}
#' @param value \eval{ts:::doc_insert("ts_tree_select_set_param_value")}
#'
#' @ts ts_tree_select_set_return
#' A `ts_tree` object with the selected parts updated.
#' @return \eval{ts:::doc_insert("ts::ts_tree_select_set_return")}
#'
#' @name select-set
#' @rdname select-set
#' @family ts_tree generics
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc('{"a": 13, "b": [1, 2, 3], "c": "x"}')
#
#' tree
#'
#' ts_tree_select(tree, "a") <- 42
#' ts_tree_select(tree, "b", -1) <- ts_tree_deleted()
#'
#' tree

`ts_tree_select<-` <- function(tree, ..., value) {
  UseMethod("ts_tree_select<-")
}

#' @export

`ts_tree_select<-.ts_tree` <- function(tree, ..., value) {
  res <- if (inherits(value, "ts_tree")) {
    value # nocov
  } else if (inherits(value, "ts_tree_action_delete")) {
    ts_tree_delete(ts_tree_select(tree, ..., refine = TRUE))
  } else {
    ts_tree_update(ts_tree_select(tree, ..., refine = TRUE), value)
  }
  attr(res, "selection") <- NULL
  res
}

#' Edit parts of a tree-sitter tree
#'
#' @ts ts_tree_double_bracket_set_description
#' The `[[<-` operator works similarly to the combination of
#' \code{\link[ts:ts_tree_select]{ts_tree_select()}} and
#' \code{\link[ts:ts_tree_update]{ts_tree_update()}}, (and also to the
#' replacement function \code{\link[ts:ts_tree_select<-]{ts_tree_select<-()}}),
#' but it might be more readable.
#' @description
#' \eval{ts:::doc_insert("ts_tree_double_bracket_set_description")}
#'
#' @ts ts_tree_double_bracket_set_details
#' The following two expressions are equivalent:
#' ```r
#' tree <- ts_tree_select(tree, <selectors>) |> ts_tree_update(value)
#' ```
#' and
#' ```r
#' tree[[list(<selectors>)]] <- value
#' ```
#' \eval{ts:::doc_tabs("ts_tree_double_brackets_set")}
#' @details
#' \eval{ts:::doc_insert("ts::ts_tree_double_bracket_set_details")}
#' \eval{ts:::doc_extra()}
#'
#' @ts ts_tree_double_bracket_set_param_x
#' A `ts_tree` object.
#' @ts ts_tree_double_bracket_set_param_i
#' A list with selection expressions, see
#' \code{\link[ts:ts_tree_select]{ts_tree_select()}} for details.
#' @ts ts_tree_double_bracket_set_param_value
#' An R expression to serialize or
#' \code{\link[ts:ts_tree_deleted]{ts_tree_deleted()}}.
#'
#' @param x
#' \eval{ts:::doc_insert("ts::ts_tree_double_bracket_set_param_x")}
#' @param i
#' \eval{ts:::doc_insert("ts::ts_tree_double_bracket_set_param_i")}
#' @param value
#' \eval{ts:::doc_insert("ts::ts_tree_double_bracket_set_param_value")}
#'
#' @ts ts_tree_double_bracket_set_return
#' The modified `ts_tree` object.
#' @return \eval{ts:::doc_insert("ts::ts_tree_double_bracket_set_return")}
#' @rdname double-bracket-set-ts-tree
#' @family ts_tree generics
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' tree <- tsjsonc::ts_parse_jsonc('{"a": 13, "b": [1, 2, 3], "c": "x"}')
#'
#' tree
#'
#' tree[[list("a")]] <- 42
#' tree[[list("b", -1)]] <- ts_tree_deleted()
#'
#' tree

`[[<-.ts_tree` <- function(x, i, value) {
  # nocov start -- not sure if still is still needed, if [[ are not nested
  if (missing(i)) {
    i <- list()
  }
  # nocov end
  res <- if (inherits(value, "ts_tree")) {
    value # nocov
  } else if (inherits(value, "ts_tree_action_delete")) {
    ts_tree_delete(ts_tree_select(x, i, refine = TRUE))
  } else {
    ts_tree_update(ts_tree_select(x, i, refine = TRUE), value)
  }
  attr(res, "selection") <- NULL
  res
}

#' @rdname select-set
#' @usage NULL
#' @details
#' `ts_tree_deleted()` is a special marker to delete elements from a
#' ts_tree object with `ts_tree_select<-` or the double bracket operator.
#'
#' @return `ts_tree_deleted()` returns a marker object to be used at the right
#'   hand side of the `ts_tree_select<-` or the double bracket replacement
#'   functions, see examples below.
#'
#' @export

ts_tree_deleted <- function() {
  structure(
    list(),
    class = c("ts_tree_action_delete", "ts_tree_action", "list")
  )
}
