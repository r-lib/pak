#' Print a tree-sitter tree
#'
#' @ts print_description
#' Print a `ts_tree` object to the screen.
#' @description \eval{ts:::doc_insert("ts::print_description")}
#'
#' @ts print_details
#' Calls \code{\link[ts:format.ts_tree]{format.ts_tree()}} to format the
#' ts_tree object, writes the formatted object to the standard output, and
#' returns the original object invisibly.
#'
#' \eval{ts:::doc_tabs("print_examples")}
#' @details \eval{ts:::doc_insert("ts::print_details")}
#'
#' @ts print_param_x
#' `ts_tree` object to print.
#' @ts print_param_dots
#' Not used currently.
#' @param x \eval{ts:::doc_insert("ts::print_param_x")}
#' @param n \eval{ts:::doc_insert("ts::format_param_n")}
#' @param ... \eval{ts:::doc_insert("ts::print_param_dots")}
#'
#' @ts print_return
#' Invisibly returns the original `ts_tree` object.
#' @return \eval{ts:::doc_insert("ts::print_return")}
#'
#' @family ts_tree generics
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": 100 } }'
#' )
#' print(json)

print.ts_tree <- function(x, n = 10, ...) {
  writeLines(format(x, n = n, ...))
  invisible(x)
}

#' Format tree-sitter trees
#'
#' @ts format_description
#' Format a `ts_tree` object for printing.
#' @description \eval{ts:::doc_insert("ts::format_description")}
#'
#' @ts format_details
#' This is the engine of \code{\link[ts:print.ts_tree]{print.ts_tree()}},
#' possibly useful to obtain a printed representation without doing the
#' actual printing.
#'
#' </p><p><!-- ------------------------------------------------------>
#'
#' If there are selected nodes in the tree, those will be highlighted
#' in the output. See \code{\link[ts:ts_tree_select]{ts_tree_select()}} to
#' select nodes in a tree.
#'
#' </p><p><!-- ------------------------------------------------------>
#'
#' \eval{ts:::doc_tabs("format_examples")}
#'
#' @details \eval{ts:::doc_insert("ts::format_details")}
#' \eval{ts:::doc_extra()}
#'
#' @ts format_param_x
#' `ts_tree` object.
#' @ts format_param_n
#' Number of lines, or number of selections to print.
#' @ts format_param_dots
#' Currently ignored.
#' @param x \eval{ts:::doc_insert("ts::format_param_x")}
#' @param n \eval{ts:::doc_insert("ts::format_param_n")}
#' @param ... \eval{ts:::doc_insert("ts::format_param_dots")}
#'
#' @ts format_return
#' Character vector of lines to print.
#' @return \eval{ts:::doc_insert("ts::format_return")}
#'
#' @family ts_tree generics
#' @export
#' @examplesIf requireNamespace("tsjsonc", quietly = TRUE)
#' # Create a parse tree with tsjsonc -------------------------------------
#' json <- tsjsonc::ts_parse_jsonc(
#'   '{ "a": 1, "b": [10, 20, 30], "c": { "c1": true, "c2": 100 } }'
#' )
#' format(json)

format.ts_tree <- function(x, n = 10, ...) {
  sel <- ts_tree_selected_nodes(x, default = FALSE)
  if (length(sel) > 0) {
    format_ts_tree_selection(x, n = n, ...)
  } else {
    format_ts_tree_noselection(x, n = n, ...)
  }
}

format_ts_tree_noselection <- function(x, n = 10, ...) {
  lns <- strsplit(rawToChar(attr(x, "text")), "\r?\n")[[1]]
  nc <- length(lns)
  sc <- min(nc, n)
  lns <- utils::head(lns, sc)
  num <- cli::col_grey(format(seq_len(sc)))

  sel <- ts_tree_selection(x, default = FALSE)

  grey <- cli::col_grey

  sfn <- if (!is.null(attr(x, "file"))) paste0(basename(attr(x, "file")), ", ")
  lang <- sub("^ts_tree_", "", grep("^ts_tree_", class(x), value = TRUE))

  c(
    if (is.null(sel)) {
      grey(glue("# {lang} ({sfn}{nc} line{plural(nc)})"))
    } else {
      grey(glue("# {lang} ({sfn}{nc} line{plural(nc)}, 0 selected elements)"))
    },
    paste0(num, if (sc) cli::col_grey(" | "), lns),
    if (nc > sc) {
      c(
        grey(glue("{cli::symbol$info} {nc-sc} more line{plural(nc-sc)}")),
        grey(glue("{cli::symbol$info} Use `print(n = ...)` to see more lines"))
      )
    }
  )
}

ts_tree_mark_selection <- function(tree, node) {
  unlist(lapply(node, ts_tree_mark_selection1, tree = tree))
}

#' Helper function to decide which AST nodes to highlight for a selection
#' (internal)
#'
#' This function are for packages implementing new parsers based on the ts
#' package. It is very unlikely that you will need to call this function
#' directly.
#'
#' In parsers where AST nodes do not correspond one-to-one to DOM nodes
#' it is useful to highlight multiple AST nodes for a single selected
#' DOM node. This generic function can be overridden in such parsers to
#' return multiple AST node ids for a single selected (DOM) node id.
#'
#' @param tree Tree-sitter tree.
#' @param node Node id, integer scalar.
#' @return Integer vector of node ids to highlight.
#' @export
#' @examples
#' # This is an internal generic for parser implementations, see the
#' # tsjsonc and tstoml packages for examples of methods implementing
#' # custom behavior.

ts_tree_mark_selection1 <- function(tree, node) {
  UseMethod("ts_tree_mark_selection1")
}

#' @rdname ts_tree_mark_selection1
#' @details
#' The default implementation simply returns the input node id.
#' @export

ts_tree_mark_selection1.ts_tree <- function(tree, node) {
  node
}

format_ts_tree_selection <- function(x, n = n, context = 3, ...) {
  lns <- strsplit(rawToChar(attr(x, "text")), "\r?\n")[[1]]
  nlns <- length(lns)
  num <- seq_along(lns)
  sel <- ts_tree_selected_nodes(x, default = FALSE)
  nsel <- length(sel)
  ssel <- min(nsel, n)
  sel <- utils::head(sel, ssel)
  isel <- ts_tree_mark_selection(x, sel)

  # calculate the lines affected by the first ssel selections
  selrows <- rep(FALSE, nlns)
  shwrows <- rep(FALSE, nlns)
  for (sel1 in isel) {
    beg <- x$start_row[sel1] + 1L
    end <- x$end_row[sel1] + 1L
    end <- x$end_row[sel1] + 1L
    if (x$end_column[sel1] == 0) {
      end <- end - 1L
    }
    selrows[beg:end] <- TRUE
    sbeg <- max(1, beg - context)
    send <- min(nlns, end + context)
    shwrows[sbeg:send] <- TRUE
  }

  # now highlight the selected elements
  mark <- rep("  ", nlns)
  for (sel1 in isel) {
    beg <- x$start_row[sel1] + 1L
    end <- x$end_row[sel1] + 1L
    endcol <- x$end_column[sel1]
    if (x$end_column[sel1] == 0) {
      end <- end - 1L
      endcol <- cli::ansi_nchar(lns[end], type = "bytes")
    }
    rows <- beg:end
    mark[rows] <- paste0(cli::bg_cyan(">"), " ")
    # one row only
    if (length(rows) == 1) {
      lns[rows] <- hl(
        lns[rows],
        x$start_column[sel1] + 1L,
        endcol
      )
    } else {
      # first row
      lns[rows[1]] <- hl(lns[rows[1]], x$start_column[sel1] + 1L, end = NULL)
      # middle rows, if any
      if (length(rows) > 2) {
        mid <- middle(rows)
        lns[mid] <- hl(lns[mid])
      }
      # last row
      if (length(rows) >= 2) {
        lns[rows[length(rows)]] <- hl(
          lns[rows[length(rows)]],
          start = NULL,
          endcol
        )
      }
    }
  }

  grey <- cli::col_grey

  # add ... between consecutive lines
  dots <- diff(c(1, num[shwrows])) > 1
  dotlns <- num[shwrows][dots] - 1L
  # add ... to the end
  lastshown <- utils::tail(num[shwrows], 1)
  if (lastshown < nlns) {
    dotlns <- c(dotlns, lastshown + 1L)
  }
  shwrows[dotlns] <- TRUE

  # format the ... lines
  num[shwrows] <- format(num[shwrows])
  num[dotlns] <- "..."
  num <- grey(format(num[shwrows]))
  lns[dotlns] <- ""
  split <- rep(cli::col_grey(" | "), nlns)
  split[dotlns] <- "   "

  slns <- paste0(mark[shwrows], num, split[shwrows], lns[shwrows])

  sfn <- if (!is.null(attr(x, "file"))) paste0(basename(attr(x, "file")), ", ")
  lang <- sub("^ts_tree_", "", grep("^ts_tree_", class(x), value = TRUE))

  c(
    grey(glue(
      "# {lang} ({sfn}{nlns} line{plural(nlns)}, \\
      {nsel} selected element{plural(nsel)})"
    )),
    slns,
    if (nsel > ssel) {
      c(
        grey(glue(
          "{cli::symbol$info} {nsel-ssel} more selected \\
          element{plural(nsel-ssel)}"
        )),
        grey(glue(
          "{cli::symbol$info} Use `print(n = ...)` to see more selected \\
           elements"
        ))
      )
    }
  )
}

plural <- function(x) {
  if (x != 1) {
    "s"
  } else {
    ""
  }
}

# TODO: only vectorized for the default case

hl <- function(txt, start = NULL, end = NULL) {
  if (is.null(start) && is.null(end)) {
    cli::col_cyan(txt)
  } else if (is.null(end) && !is.null(start)) {
    stopifnot(length(txt) == 1)
    paste0(
      if (start > 1) {
        cli::ansi_substr(txt, 1, start - 1)
      },
      cli::col_cyan(cli::ansi_substr(txt, start, cli::ansi_nchar(txt)))
    )
  } else if (is.null(start) && !is.null(end)) {
    stopifnot(length(txt) == 1)
    nc <- cli::ansi_nchar(txt)
    paste0(
      cli::col_cyan(cli::ansi_substr(txt, 1, end)),
      if (end < nc) {
        cli::ansi_substr(txt, end + 1, nc)
      }
    )
  } else {
    stopifnot(length(txt) == 1)
    nc <- cli::ansi_nchar(txt)
    paste0(
      if (start > 1) {
        cli::ansi_substr(txt, 1, start - 1)
      },
      cli::col_cyan(cli::ansi_substr(txt, start, end)),
      if (end < nc) {
        cli::ansi_substr(txt, end + 1, nc)
      }
    )
  }
}
