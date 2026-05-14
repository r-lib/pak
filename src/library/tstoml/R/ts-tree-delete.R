#' Delete selected elements from a tstoml object
#'
#' The formatting of the rest of TOML document is kept as is. Comments
#' appearing inside the deleted elements are also deleted. Other comments
#' are left as is.
#'
#' @ts ts_tree_delete_details_formatting TOML example
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml("
#'   [package]
#'   name = 'tstoml'
#'   version = '0.1.0'
#' ")
#' toml
#' ```
#'
#' ```{asciicast}
#' toml |> ts_tree_select("package", "name") |> ts_tree_delete()
#' ```
#'
#' @ts ts_tree_delete_details_no_selection TOML example
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml("
#'   [package]
#'   name = 'tstoml'
#'   version = '0.1.0'
#' ") |> ts::ts_tree_format()
#' toml |> ts_tree_delete()
#' ```
#'
#' @ts ts_tree_delete_details_empty_selection
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml("
#'   [package]
#'   name = 'tstoml'
#'   version = '0.1.0'
#' ") |> ts::ts_tree_format()
#' toml |> ts_tree_select("nothere") |> ts_tree_delete()
#' ```
#'
#' @ts ts_tree_delete_details_comments
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml("
#'   # top comment
#'   [package]
#'   name = 'tstoml' # inline comment
#'   version = '0.1.0'
#' ") |> ts::ts_tree_format()
#' toml
#' ```
#'
#' ```{asciicast}
#' toml |> ts_tree_select("package", "name") |> ts_tree_delete()
#' ```
#'
#' @details
#' If `toml` has no selection then the the whole document is deleted.
#' If `toml` has an empty selection, then nothing is delted.
#'
#' @param tree tstoml object.
#' @param ... Reserved for future use.
#' @return Modified tstoml object.
#'
#' @export
#' @keywords internal
#' @examples
#' library(ts)
#' toml <- ts_parse_toml(text = toml_example_text())
#' toml
#'
#' toml |> ts_tree_select("owner", "name") |> ts_tree_delete()
#' toml |> ts_tree_select("owner") |> ts_tree_delete()

ts_tree_delete.ts_tree_toml <- function(tree, ...) {
  select <- ts_tree_selected_nodes(tree)

  if (length(select) == 0) {
    attr(tree, "selection") <- NULL
    return(tree)
  }

  # if deleting from an array, then we need to look at the array and
  # remove some commas, probably
  pares <- tree$parent[select]
  ptypes <- tree$type[pares]
  trimmed_arrays <- unique(pares[ptypes == "array"])
  trimmed_pairs <- pares[ptypes == "pair"]
  select <- c(select, trimmed_pairs)
  trimmed_objects <- unique(tree$parent[trimmed_pairs])
  trimmed_objects <- trimmed_objects[
    tree$type[trimmed_objects] == "inline_table"
  ]

  # get the full subtree of nodes to delete
  subtrees <- lapply(select, get_dom_subtree, tree = tree, with_root = TRUE)
  subtrees <- lapply(subtrees, function(st) {
    # when deleting values, we are deleting their keys as well
    # which to remove NA coming from the document node
    is_pair_value <- which(tree$type[tree$parent[st]] == "pair")

    st[is_pair_value] <- tree$parent[st[is_pair_value]]
    st
  })
  subtrees <- lapply(subtrees, function(st) {
    unlist(lapply(st, get_subtree, tree = tree, with_root = TRUE))
  })
  deleted <- unique(unlist(subtrees))

  # if deleting an AOT element, remove the whole element
  del_pares <- tree$parent[deleted]
  aot_elements <- (tree$type[deleted] %in% key_types) &
    tree$type[del_pares] == "table_array_element"
  deleted <- unique(c(deleted, unlist(tree$children[del_pares[aot_elements]])))

  trim_commas <- function(id, open, close) {
    # remove deleted children and see what is left
    allchld <- tree$children[[id]]
    chld <- setdiff(allchld, deleted)
    nc <- length(chld)
    # if nothing left, then nothing to do
    if (nc == 2) {
      return()
    }
    ctypes <- tree$type[chld]
    todel <- rep(FALSE, length(chld))
    # this is hard to write in a vectorized form, it is easier iteratively
    # leading commas
    for (i in seq_along(todel)[-1]) {
      if (ctypes[i] == ',') {
        todel[i] <- TRUE
      } else {
        break
      }
    }
    # trailing commas
    for (i in rev(seq_along(todel))[-1]) {
      if (ctypes[i] == ',') {
        todel[i] <- TRUE
      } else {
        break
      }
    }
    # duplicate commas
    todel[ctypes[-nc] == ',' & ctypes[-1] == ','] <- TRUE
    chdel <- chld[which(todel)]
    deleted <<- c(deleted, chdel)

    # if the last element is deleted, take the trailing whitespace of
    # its last token and add it to the last token of the last kept element
    chdeld <- intersect(allchld, deleted)
    chkept <- setdiff(allchld, chdeld)
    last_deld <- chdeld[length(chdeld)]
    last_kept <- chkept[length(chkept) - 1L]
    if (last_deld > last_kept) {
      while (is.na(tree$code[last_deld])) {
        last_deld <- utils::tail(tree$children[[last_deld]], 1)
      }
      while (is.na(tree$code[last_kept])) {
        last_kept <- utils::tail(tree$children[[last_kept]], 1)
      }
      tree$tws[last_kept] <<- tree$tws[last_deld]
    }
  }

  # trim commas from arrays
  trimmed_arrays <- setdiff(trimmed_arrays, deleted)
  for (arr in trimmed_arrays) {
    trim_commas(arr, "[", "]")
  }

  # trim pairs and commas from objects
  trimmed_objects <- setdiff(trimmed_objects, deleted)
  for (obj in trimmed_objects) {
    trim_commas(obj, "{", "}")
  }

  # NA happens if the whole document is deleted, the parent of 1 is NA
  tree2 <- tree[-na_omit(deleted), ]

  # update text
  parts <- c(rbind(tree2$code, tree2$tws))
  text <- as.raw(unlist(lapply(na_omit(parts), charToRaw)))

  # TODO: update coordinates without reparsing
  new <- ts_parse_toml(text = text)
  attr(new, "file") <- attr(tree, "file")

  new
}

key_types <- c("bare_key", "quoted_key", "dotted_key")
