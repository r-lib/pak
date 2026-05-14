#' Format the selected TOML elements
#'
#' @ts ts_tree_format_details_no_selection
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml("a = [1,2,3]")
#' toml |> ts_tree_format()
#' ```
#'
#' @ts ts_tree_format_details_empty_selection
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml("a = [1,2,3]")
#' toml |> ts_tree_select("c") |> ts_tree_format()
#' ```
#'
# TODO: @ts ts_tree_format_details_options
#'
#' @details
#' If `tree` does not have a selection, then all of it is formatted.
#' If `tree` has an empty selection, then nothing happens.
#'
#' @inheritParams ts_parse_toml
#' @param tree tstoml object.
#' @param ... Reserved for future use.
#' @return The updated tstoml object.
#'
#' @export
#' @keywords internal

ts_tree_format.ts_tree_toml <- function(
  tree,
  options = NULL,
  ...
) {
  if (!missing(options)) {
    ts_check_named_arg(options)
  }
  options <- as_tstoml_options(options)
  select <- ts_tree_selected_nodes(tree)
  fmt <- lapply(
    select,
    format_element,
    tree = tree,
    options = options
  )
  for (i in seq_along(select)) {
    sel1 <- select[i]
    prevline <- rev(which(tree$end_row == tree$start_row[sel1] - 1))[1]
    ind0 <- sub("^.*\n", "", tree$tws[prevline])
    if (!is.na(prevline)) {
      fmt[[i]] <- paste0(c("", rep(ind0, length(fmt[[i]]) - 1L)), fmt[[i]])
    }
  }

  subtrees <- lapply(select, get_subtree, tree = tree, with_root = FALSE)
  deleted <- unique(unlist(subtrees))

  # need to keep the trailing ws of the last element
  lasts <- map_int(subtrees, max_or_na)
  tws <- tree$tws[lasts]
  tree$code[deleted] <- NA_character_
  tree$tws[deleted] <- NA_character_
  tree$code[select] <- paste0(
    map_chr(fmt, paste, collapse = "\n"),
    ifelse(is.na(tws), "", tws)
  )
  tree$tws[select] <- NA_character_

  parts <- c(rbind(tree$code, tree$tws))
  text <- unlist(lapply(na_omit(parts), charToRaw))

  # TODO: update coordinates without reparsing
  new <- ts_parse_toml(text = text)
  attr(new, "file") <- attr(tree, "file")

  new
}

get_subtree <- function(tree, id, with_root = FALSE) {
  sel <- c(if (with_root) id, tree$children[[id]])
  while (TRUE) {
    sel2 <- unique(c(sel, unlist(tree$children[sel])))
    if (length(sel2) == length(sel)) {
      return(sel)
    }
    sel <- sel2
  }
}


format_element <- function(tree, id, options) {
  type <- tree$type[id]
  switch(
    type,
    table = {
      format_table(tree, id, options)
    },
    document = {
      format_document(tree, id, options)
    },
    table_array_element = {
      format_table_array_element(tree, id, options)
    },
    inline_table = {
      format_inline_table(tree, id, options)
    },
    array = {
      format_array(tree, id, options)
    },
    pair = {
      format_pair(tree, id, options)
    },
    integer = {
      format_integer(tree, id, options)
    },
    float = {
      format_float(tree, id, options)
    },
    boolean = {
      format_boolean(tree, id, options)
    },
    offset_date_time = {
      format_offset_date_time(tree, id, options)
    },
    local_date_time = {
      format_local_date_time(tree, id, options)
    },
    local_date = {
      format_local_date(tree, id, options)
    },
    local_time = {
      format_local_time(tree, id, options)
    },
    string = {
      format_string(tree, id, options)
    },
    comment = {
      format_comment(tree, id, options)
    },
    "," = {
      ", "
    },
    stop(ts_cnd("Internal tstoml error, unknown TOML node type: {type}."))
  )
}

format_table <- function(tree, id, options) {
  hdr_chld <- get_subtree(tree, tree$children[[id]][2L], with_root = TRUE)
  hdr <- paste(na_omit(tree$code[hdr_chld]), collapse = "")
  chld <- tree$children[[id]][-(1:3)]
  c(
    if (!is_false(options$insert_empty_line_before_tables)) "",
    paste0("[", hdr, "]"),
    unlist(lapply(chld, format_element, tree = tree, options = options))
  )
}

format_document <- function(tree, id, options) {
  chld <- tree$children[[id]]
  lns <- unlist(lapply(chld, format_element, tree = tree, options = options))
  if (length(lns) > 0 && lns[1] == "") {
    lns <- lns[-1]
  }
  lns
}

format_table_array_element <- function(tree, id, options) {
  hdr_chld <- get_subtree(tree, tree$children[[id]][2L], with_root = TRUE)
  hdr <- paste(na_omit(tree$code[hdr_chld]), collapse = "")
  chld <- tree$children[[id]][-(1:3)]
  c(
    if (!is_false(options$insert_empty_line_before_tables)) "",
    paste0("[[", hdr, "]]"),
    unlist(lapply(chld, format_element, tree = tree, options = options))
  )
}

format_inline_table <- function(tree, id, options) {
  chld <- tree$children[[id]]
  chld <- chld[!tree$type[chld] %in% c("{", "}", ",")]
  paste0(
    "{ ",
    paste(
      map_chr(chld, format_pair, tree = tree, options = options),
      collapse = ", "
    ),
    " }"
  )
}

which_line_comments <- function(tree, ids) {
  # this only works because `start_row` is sorted
  which(
    tree$type[ids] == "comment" &
      tree$end_row[ids - 1] == tree$start_row[ids]
  )
}

format_line_comments <- function(tree, elts, ids) {
  cmts <- which_line_comments(tree, ids)
  for (i in cmts) {
    # may happen if an array or object starts with a comment
    if (i == 1L) {
      next
    }
    elts[[i - 1]][length(elts[[i - 1]])] <- paste(
      elts[[i - 1]][length(elts[[i - 1]])],
      elts[[i]]
    )
    elts[i] <- list(NULL)
  }
  elts
}

format_post_process_commas <- function(tree, elts, ids) {
  commas <- map_lgl(elts, function(x) {
    length(x) == 1 && startsWith(x, ",")
  })
  for (i in which(commas)) {
    if (tree$type[ids[i - 1]] == "comment") {
      next
    }
    elts[[i - 1]][length(elts[[i - 1]])] <- paste0(
      elts[[i - 1]][length(elts[[i - 1]])],
      elts[[i]]
    )
    elts[i] <- list(NULL)
  }
  elts
}

format_create_indent <- function(options) {
  if (options[["indent_style"]] == "space") {
    strrep(" ", options[["indent_width"]])
  } else {
    "\t"
  }
}

# TODO: use multiple lines for long arrays
format_array <- function(tree, id, options) {
  chdn <- tree$children[[id]]

  if (length(chdn) == 2) {
    return("[]")
  }

  chdn <- middle(chdn)
  elts <- lapply(
    chdn,
    format_element,
    tree = tree,
    options = options
  )

  elts <- format_line_comments(tree, elts, chdn)
  elts <- format_post_process_commas(tree, elts, chdn)

  has_comments <- any(tree$type[chdn] == "comment")
  if (has_comments) {
    indent <- format_create_indent(options)
    c("[", paste0(indent, unlist(elts)), "]")
  } else {
    paste0("[ ", paste0(unlist(elts), collapse = ""), " ]")
  }
}

format_pair <- function(tree, id, options) {
  key_chld <- get_subtree(tree, tree$children[[id]][1L], with_root = TRUE)
  key <- paste(na_omit(tree$code[key_chld]), collapse = "")
  val <- format_element(tree, tree$children[[id]][3L], options = options)
  cmt <- if (length(tree$children[[id]]) > 3L) {
    paste0(
      " ",
      format_element(tree, tree$children[[id]][4L], options = options)
    )
  }
  paste0(key, " = ", val, cmt)
}

format_integer <- function(tree, id, options) {
  tree$code[id]
}

format_float <- function(tree, id, options) {
  tree$code[id]
}

format_boolean <- function(tree, id, options) {
  tree$code[id]
}

format_offset_date_time <- function(tree, id, options) {
  tree$code[id]
}

format_local_date_time <- function(tree, id, options) {
  tree$code[id]
}

format_local_date <- function(tree, id, options) {
  tree$code[id]
}

format_local_time <- function(tree, id, options) {
  tree$code[id]
}

format_string <- function(tree, id, options) {
  chld <- get_subtree(tree, id, with_root = TRUE)
  paste(na_omit(tree$code[chld]), collapse = "")
}

format_comment <- function(tree, id, options) {
  code <- trimws(tree$code[id], "left")
  paste0(
    "#",
    if (substr(code, 2, 2) != " ") {
      paste0(" ", substr(code, 2, nchar(code)))
    } else {
      substr(code, 2, nchar(code))
    }
  )
}
