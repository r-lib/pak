#' @export
#'
#' @ts ts_tree_new_examples TOML examples
#'
#' ```{asciicast}
#' toml <- ts::ts_tree_new(
#'   tstoml::ts_language_toml(),
#'   text = "[table]\nkey = \"value\""
#' )
#' toml
#' ```
#'
#' @ts ts_tree_select_refine TOML example
#'
#' ```{asciicast}
#' #| results = "hide"
#' toml <- tstoml::ts_parse_toml(
#'   '[table]\na = 1\nb = [10, 20, 30]\nc = { c1 = true, c2 = [] }\n'
#' )
#' toml <- toml |> ts_tree_select("table", "b")
#' ```
#'
#' ```{asciicast}
#' # selects the first two elements in the document node, ie. "table"
#' toml |> ts_tree_select(1:2)
#' ```
#'
#' ```{asciicast}
#' # selects the first two elements inside "table" and "b"
#' toml |> ts_tree_select(1:2, refine = TRUE)
#' ```
#'
#' @ts ts_tree_select_set TOML example
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml(
#'   '[table]\na = 1\nb = [10, 20, 30]\nc = { c1 = true, c2 = [] }\n'
#' )
#' toml
#' ```
#'
#' ```{asciicast}
#' toml |> ts_tree_select("table", "b", 1)
#' ```
#'
#' ```{asciicast}
#' ts_tree_select(toml, "table", "b", 1) <- 100
#' toml
#' ```
#'
#' @ts ts_tree_select_brackets TOML example
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml(
#'   '[table]\na = 1\nb = [10, 20, 30]\nc = { c1 = true, c2 = [] }\n'
#' )
#' toml |> ts_tree_select("table", "b", 1)
#' ```
#'
#' ```{asciicast}
#' toml[[list("table", "b", 1)]]
#' ```
#'
#' @ts ts_tree_select_brackets_set TOML example
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml(
#'   '[table]\na = 1\nb = [10, 20, 30]\nc = { c1 = true, c2 = [] }\n'
#' )
#' toml
#' ```
#'
#' ```{asciicast}
#' toml |> ts_tree_select("table", "b", 1)
#' ```
#'
#' ```{asciicast}
#' toml[[list("table", "b", 1)]] <- 100
#' toml
#' ```

ts_tree_new.ts_language_toml <- function(
  language,
  file = NULL,
  text = NULL,
  ranges = NULL,
  fail_on_parse_error = TRUE,
  options = NULL,
  ...
) {
  if (!missing(options)) {
    ts_check_named_arg(options)
  }
  options <- as_tstoml_options(options)

  tree <- NextMethod()

  # this is a workarond for TS adding code to a non-terminal array/object node
  tree$code[tree$type %in% c("array", "inline_table")] <- NA_character_

  tree <- add_dom(tree)

  tree
}

# ------------------------------------------------------------------------------

encode_key <- function(key) {
  paste0(".", paste0(nchar(key, type = "bytes"), ":", key, collapse = "."))
}

add_dom <- function(tab) {
  tab$dom_parent <- rep(NA_integer_, nrow(tab))
  tab$dom_type <- rep(NA_character_, nrow(tab))
  tab$dom_type[1] <- "document"
  tab$dom_name <- rep(NA_character_, nrow(tab))
  dict <- new.env(parent = emptyenv())
  current_table <- 1L
  current_prefix <- character()

  check_sub_keys <- function(key, prefix = NULL) {
    ec <- if (length(prefix) == 0) "" else encode_key(prefix)
    for (idx in seq_along(key$key[-1])) {
      ec <- paste0(ec, encode_key(key$key[idx]))
      rec <- dict[[ec]]
      if (is.null(rec)) {
        dict[[ec]] <- rec <- list(id = key$ids[[idx]], type = "subtable")
        tab$dom_parent[key$ids[[idx]]] <<- current_table
        tab$dom_name[key$ids[[idx]]] <<- key$key[idx]
        tab$dom_type[key$ids[[idx]]] <<- "subtable"
      } else if (rec$type == "pair") {
        stop(ts_cnd(
          "Cannot define subtable under pair: \\
          {paste(c(prefix, key$key[1:idx]), collapse = '.')}."
        ))
      }
      current_table <<- rec$id
      current_prefix <<- key$key[1:idx]
    }
  }

  add_dom_pair <- function(id) {
    current_table_save <- current_table
    current_prefix_save <- current_prefix
    # make sure subtables exist
    key <- unserialize_key_with_ids(tab, tab$children[[id]][1])
    check_sub_keys(key, prefix = current_prefix_save)

    # check for duplicate keys
    ec <- encode_key(c(current_prefix_save, key$key))
    rec <- dict[[ec]]
    if (!is.null(rec)) {
      stop(ts_cnd(
        "Duplicate key definition: {paste(key$key, collapse = '.')}."
      ))
    } else {
      dict[[ec]] <- list(id = id, type = "pair")
    }

    # add pair to current table
    tab$dom_parent[id] <<- current_table
    tab$dom_name[id] <<- last(key$key)
    tab$dom_type[id] <<- "pair"
    tab$dom_parent[tab$children[[id]][3]] <<- id
    tab$dom_type[tab$children[[id]][3]] <<- "value"
    current_table <<- current_table_save
    current_prefix <<- current_prefix_save
  }

  add_dom_table <- function(id) {
    # make sure subtables exist, create if not
    current_table <<- 1L
    current_prefix <<- character()
    key <- unserialize_key_with_ids(tab, tab$children[[id]][2])
    check_sub_keys(key)

    # create new table of upgrade a subtable to a table
    ec <- encode_key(key$key)
    rec <- dict[[ec]]
    if (is.null(rec)) {
      dict[[ec]] <- rec <- list(id = id, type = "table")
      tab$dom_parent[id] <<- current_table
      tab$dom_name[id] <<- last(key$key)
      tab$dom_type[id] <<- "table"
    } else if (rec$type == "subtable") {
      dict[[ec]] <- list(id = rec$id, type = "table")
    } else if (rec$type == "table") {
      stop(ts_cnd(
        "Duplicate table definition: {paste(key$key, collapse = '.')}."
      ))
    } else {
      stop(ts_cnd(
        "Cannot redefine array of tables as table: \\
        {paste(key$key, collapse = '.')}."
      ))
    }
    # this is the current table now
    current_table <<- rec$id
    current_prefix <<- key$key
  }

  add_dom_table_array_element <- function(id) {
    # make sure subtables exist, create if not
    current_table <<- 1L
    current_prefix <<- character()
    key <- unserialize_key_with_ids(tab, tab$children[[id]][2])
    check_sub_keys(key)

    # create new array of tables if it does not exist
    element_id <- tab$children[[id]][2]
    ec <- encode_key(key$key)
    rec <- dict[[ec]]
    if (is.null(rec)) {
      dict[[ec]] <- rec <- list(id = element_id, type = "array_of_tables")
      tab$dom_parent[id] <<- current_table
      tab$dom_name[id] <<- last(key$key)
      tab$dom_type[id] <<- "array_of_tables"
      tab$dom_parent[element_id] <<- id
      tab$dom_type[element_id] <<- "table_array_element"
    } else if (rec$type == "array_of_tables") {
      nms <- ls(dict, all.names = TRUE)
      subs <- nms[startsWith(nms, paste0(ec, "."))]
      rm(list = subs, envir = dict)
      tab$dom_parent[element_id] <<- tab$dom_parent[rec$id]
      tab$dom_type[element_id] <<- "table_array_element"
      rec$id <- element_id
      dict[[ec]] <- rec
    } else {
      stop(ts_cnd(
        "Cannot redefine table as array of tables: \\
        {paste(key$key, collapse = '.')}."
      ))
    }

    current_table <<- element_id
    current_prefix <<- key$key
  }

  tables <- c(1L, which(tab$type %in% c("table", "table_array_element")))
  tabchld <- unlist(tab$children[tables])
  pairs <- tabchld[tab$type[tabchld] == "pair"]
  todo <- sort(c(tables, pairs))

  for (i in todo) {
    switch(
      tab$type[i],
      pair = {
        add_dom_pair(i)
      },
      table = {
        add_dom_table(i)
      },
      table_array_element = {
        add_dom_table_array_element(i)
      }
    )
  }

  check_inline_table <- function(id) {
    # We can use a local dictionary for each inline table
    dict <- new.env(parent = emptyenv())
    children <- tab$children[[id]]
    pairs <- children[tab$type[children] == "pair"]

    check_keys <- function(key) {
      ec <- ""
      for (idx in seq_along(key$key[-1])) {
        ec <- paste0(ec, encode_key(key$key[idx]))
        rec <- dict[[ec]]
        if (is.null(rec)) {
          dict[[ec]] <- rec <- list(id = key$ids[[idx]], type = "subtable")
          tab$dom_parent[key$ids[[idx]]] <<- parent
          tab$dom_name[key$ids[[idx]]] <<- key$key[idx]
          tab$dom_type[key$ids[[idx]]] <<- "subtable"
        } else if (rec$type == "pair") {
          stop(ts_cnd(
            "Cannot define subtable under pair in inline table: \\
            {paste(key$key[1:idx], collapse = '.')}."
          ))
        }
        parent <<- rec$id
      }
    }

    for (p in pairs) {
      parent <- id
      key <- unserialize_key_with_ids(tab, tab$children[[p]][1])
      check_keys(key)
      ec <- encode_key(key$key)
      rec <- dict[[ec]]
      if (!is.null(rec)) {
        stop(ts_cnd(
          "Duplicate key definition in inline table: \\
          {paste(key$key, collapse = '.')}."
        ))
      } else {
        rec <- dict[[ec]] <- list(id = p, type = "pair")
      }
      tab$dom_parent[p] <<- parent
      tab$dom_name[p] <<- last(key$key)
      tab$dom_type[p] <<- "pair"
      tab$dom_parent[tab$children[[p]][3]] <<- p
      tab$dom_type[tab$children[[p]][3]] <<- "value"
    }
  }

  for (i in which(tab$type == "inline_table")) {
    tab$dom_type[i] <- "inline_table"
    check_inline_table(i)
  }

  # wire up arrays as well
  for (i in which(tab$type == "array")) {
    tab$dom_type[i] <- "array"
    children <- tab$children[[i]]
    children <- children[!tab$type[children] %in% c("[", "]", ",", "comment")]
    for (cx in seq_along(children)) {
      el <- children[cx]
      tab$dom_parent[el] <- i
      if (is.na(tab$dom_type[el])) tab$dom_type[el] <- "value"
    }
  }

  # pairs should not be in the DOM
  parent_is_pair <- which(tab$dom_type[tab$dom_parent] == "pair")
  pairs <- tab$dom_parent[parent_is_pair]
  tab$dom_parent[parent_is_pair] <- tab$dom_parent[pairs]
  tab$dom_name[parent_is_pair] <- tab$dom_name[pairs]
  tab$dom_type[pairs] <- NA_character_
  tab$dom_name[pairs] <- NA_character_
  tab$dom_parent[pairs] <- NA_integer_

  # add dom_children
  lvls <- seq_len(nrow(tab))
  tab$dom_children <- I(unname(split(
    lvls,
    factor(tab$dom_parent, levels = lvls)
  )))

  # name dom_children
  for (i in seq_len(nrow(tab))) {
    nmsi <- tab$dom_name[tab$dom_children[[i]]]
    if (length(nmsi) > 0 && any(!is.na(nmsi))) {
      names(tab$dom_children[[i]]) <- tab$dom_name[tab$dom_children[[i]]]
    }
  }

  tab
}
