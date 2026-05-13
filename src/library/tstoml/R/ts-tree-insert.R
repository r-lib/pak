#' @ts ts_tree_insert_details_errors
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml("a = true\nb = [1, 2, 3]")
#' toml |> ts_tree_select("a") |> ts_tree_insert("foo")
#' ```
#'
#' @ts ts_tree_insert_details_no_selection
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml("a = true\nb = [1, 2, 3]")
#' toml |> ts_tree_insert(key = "c", new = "foo")
#' ```
#'
#' @ts ts_tree_insert_details_empty_selection
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml("a = true\nb = [1, 2, 3]")
#' toml |> ts_tree_select("nonexistent") |> ts_tree_insert("foo")
#' ```
#'
#' @export

ts_tree_insert.ts_tree_toml <- function(
  tree,
  new,
  key = NULL,
  at = Inf,
  options = NULL,
  ...
) {
  select <- ts_tree_selected_nodes(tree)

  if (length(select) == 0) {
    return(tree)
  }

  insertions <- lapply(select, function(sel1) {
    dom_type <- tree$dom_type[sel1]
    switch(
      dom_type,
      document = {
        insert_into_document(tree, sel1, new, key = key)
      },
      subtable = {
        insert_into_subtable(tree, sel1, new, key = key)
      },
      inline_table = {
        insert_into_inline_table(tree, sel1, new, key = key, at = at)
      },
      table = {
        insert_into_table(tree, sel1, new, key = key, at = at)
      },
      array_of_tables = {
        insert_into_aot(tree, sel1, new, at = at)
      },
      table_array_element = {
        insert_into_aot_element(tree, sel1, new, key = key, at = at)
      },
      array = {
        insert_into_array(tree, sel1, new, key = key, at = at)
      },
      stop(ts_cnd("Cannot insert into a `{dom_type}` TOML element."))
    )
  })

  insertions <- insertions[order(map_int(insertions, "[[", "after"))]

  for (ins in insertions) {
    if (!isFALSE(ins$leading_comma)) {
      aft <- last_descendant(tree, ins$leading_comma)
      tree$tws[aft] <- paste0(",", tree$tws[aft])
    }

    aft <- last_descendant(tree, ins$after)
    firstchld <- tree$children[[ins$select]][1]
    # mark first child for reformatting the whole array
    before_tws <- isTRUE(ins$before_trailing_ws)
    tree$tws[firstchld] <- paste0(reformat_mark, tree$tws[firstchld])
    tree$tws[aft] <- paste0(
      if (!before_tws) tree$tws[aft],
      ins$code,
      if (ins$trailing_comma) ", ",
      if (before_tws) tree$tws[aft],
      if (ins$trailing_newline) "\n",
      ins$tws
    )
  }

  parts <- c(rbind(tree$code, tree$tws))
  text <- unlist(lapply(na_omit(parts), charToRaw))

  # get the reformatting positions and remove the marks
  # (the toml parser does not handle the \f reformatting mark)

  refmt_pos <- grepRaw(reformat_mark, text, fixed = TRUE, all = TRUE)
  if (length(refmt_pos) > 0) {
    text <- text[-refmt_pos]
    refmt_pos <- refmt_pos - seq_along(refmt_pos)
  }

  # TODO: update coordinates without reparsing
  new <- ts_parse_toml(text = text)
  attr(new, "file") <- attr(tree, "file")

  # now reformat the new parts, or the newly non-empty arrays/objects
  fws <- new$end_byte %in% refmt_pos
  tofmt2 <- unique(new$parent[which(fws)])

  # do not refortmat the whole document
  tofmt2 <- setdiff(tofmt2, 1L)

  # auto format then each insertion might need a different format
  new <- ts_tree_select(new, I(tofmt2))
  options$insert_empty_line_before_tables <- FALSE
  new <- ts_tree_format(new, options = options)

  new
}

last_descendant <- function(toml, node) {
  while (node != 1 && is.na(toml$code[node])) {
    node <- utils::tail(toml$children[[node]], 1)
  }
  node
}

last_dom_descendant <- function(toml, node) {
  while (length(toml$dom_children[[node]]) >= 1) {
    node <- utils::tail(toml$dom_children[[node]], 1)
  }
  node
}

# this has to be a single byte for TOML!
reformat_mark <- "\f"
stopifnot(nchar(reformat_mark, type = "bytes") == 1)

# ------------------------------------------------------------------------------

insert_into_document <- function(toml, sel1, new, key = NULL) {
  newtype <- get_stl_type(new)
  newtypename <- stl_type_names[[newtype]]
  if (is.null(key)) {
    stop(ts_cnd(
      "The `key` argument is required when inserting a {newtypename} \\
       into the document."
    ))
  }
  if (length(key) != 1) {
    stop(ts_cnd("The `key` argument must be a single string for now."))
  }
  chdn <- toml$dom_children[[1]]
  keys <- toml$dom_name[chdn]
  if (key %in% keys) {
    stop(ts_cnd("Key `{key}` already exists in the document."))
  }

  switch(
    newtype,
    "pair" = {
      insert_into_document_pair(toml, sel1, new, key = key)
    },
    "table" = {
      insert_into_document_table(toml, sel1, new, key = key)
    },
    "array_of_tables" = {
      insert_into_document_aot(toml, sel1, new, key = key)
    },
    stop(ts_cnd(
      "Cannot insert {newtypename} ({newtype)} into document. \\
       This is an internal error in tstoml"
    ))
  )
}

insert_into_document_pair <- function(toml, sel1, new, key) {
  # need to put it before the first table or AOT
  chdn <- toml$children[[1]]
  wtable <- which(toml$type[chdn] %in% c("table", "table_array_element"))[1]
  # if no table or AOT, append to the end
  after <- if (is.na(wtable)) nrow(toml) else chdn[wtable] - 1L
  code <- paste0(
    if (nrow(toml) > 1) {
      "\n"
    },
    key,
    " = ",
    paste0(ts_serialize_toml_value(new), collapse = "\n")
  )

  list(
    select = sel1,
    after = after,
    code = code,
    leading_comma = FALSE,
    trailing_comma = FALSE,
    trailing_newline = TRUE
  )
}

insert_into_document_table <- function(toml, sel1, new, key) {
  code <- paste0(
    if (nrow(toml) > 1) {
      "\n"
    },
    paste0(
      ts_serialize_toml(structure(list(new), names = key)),
      collapse = "\n"
    )
  )
  list(
    select = sel1,
    after = nrow(toml),
    code = code,
    leading_comma = FALSE,
    trailing_comma = FALSE,
    trailing_newline = TRUE
  )
}

insert_into_document_aot <- function(toml, sel1, new, key) {
  code <- paste0(
    if (nrow(toml) > 1) {
      "\n"
    },
    paste0(
      ts_serialize_toml(structure(list(new), names = key)),
      collapse = "\n"
    )
  )
  list(
    select = sel1,
    after = nrow(toml),
    code = code,
    leading_comma = FALSE,
    trailing_comma = FALSE,
    trailing_newline = TRUE
  )
}

# ------------------------------------------------------------------------------

insert_into_subtable <- function(toml, sel1, new, key = key) {
  newtype <- get_stl_type(new)
  newtypename <- stl_type_names[[newtype]]
  if (is.null(key)) {
    stop(ts_cnd(
      "The `key` argument is required when inserting a {newtypename} \\
       into a subtable."
    ))
  }
  if (length(key) != 1) {
    stop(ts_cnd("The `key` argument must be a single string for now."))
  }
  chdn <- toml$dom_children[[sel1]]
  keys <- toml$dom_name[chdn]
  if (key %in% keys) {
    stop(ts_cnd("Key `{key}` already exists in the subtable."))
  }

  # check if the subtable was created from the key of a pair or table/AOT
  pkey <- sel1
  while (toml$type[pkey] %in% key_types) {
    pkey <- toml$parent[pkey]
  }

  # the key is the key of the subtable plus the new key
  newkey <- key
  dompkey <- sel1
  while (toml$type[dompkey] %in% key_types) {
    newkey <- c(unserialize_key(toml, dompkey), newkey)
    dompkey <- toml$dom_parent[dompkey]
  }

  if (toml$type[pkey] == "pair") {
    after <- last_descendant(
      toml,
      last_dom_descendant(toml, chdn[length(chdn)])
    )
    trailing_newline <- FALSE
  } else {
    # we cannot create a table with the key of sel1, and a bare key in that
    # because the Rust parser might not parse that. E.g.
    # a.y = 100
    # [a]
    # x = 100
    # [a.b]
    # c.d.e = 1
    # does not parse (just insertd the [a] table here).
    # So we create a dotted pair at the beginning of the document, before
    # all tables and AOTs
    dchdn <- toml$children[[1]]
    wtable <- which(toml$type[dchdn] %in% c("table", "table_array_element"))[1]
    after <- if (is.na(wtable)) nrow(toml) else dchdn[wtable] - 1L
    trailing_newline <- TRUE
  }

  code <- paste0(
    if (after != 1L) "\n",
    ts_toml_key(newkey),
    " = ",
    paste0(ts_serialize_toml_value(new), collapse = "\n")
  )

  list(
    select = sel1,
    after = after,
    code = code,
    leading_comma = FALSE,
    trailing_comma = FALSE,
    # there must be a trailing newline after the table already
    trailing_newline = trailing_newline,
    before_trailing_ws = TRUE
  )
}

# ------------------------------------------------------------------------------

insert_into_inline_table <- function(toml, sel1, new, key = key, at = at) {
  newtype <- get_stl_type(new)
  newtypename <- stl_type_names[[newtype]]
  if (is.null(key)) {
    stop(ts_cnd(
      "The `key` argument is required when inserting a {newtypename} \\
       into an inline table."
    ))
  }
  if (length(key) != 1) {
    stop(ts_cnd("The `key` argument must be a single string for now."))
  }
  chdn <- toml$dom_children[[sel1]]
  keys <- toml$dom_name[chdn]
  if (key %in% keys) {
    stop(ts_cnd("Key `{key}` already exists in the inline table."))
  }

  chdn <- toml$children[[sel1]]
  isxtr <- toml$type[chdn] != "pair"
  idx <- seq_along(chdn)[!isxtr]
  nchdn <- length(idx)

  if (is.character(at)) {
    if (length(at) != 1) {
      stop(ts_cnd("The `at` argument must be a single string for now."))
    }
    rchdn <- chdn[toml$type[chdn] == "pair"]
    keys <- map_chr(rchdn, function(id) {
      keyid <- toml$children[[id]][1]
      unserialize_key(toml, keyid)
    })
    at <- match(at, keys)
    if (is.na(at)) {
      at <- Inf
    }
  }

  after_comma <- after <- if (at < 1 || nchdn == 0) {
    1
  } else if (at >= nchdn) {
    idx[nchdn]
  } else {
    idx[at]
  }

  # Comments are not allowed inside inline tables, so we don't need to
  # deal with them here.

  # skip comma
  if (toml$type[chdn[after + 1L]] == ",") {
    after <- after + 1L
  }

  code <- paste0(key, " = ", ts_serialize_toml_value(new))

  add_leading_comma <- at >= nchdn && nchdn > 0
  add_trailing_comma <- (at < nchdn && nchdn > 0)

  list(
    select = sel1,
    after = chdn[after],
    code = paste0(code, collapse = "\n"),
    # need a leading comma if inserting at the end into non-empty array
    leading_comma = if (add_leading_comma) chdn[after_comma] else FALSE,
    # need a trailing comma everywhere except at the end or in an empty array
    trailing_comma = add_trailing_comma,
    trailing_newline = toml$type[chdn[after + 1L]] == "comment"
  )
}

# ------------------------------------------------------------------------------

insert_into_table <- function(toml, sel1, new, key = key, at = at) {
  newtype <- get_stl_type(new)
  newtypename <- stl_type_names[[newtype]]
  if (is.null(key)) {
    stop(ts_cnd(
      "The `key` argument is required when inserting a {newtypename} \\
       into a table."
    ))
  }
  if (length(key) != 1) {
    stop(ts_cnd("The `key` argument must be a single string for now."))
  }
  chdn <- toml$dom_children[[sel1]]
  keys <- toml$dom_name[chdn]
  if (key %in% keys) {
    stop(ts_cnd("Key `{key}` already exists in the table."))
  }

  switch(
    newtype,
    "pair" = {
      insert_into_table_pair(toml, sel1, new, key = key)
    },
    "table" = {
      insert_into_table_table(toml, sel1, new, key = key)
    },
    "array_of_tables" = {
      insert_into_table_aot(toml, sel1, new, key = key)
    },
    stop(ts_cnd(
      "Cannot insert {newtypename} ({newtype)} into table. \\
       This is an internal error in tstoml"
    ))
  )
}

insert_into_table_pair <- function(toml, sel1, new, key) {
  #  we also this for aot elements
  tid <- if (toml$type[sel1] == "table") {
    sel1
  } else {
    toml$parent[sel1]
  }
  after <- last_descendant(toml, tid)
  code <- paste0(
    "\n",
    key,
    " = ",
    paste0(ts_serialize_toml_value(new), collapse = "\n")
  )
  list(
    select = sel1,
    after = after,
    code = code,
    leading_comma = FALSE,
    trailing_comma = FALSE,
    # there must be a trailing newline after the table already
    trailing_newline = FALSE,
    before_trailing_ws = TRUE
  )
}

insert_into_table_table <- function(toml, sel1, new, key) {
  #  we also this for aot elements
  tid <- if (toml$type[sel1] == "table") {
    sel1
  } else {
    toml$parent[sel1]
  }
  after <- last_descendant(toml, tid)
  keyid <- toml$children[[tid]][2]
  newkey <- ts_toml_key(c(unserialize_key(toml, keyid), key))
  code <- paste0(
    "\n\n",
    paste0(
      ts_serialize_toml(structure(list(new), names = newkey)),
      collapse = "\n"
    )
  )
  list(
    select = sel1,
    after = after,
    code = code,
    leading_comma = FALSE,
    trailing_comma = FALSE,
    trailing_newline = FALSE,
    before_trailing_ws = TRUE
  )
}

insert_into_table_aot <- function(toml, sel1, new, key) {
  #  we also this for aot elements
  tid <- if (toml$type[sel1] == "table") {
    sel1
  } else {
    toml$parent[sel1]
  }
  after <- last_descendant(toml, tid)
  keyid <- toml$children[[tid]][2]
  newkey <- ts_toml_key(c(unserialize_key(toml, keyid), key))
  code <- paste0(
    "\n\n",
    paste0(
      ts_serialize_toml(structure(list(new), names = newkey)),
      collapse = "\n"
    )
  )
  list(
    select = sel1,
    after = after,
    code = code,
    leading_comma = FALSE,
    trailing_comma = FALSE,
    trailing_newline = FALSE,
    before_trailing_ws = TRUE
  )
}

# ------------------------------------------------------------------------------

insert_into_aot <- function(toml, sel1, new, at = Inf) {
  chdn <- toml$dom_children[[sel1]]
  after <- if (at < 1) {
    leading_newline <- FALSE
    sel1 - 1L
  } else if (at >= length(chdn)) {
    leading_newline <- TRUE
    last_descendant(toml, toml$parent[chdn[length(chdn)]])
  } else {
    leading_newline <- FALSE
    last_descendant(toml, toml$parent[chdn[at]])
  }

  key <- unserialize_key(toml, toml$children[[sel1]][2])
  code <- paste0(
    if (leading_newline) "\n",
    "[[",
    key,
    "]]\n",
    paste(stl_table_body(new), collapse = "\n"),
    "\n"
  )

  list(
    select = sel1,
    after = after,
    code = code,
    leading_comma = FALSE,
    trailing_comma = FALSE,
    trailing_newline = after != nrow(toml)
  )
}

# ------------------------------------------------------------------------------

insert_into_aot_element <- function(toml, sel1, new, key = key, at = at) {
  insert_into_table(toml, sel1, new, key = key, at = at)
}

# ------------------------------------------------------------------------------

insert_into_array <- function(toml, sel1, new, key = key, at = at) {
  # this is complicated by comments inside the array
  # we need to build an index to map non-comment children to actual children
  # we might as well treat the [ ] and comma nodes the same way
  chdn <- toml$children[[sel1]]
  isxtr <- toml$type[chdn] %in% c("comment", "[", "]", ",")
  idx <- seq_along(chdn)[!isxtr]
  nchdn <- length(idx)

  after_comma <- after <- if (at < 1 || nchdn == 0) {
    1
  } else if (at >= nchdn) {
    idx[nchdn]
  } else {
    idx[at]
  }

  if (
    toml$type[chdn[after + 1L]] == "," &&
      toml$type[chdn[after + 2L]] == "comment" &&
      toml$end_row[chdn[after + 1L]] == toml$start_row[chdn[after + 2L]]
  ) {
    # skip comma + comment on the same line!
    after <- after + 2L
  } else if (
    toml$type[chdn[after + 1L]] == "comment" &&
      toml$end_row[chdn[after]] == toml$start_row[chdn[after + 1L]]
  ) {
    # skip comment on the same line
    after <- after + 1L
    # maybe more comments
    while (toml$type[chdn[after + 1L]] == "comment") {
      after <- after + 1L
    }
    # maybe a comma
    if (toml$type[chdn[after + 1L]] == ",") {
      after <- after + 1L
    }
  } else if (toml$type[chdn[after + 1L]] == ",") {
    # skip comma w/o comment on the same line
    # keep non-line comment as non-line comment
    after <- after + 1L
  } else {
    # skip comments and potentially a comma
    while (toml$type[chdn[after + 1L]] == "comment") {
      after <- after + 1L
    }
    if (toml$type[chdn[after + 1L]] == ",") {
      after <- after + 1L
    }
  }

  # handle appending when there is a trailig comma
  chdnx <- chdn[toml$type[chdn] != "comment"]
  has_trailing_comma <- toml$type[rev(chdnx)][2] == ","
  add_leading_comma <- !has_trailing_comma && at >= nchdn && nchdn > 0
  add_trailing_comma <- (at < nchdn && nchdn > 0) ||
    (at >= nchdn && has_trailing_comma)

  list(
    select = sel1,
    after = chdn[after],
    code = ts_serialize_toml_value(new),
    # need a leading comma if inserting at the end into non-empty array
    leading_comma = if (add_leading_comma) chdn[after_comma] else FALSE,
    # need a trailing comma everywhere except at the end or in an empty array
    trailing_comma = add_trailing_comma,
    trailing_newline = toml$type[chdn[after + 1L]] == "comment"
  )
}
