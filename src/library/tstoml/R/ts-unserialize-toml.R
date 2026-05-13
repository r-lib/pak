#' Unserialize TOML to R objects
#'
#' @inheritParams ts_parse_toml
#' @export

ts_unserialize_toml <- function(
  file = NULL,
  text = NULL,
  ranges = NULL,
  options = NULL
) {
  # if (!missing(options)) {
  #   check_named_arg(options)
  # }
  # options <- as_tstoml_options(options)

  tab <- ts_tree_new(
    language = ts_language_toml(),
    file = file,
    text = text,
    ranges = ranges,
    options = options
  )
  unserialize_element(tab, 1L)
}

unserialize_element <- function(tree, id) {
  type <- tree$type[id]
  parent <- tree$parent[id]
  if (
    !is.na(parent) &&
      tree$type[parent] == "table_array_element" &&
      type %in% c("bare_key", "quoted_key", "dotted_key")
  ) {
    # this is an AOT _element_, unserialize like a table
    type <- "table"
  }

  elt <- switch(
    type,
    table = ,
    document = ,
    inline_table = ,
    bare_key = ,
    quoted_key = {
      unserialize_table(tree, id)
    },
    array = ,
    table_array_element = {
      unserialize_array(tree, id)
    },
    integer = {
      unserialize_integer(tree, id)
    },
    float = {
      unserialize_float(tree, id)
    },
    boolean = {
      unserialize_boolean(tree, id)
    },
    offset_date_time = {
      unserialize_offset_date_time(tree, id)
    },
    local_date_time = {
      unserialize_local_date_time(tree, id)
    },
    local_date = {
      unserialize_local_date(tree, id)
    },
    local_time = {
      unserialize_local_time(tree, id)
    },
    string = {
      unserialize_string(tree, id)
    },
    basic_string = {
      unserialize_basic_string(tree, id)
    },
    multiline_basic_string = {
      unserialize_multiline_basic_string(tree, id)
    },
    literal_string = {
      unserialize_literal_string(tree, id)
    },
    multiline_literal_string = {
      unserialize_multiline_literal_string(tree, id)
    },
    stop("Unsupported token type: ", tree$type[id])
  )
  elt
}

unserialize_key <- function(tree, id) {
  switch(
    tree$type[id],
    bare_key = {
      unserialize_bare_key(tree, id)
    },
    quoted_key = {
      unserialize_quoted_key(tree, id)
    },
    dotted_key = {
      unserialize_dotted_key(tree, id)
    },
    stop("Unsupported key type in pair: ", tree$type[id])
  )
}

unserialize_bare_key <- function(tree, id) {
  stopifnot(tree$type[id] == "bare_key")
  tree$code[id]
}

unserialize_quoted_key <- function(tree, id) {
  stopifnot(tree$type[id] == "quoted_key")
  unserialize_element(tree, tree$children[[id]][1])
}

unserialize_dotted_key <- function(tree, id) {
  stopifnot(tree$type[id] == "dotted_key")
  children <- tree$children[[id]]
  c(
    unserialize_key(tree, children[1]),
    unserialize_key(tree, children[3])
  )
}

unserialize_key_with_ids <- function(tree, id) {
  switch(
    tree$type[id],
    bare_key = {
      unserialize_bare_key_with_ids(tree, id)
    },
    quoted_key = {
      unserialize_quoted_key_with_ids(tree, id)
    },
    dotted_key = {
      unserialize_dotted_key_with_ids(tree, id)
    },
    stop("Unsupported key type in pair: ", tree$type[id])
  )
}

unserialize_bare_key_with_ids <- function(tree, id) {
  stopifnot(tree$type[id] == "bare_key")
  list(key = tree$code[id], ids = id)
}

unserialize_quoted_key_with_ids <- function(tree, id) {
  stopifnot(tree$type[id] == "quoted_key")
  list(
    key = unserialize_element(tree, tree$children[[id]][1]),
    ids = id
  )
}

unserialize_dotted_key_with_ids <- function(tree, id) {
  stopifnot(tree$type[id] == "dotted_key")
  children <- tree$children[[id]]
  ki1 <- unserialize_key_with_ids(tree, children[1])
  ki2 <- unserialize_key_with_ids(tree, children[3])
  list(key = c(ki1$key, ki2$key), ids = c(ki1$ids, ki2$ids))
}


# TODO: support 64 bit integers
unserialize_integer <- function(tree, id) {
  stopifnot(tree$type[id] == "integer")
  code <- tree$code[id]
  base <- if (startsWith(code, "0x")) {
    16L
  } else if (startsWith(code, "0o")) {
    code <- substr(code, 3L, nchar(code))
    8L
  } else if (startsWith(code, "0b")) {
    code <- substr(code, 3L, nchar(code))
    2L
  } else {
    10L
  }
  code <- gsub("_", "", code, fixed = TRUE)
  strtoi(code, base = base)
}

unserialize_float <- function(tree, id) {
  stopifnot(tree$type[id] == "float")
  code <- tree$code[id]
  code <- gsub("_", "", code, fixed = TRUE)
  as.numeric(code)
}

unserialize_boolean <- function(tree, id) {
  stopifnot(tree$type[id] == "boolean")
  if (tree$code[id] == "true") {
    TRUE
  } else {
    FALSE
  }
}

unserialize_offset_date_time <- function(tree, id) {
  stopifnot(tree$type[id] == "offset_date_time")
  code <- tree$code[id]
  parse_iso_8601(code)
}

unserialize_local_date_time <- function(tree, id) {
  stopifnot(tree$type[id] == "local_date_time")
  code <- tree$code[id]
  # parse it as a local time
  t <- parse_iso_8601(code, "")
  as.POSIXlt(.POSIXct(t, tz = ""))
}

unserialize_local_date <- function(tree, id) {
  stopifnot(tree$type[id] == "local_date")
  code <- tree$code[id]
  as.Date(code, tryFormats = "%Y-%m-%d")
}

unserialize_local_time <- function(tree, id) {
  stopifnot(tree$type[id] == "local_time")
  code <- tree$code[id]
  hms <- strsplit(code, ":", fixed = TRUE)[[1]]
  structure(
    as.difftime(
      strtoi(hms[1]) * 3600 + strtoi(hms[2]) * 60 + as.numeric(hms[3]),
      units = "secs"
    ),
    class = c("hms", "difftime")
  )
}

# TODO: embedded NULL not supported

unserialize_string <- function(tree, id, type = "string") {
  stopifnot(tree$type[id] == type)
  chdn <- tree$children[[id]]
  unserialize_element(tree, chdn[1])
}

# - Embedded zero is not supported in R, so \u0000 will error.
# - All other escapes supported by TOML are also supported by R, hence
#   we use `eval(parse())`.
# - The parser will error for the escapes that are not supported by TOML,
#   even if they are supported by R.

unserialize_basic_string <- function(tree, id) {
  stopifnot(tree$type[id] == "basic_string")
  # also has " delimiters
  chdn <- tree$children[[id]]
  str <- paste(tree$code[chdn], collapse = "")
  eval(parse(text = str, keep.source = FALSE))
}

unserialize_multiline_basic_string <- function(tree, id) {
  stopifnot(tree$type[id] == "multiline_basic_string")
  chdn <- tree$children[[id]]
  str <- paste(tree$code[chdn], collapse = "")
  # trim delimiters first
  str <- substr(str, 4L, nchar(str) - 3L)
  # remove leading newline if present
  if (startsWith(str, "\n")) {
    str <- substr(str, 2L, nchar(str))
  }
  # replace \ + \n + whitespace with nothing
  str <- gsub("\\\\\n\\s*", "", str, perl = TRUE)
  # if we escape the single quotes, then we can use R's parser
  str <- gsub("'", "\\'", str, fixed = TRUE)
  eval(parse(text = paste0("'", str, "'"), keep.source = FALSE))
}

unserialize_literal_string <- function(tree, id) {
  stopifnot(tree$type[id] == "literal_string")
  chdn <- tree$children[[id]]
  str <- paste(tree$code[chdn], collapse = "")
  # trim delimiters first
  str <- substr(str, 2L, nchar(str) - 1L)
  str
}

unserialize_multiline_literal_string <- function(tree, id) {
  stopifnot(tree$type[id] == "multiline_literal_string")
  chdn <- tree$children[[id]]
  str <- paste(tree$code[chdn], collapse = "")
  # trim delimiters first
  str <- substr(str, 4L, nchar(str) - 3L)
  # remove leading newline if present
  if (startsWith(str, "\n")) {
    str <- substr(str, 2L, nchar(str))
  }
  str
}

unserialize_array <- function(tree, id) {
  children <- tree$dom_children[[id]]
  result <- vector("list", length(children))
  for (i in seq_along(children)) {
    result[[i]] <- unserialize_element(tree, children[i])
  }
  result
}

unserialize_table <- function(tree, id) {
  children <- tree$dom_children[[id]]
  res <- named_list(length(children))
  names(res) <- tree$dom_name[children]
  for (i in seq_along(children)) {
    res[[i]] <- unserialize_element(tree, children[i])
  }
  res
}
