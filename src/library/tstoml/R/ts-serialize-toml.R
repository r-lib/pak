#' Serialize an R object to TOML
#'
#' Create TOML from an R object.
#' Note that this function is not a generic serializer that can represent
#' any R object in TOML. Also, you cannot expect that
#' [ts_unserialize_toml()] will do the exact inverse of
#' [ts_serialize_toml()].
#'
#  tstoml functions [update_selected()] and [insert_into_selected()] use
# [ts_serialize_toml()] to create new TOML code.
#'
#' See the examples below on how to create all possible TOML elements with
#' [ts_serialize_toml()].
#'
#' @param obj R object to serialize.
#' @param file If not `NULL` then the result if written to this file.
#' @param collapse If `file` is `NULL` then whether to return a character
#'   scalar or a character vector.
#' @param options A named list of `tstoml` options, see
#'   [tstoml_options()].
#' @return If `file` is `NULL` then a character scalar (`collapse` = TRUE)
#'   or vector (`collapse` = FALSE). If `file` is not `NULL` then nothing.
#'
#' @export
#' @seealso [ts_unserialize_toml()] for the opposite.

ts_serialize_toml <- function(
  obj,
  file = NULL,
  collapse = FALSE,
  options = NULL
) {
  lns <- stl_table(
    NULL,
    obj,
    options = options,
    arg = ts_caller_arg(obj),
    call = ts_caller_env()
  )

  # if it starts with a table or array of tables, there is an empty line
  if (lns[1] == "") {
    lns <- lns[-1]
  }

  if (is.null(file)) {
    if (collapse) {
      paste0(lns, collapse = "\n")
    } else {
      lns
    }
  } else {
    writeLines(lns, con = file)
    invisible(NULL)
  }
}

#' @export
#' @rdname ts_serialize_toml
#' @param ... Elements of the TOML table, array, inline table or array
#'   of tables.
#' @details Use `ts_toml_table()` to make a list to be serialized as a TOML
#'   table. All named lists are serialized as TOML tables by default, so
#'   this is only readability.

ts_toml_table <- function(...) {
  structure(
    list(...),
    class = c("ts_toml_table", "list")
  )
}

#' @export
#' @rdname ts_serialize_toml
#' @details Use `ts_toml_inline_table()` to make a list to be serialized
#'   as an inline TOML table. By default named lists are serialized as
#'   regular TOML tables, if possible.

ts_toml_inline_table <- function(...) {
  tab <- list(...)
  if (!is_named(tab)) {
    stop(ts_cnd("All elements of TOML tables must be named."))
  }
  structure(
    tab,
    class = c("ts_toml_inline_table", "list")
  )
}

#' @export
#' @rdname ts_serialize_toml
#' @details Use `ts_toml_array()` to make a list to be serialized as a TOML
#'   array. By default un-named lists are serialized as arrays, unless they
#'   are lists of named lists (which are serialized as arrays of tables).
#'   Use this funtion to mark any list to be serialized as an array.

ts_toml_array <- function(...) {
  structure(
    list(...),
    class = c("ts_toml_array", "list")
  )
}

#' @export
#' @rdname ts_serialize_toml
#' @details Use `ts_toml_array_of_tables()` to make a list to be serialized
#'   as a TOML array of tables. It must a list of named lists. By default
#'   lists of named lists are serialized as arrays of tables, so this is
#'   only for readability.

ts_toml_array_of_tables <- function(...) {
  aot <- list(...)
  if (length(aot) == 0L) {
    stop(ts_cnd("TOML array of tables must have at least one element."))
  }
  if (!all(map_lgl(aot, is_named))) {
    stop(ts_cnd("All elements of a TOML array of tables must be named lists."))
  }
  structure(
    aot,
    class = c("ts_toml_array_of_tables", "list")
  )
}

ts_toml_key <- function(...) {
  key <- c(...)
  # TODO: argument check
  # TODO: escape keys, quote if needed
  paste(key, collapse = ".")
}

get_stl_type <- function(x) {
  if (inherits(x, "ts_toml_inline_table")) {
    "pair"
  } else if (inherits(x, "ts_toml_array")) {
    "pair"
  } else if (inherits(x, "ts_toml_table")) {
    "table"
  } else if (inherits(x, "ts_toml_array_of_tables")) {
    "array_of_tables"
  } else if (!is.list(x)) {
    "pair"
  } else if (is_named(x)) {
    "table"
  } else if (length(x) > 0 && all(map_lgl(x, is_named))) {
    "array_of_tables"
  } else {
    "pair"
  }
}

stl_type_names <- c(
  "pair" = "key-value pair",
  "table" = "table",
  "array_of_tables" = "array of tables"
)

stl_table <- function(
  name,
  obj,
  options = NULL,
  path = NULL,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  stopifnot(is_named(obj))
  c(
    if (!is.null(name)) {
      c("", paste0("[", paste(c(path, name), collapse = "."), "]"))
    },
    stl_table_body(
      obj,
      options = options,
      path = c(path, name),
      arg = arg,
      call = call
    )
  )
}

stl_inline <- function(
  obj,
  options = NULL,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  if (inherits(obj, "POSIXct")) {
    stl_offset_date_time(obj, options = options, arg = arg, call = call)
  } else if (inherits(obj, "POSIXlt")) {
    stl_local_date_time(obj, options = options, arg = arg, call = call)
  } else if (inherits(obj, "Date")) {
    stl_local_date(obj, options = options, arg = arg, call = call)
  } else if (inherits(obj, "difftime")) {
    stl_local_time(obj, options = options, arg = arg, call = call)
  } else if (is.list(obj) && is_named(obj)) {
    stl_inline_table(obj, options = options, arg = arg, call = call)
  } else if (is.list(obj)) {
    stl_inline_array(obj, options = options, arg = arg, call = call)
  } else if (is.double(obj)) {
    stl_float(obj, options = options, arg = arg, call = call)
  } else if (is.character(obj)) {
    stl_string(obj, options = options, arg = arg, call = call)
  } else if (is.integer(obj)) {
    stl_integer(obj, options = options, arg = arg, call = call)
  } else if (is.logical(obj)) {
    stl_boolean(obj, options = options, arg = arg, call = call)
  } else {
    stop(ts_cnd(
      "Invalid argument: `{arg}`. Cannot convert {typename(obj)} to TOML.",
      call = call
    ))
  }
}

#' Serialize a TOML value
#'
#' @param obj R object to serialize as a TOML value.
#' @param options A named list of `tstoml` options, see
#'   [tstoml_options()].
# '@export

ts_serialize_toml_value <- function(obj, options = NULL) {
  stl_inline(obj, options = options)
}

stl_table_body <- function(
  obj,
  options = NULL,
  inline = FALSE,
  path = NULL,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  types <- map_chr(obj, get_stl_type)
  wpairs <- which(types == "pair")
  wtables <- which(types == "table")
  waots <- which(types == "array_of_tables")

  c(
    unlist(
      use.names = FALSE,
      lapply(wpairs, function(idx) {
        paste0(
          names(obj)[idx],
          " = ",
          stl_inline(obj[[idx]], options = options, arg = arg, call = call)
        )
      })
    ),
    unlist(
      use.names = FALSE,
      lapply(wtables, function(idx) {
        stl_table(
          names(obj)[idx],
          obj[[idx]],
          options = options,
          path = path,
          arg = arg,
          call = call
        )
      })
    ),
    unlist(
      use.names = FALSE,
      lapply(waots, function(idx) {
        stl_array_of_tables(
          names(obj)[idx],
          obj[[idx]],
          options = options,
          path = path,
          arg = arg,
          call = call
        )
      })
    )
  )
}

stl_array_of_tables <- function(
  name,
  obj,
  options = options,
  path = path,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  unlist(lapply(seq_along(obj), function(i) {
    c(
      "",
      paste0("[[", paste0(c(path, name), collapse = "."), "]]"),
      stl_table_body(
        obj[[i]],
        options = options,
        path = c(path, name),
        arg = arg,
        call = call
      )
    )
  }))
}

stl_float <- function(
  obj,
  options = NULL,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  obj <- as_toml_float(obj, arg = arg, call = call)
  if (is.nan(obj)) {
    "nan"
  } else if (obj == Inf) {
    "inf"
  } else if (obj == -Inf) {
    "-inf"
  } else {
    format(obj, nsmall = 1)
  }
}

# TODO: check scalar, escape, not NA, etc.
# TODO: select between various string types
stl_string <- function(
  obj,
  options = NULL,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  encodeString(obj, quote = "\"")
}

stl_integer <- function(
  obj,
  options = NULL,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  obj <- as_toml_integer(obj, arg = arg, call = call)
  as.character(obj)
}

stl_boolean <- function(
  obj,
  options = NULL,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  obj <- as_toml_boolean(obj, arg = arg, call = call)
  if (obj) "true" else "false"
}

# TODO: check scalar, not NA
stl_offset_date_time <- function(
  obj,
  options = NULL,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  z <- format(obj, "%z")
  paste0(
    format(obj, "%Y-%m-%dT%H:%M:%S"),
    substr(z, 1, 3),
    ":",
    substr(z, 4, 5)
  )
}

# TODO: check scalar, not NA
stl_local_date_time <- function(
  obj,
  options = NULL,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  format(obj, "%Y-%m-%dT%H:%M:%S")
}

# TODO: check scalar, not NA
stl_local_date <- function(
  obj,
  options = NULL,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  format(obj, "%Y-%m-%d")
}

# TODO: check scalar, not NA
stl_local_time <- function(
  obj,
  options = NULL,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  secs <- as.double(obj, units = "secs")
  tics_per_second <- 1e+06
  tics_per_minute <- 6e+07
  tics_per_hour <- 3.6e+09
  tics <- secs * tics_per_second
  h <- trunc(tics / tics_per_hour)
  tics <- tics - h * tics_per_hour
  m <- trunc(tics / tics_per_minute)
  tics <- tics - m * tics_per_minute
  s <- trunc(tics / tics_per_second)
  tics <- tics - s * tics_per_second
  paste0(
    formatC(h, format = "d", width = 2, flag = "0"),
    ":",
    formatC(m, format = "d", width = 2, flag = "0"),
    ":",
    formatC(s, format = "d", width = 2, flag = "0"),
    ".",
    formatC(as.integer(tics), format = "d", width = 6, flag = "0")
  )
}

stl_inline_table <- function(
  obj,
  options = options,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  paste0(
    "{ ",
    paste0(
      names(obj),
      " = ",
      map_chr(obj, stl_inline, options = options),
      collapse = ", "
    ),
    " }"
  )
}

stl_inline_array <- function(
  obj,
  options = options,
  arg = ts_caller_arg(obj),
  call = ts_caller_env()
) {
  paste0(
    "[ ",
    paste(map_chr(obj, stl_inline, options = options), collapse = ", "),
    " ]"
  )
}
