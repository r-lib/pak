is_string <- function(x, na = FALSE) {
  if (na) {
    is.character(x) && length(x) == 1
  } else {
    is.character(x) && length(x) == 1 && !is.na(x)
  }
}

is_count <- function(x, positive = FALSE) {
  limit <- if (positive) 1L else 0L
  is.numeric(x) &&
    length(x) == 1 &&
    !is.na(suppressWarnings(as.integer(x))) &&
    suppressWarnings(as.integer(x)) == x &&
    !is.na(x) &&
    x >= limit
}

as_count <- function(
  x,
  positive = FALSE,
  null = FALSE,
  arg = ts_caller_arg(x),
  call = ts_caller_env()
) {
  if (is.null(x) && null) {
    return(x)
  }
  if (is_count(x, positive = positive)) {
    return(as.integer(x))
  }

  if (is_string(x)) {
    xi <- suppressWarnings(as.integer(x))
    if (is_count(xi, positive = positive)) {
      return(xi)
    }
  }

  limit <- if (positive) 1L else 0L
  if (is.numeric(x) && length(x) != 1) {
    stop(ts_cnd(
      call = call,
      "Invalid argument: `{arg}` must be an integer scalar, not a vector."
    ))
  } else if (is.numeric(x) && length(x) == 1 && is.na(x)) {
    stop(ts_cnd(
      call = call,
      "Invalid argument: `{arg}` must not be `NA`."
    ))
  } else if (is.numeric(x) && length(x) == 1 && !is.na(x) && x < limit) {
    stop(ts_cnd(
      call = call,
      "Invalid argument: `{arg}` must be \\
      {if (positive) 'positive' else 'non-negative'}."
    ))
  } else {
    stop(ts_cnd(
      call = call,
      "Invalid argument: `{arg}` must be a \\
      {if (positive) 'positive' else 'non-negative'} integer scalar, \\
      but it is {typename(x)}."
    ))
  }
}

as_choice <- function(
  x,
  choices,
  arg = ts_caller_arg(x),
  call = ts_caller_env()
) {
  if (is_string(x) && tolower(x) %in% choices) {
    return(tolower(x))
  }

  cchoices <- paste0("'", choices, "'", collapse = ", ")
  if (is_string(x)) {
    stop(ts_cnd(
      call = call,
      "Invalid argument: `{arg}` must be one of {cchoices}, but it is '{x}'."
    ))
  } else {
    stop(ts_cnd(
      call = call,
      "Invalid argument: `{arg}` must be a string scalar, one of \\
       {cchoices}, but it is {typename(x)}."
    ))
  }
}

opt_indent_width_default <- function() {
  4L
}

opt_indent_style_default <- function() {
  "space"
}

#' tstoml options
#'
#' Options that control the behavior of tstoml functions.
#'
#' ## Formatting options:
#'
#' * `indent_width`: integer, the number of spaces to use for indentation
#'   when `indent_style` is `"space"`. Default is
#'   `r opt_indent_width_default()`.
#' * `indent_style`: string, either `"space"` or `"tab"`, the type of
#'   indentation to use. Default is `r opt_indent_style_default()`.
#'
#' @name tstoml_options
NULL

as_tstoml_options <- function(
  x,
  arg = ts_caller_arg(x),
  call = ts_caller_env()
) {
  nms <- c(
    "indent_width",
    "indent_style",
    # this is internal only
    internal = "insert_empty_line_before_tables"
  )
  if (
    (is.list(x) || is.null(x)) &&
      is_named(x, null = TRUE) &&
      all(names(x) %in% nms)
  ) {
    force(arg)
    # older R versions would set NULL to logical(), not list()
    x <- as.list(x)

    x[["indent_width"]] <- as_count(
      x[["indent_width"]] %||% opt_indent_width_default(),
      arg = as_ts_caller_arg(substitute(
        x[["indent_width"]],
        list(x = arg[[1]])
      )),
      call = call
    )

    x[["indent_style"]] <- as_choice(
      x[["indent_style"]] %||% opt_indent_style_default(),
      choices = c("space", "tab"),
      arg = as_ts_caller_arg(substitute(
        x[["indent_style"]],
        list(x = arg[[1]])
      )),
      call = call
    )

    return(x)
  }

  if (!is.list(x) && !is.null(x)) {
    stop(ts_cnd(
      call = call,
      "Invalid argument: `{arg}` must be a named list of tsjsonc options \\
       (see `?tsjsonc_options`), but it is {typename(options)}."
    ))
  }

  if (!is_named(x, null = TRUE)) {
    stop(ts_cnd(
      call = call,
      "Invalid argument: `{arg}` must be a named list of tsjsonc options \\
       (see `?tsjsonc_options`), but not all of its entries are named."
    ))
  }

  bad <- paste0("`", unique(setdiff(names(x), nms)), "`")
  good <- paste0("`", nms[names(nms) != "internal"], "`")
  stop(ts_cnd(
    call = call,
    "Invalid argument: `{arg}` contains unknown tsjsonc \\
    option{plural(length(bad))}: {ts_collapse(bad)}. Known tsjsonc options \\
    are: {ts_collapse(good)}."
  ))
}

as_toml_float <- function(
  x,
  arg = ts_caller_arg(x),
  call = ts_caller_env()
) {
  if (is.numeric(x) && length(x) == 1) {
    if (!is.na(x)) {
      return(as.numeric(x))
    } else {
      return(NaN)
    }
  }

  stop(ts_cnd(
    "Invalid argument: `{arg}` contains an atomic numeric vector of length \\
     {length(x)}. TOML only supports numeric scalars and lists.",
    call = call
  ))
}

as_toml_boolean <- function(
  x,
  arg = ts_caller_arg(x),
  call = ts_caller_env()
) {
  if (is.logical(x) && length(x) == 1 && !is.na(x)) {
    return(x)
  }

  if (!is.logical(x) || length(x) != 1) {
    stop(ts_cnd(
      "Invalid argument: `{arg}` contains an atomic logical vector of length \\
       {length(x)}. TOML only supports logical scalars and lists.",
      call = call
    ))
  }

  stop(ts_cnd(
    "Invalid argument: `{arg}` contains a logical `NA`. TOML does not support \\
    logical `NA` values.",
    call = call
  ))
}

as_toml_integer <- function(
  x,
  arg = ts_caller_arg(x),
  call = ts_caller_env()
) {
  if (is.integer(x) && length(x) == 1 && !is.na(x)) {
    return(x)
  }

  if (!is.integer(x) || length(x) != 1) {
    stop(ts_cnd(
      "Invalid argument: `{arg}` contains an atomic integer vector of length \\
       {length(x)}. TOML only supports integer scalars and lists.",
      call = call
    ))
  }

  stop(ts_cnd(
    "Invalid argument: `{arg}` contains an integer `NA`. TOML does not support \\
    integer `NA` values.",
    call = call
  ))
}

as_character <- function(x) {
  # TODO
  x # nocov
}

as_existing_file <- function(x) {
  # TODO
  x # nocov
}
