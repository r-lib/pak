
# This is based on rlang:::obj_type_friendly, but adapted to cli

friendly_type <- local({

friendly_type <- function(x, value = TRUE, length = FALSE) {
  if (is_missing(x)) {
    return("absent")
  }

  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      return("a {.cls quosure} object")
    } else if (identical(class(x), "data.frame")) {
      return("a data frame")
    } else if (identical(class(x), c("tbl_df", "tbl", "data.frame"))) {
      return("a tibble")
    } else {
      # this is sometimes wrong for 'h', but ce la vie
      fst <- tolower(substr(class(x)[1], 1, 1))
      prop <- if (fst %in% c("a", "e", "i", "o", "u")) {
        "an"
      } else {
        "a"
      }
      return(paste0(prop, " {.cls {class(x)[1]}} object"))
    }
  }

  if (!is_vector(x)) {
    return(as_friendly_type(typeof(x)))
  }

  n_dim <- length(dim(x))

  if (value && !n_dim) {
    if (is_na(x)) {
      return(switch(
        typeof(x),
        logical = "{.code NA}",
        integer = "an integer {.code NA}",
        double = "a numeric {.code NA}",
        complex = "a complex {.code NA}",
        character = "a character {.code NA}",
        typeof(x)
      ))
    }
    if (length(x) == 1 && !is_list(x)) {
      return(switch(
        typeof(x),
        logical = if (x) "{.code TRUE}" else "{.code FALSE}",
        integer = "an integer",
        double = "a number",
        complex = "a complex number",
        character = if (nzchar(x)) "a string" else "{.code \"\"}",
        raw = "a raw value",
        sprintf("a %s value", typeof(x))
      ))
    }
    if (length(x) == 0) {
      return(switch(
        typeof(x),
        logical = "an empty logical vector",
        integer = "an empty integer vector",
        double = "an empty numeric vector",
        complex = "an empty complex vector",
        character = "an empty character vector",
        raw = "an empty raw vector",
        list = "an empty list",
        sprintf("a %s of length one", typeof(x))
      ))
    }
  }

  type <- friendly_vector_type(typeof(x), n_dim)

  if (length && !n_dim) {
    type <- paste0(type, sprintf(" of length %s", length(x)))
  }

  type
}

friendly_vector_type <- function(type, n_dim) {
  if (type == "list") {
    if (n_dim < 2) {
      return("a list")
    } else if (n_dim == 2) {
      return("a list matrix")
    } else {
      return("a list array")
    }
  }

  type <- switch(
    type,
    logical = "a logical %s",
    integer = "an integer %s",
    numeric = ,
    double = "a double %s",
    complex = "a complex %s",
    character = "a character %s",
    raw = "a raw %s",
    type = paste0("a ", type, " %s")
  )

  if (n_dim < 2) {
    kind <- "vector"
  } else if (n_dim == 2) {
    kind <- "matrix"
  } else {
    kind <- "array"
  }
  sprintf(type, kind)
}

as_friendly_type <- function(type) {
  switch(
    type,

    list = "a list",

    NULL = "NULL",
    environment = "an environment",
    externalptr = "a pointer",
    weakref = "a weak reference",
    S4 = "an S4 object",

    name = ,
    symbol = "a symbol",
    language = "a call",
    pairlist = "a pairlist node",
    expression = "an expression vector",

    char = "an internal string",
    promise = "an internal promise",
    ... = "an internal dots object",
    any = "an internal {.code any} object",
    bytecode = "an internal bytecode object",

    primitive = ,
    builtin = ,
    special = "a primitive function",
    closure = "a function",

    type
  )
}

is_missing <- function(x) {
  missing(x) || identical(x, quote(expr = ))
}

is_vector <- function(x) {
  t <- typeof(x)
  t %in% c(
    "logical",
    "integer",
    "double",
    "complex",
    "character",
    "raw",
    "list"
  )
}

is_scalar_vector <- function(x) {
  is_vector(x) && length(x) == 1L
}

is_na <- function(x) {
  is_scalar_vector(x) && is.na(x)
}

is_list <- function(x) {
  typeof(x) == "list"
}

list(
  .internal = environment(),
  friendly_type = friendly_type
)

})

typename <- friendly_type$friendly_type
