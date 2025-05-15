is_character <- function(x) {
  if (!is.character(x)) {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must be a character vector without {.code NA},
             but it is {.type {x}}",
      env = environment()
    )
  } else if (anyNA(x)) {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must be a character vector without {.code NA},
             but it has {sum(is.na(x))} {.code NA} value{?s}.",
      env = environment()
    )
  } else {
    TRUE
  }
}

is_string <- function(x) {
  if (is.character(x) && length(x) == 1 && !is.na(x)) return(TRUE)
  if (is.character(x) && length(x) == 1 && is.na(x)) {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must not be {.code NA}.",
      env = environment()
    )
  } else {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must be a string (character scalar),
           but it is {.type {x}}.",
      env = environment()
    )
  }
}

is_optional_string <- function(x) {
  if (is.null(x) || is_string(x)) return(TRUE)
  structure(
    FALSE,
    msg = "{.arg {(.arg)}} must be a path (character scalar),
           but it is {.type {x}}.",
    env = environment()
  )
}

is_flag <- function(x) {
  if (is.logical(x) && length(x) == 1 && !is.na(x)) return(TRUE)
  if (is.logical(x) && length(x) == 1 && is.na(x)) {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must not be {.code NA}.",
      env = environment()
    )
  } else {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must be a flag (logical scalar),
             but it is {.type {x}}.",
      env = environment()
    )
  }
}

## To be refined

is_path <- function(x) {
  if (is_string(x)) return(TRUE)
  structure(
    FALSE,
    msg = "{.arg {(.arg)}} must be a path (character scalar),
          but it is {.type {x}}.",
    env = environment()
  )
}

is_optional_path <- function(x) {
  if (is_optional_string(x)) return(TRUE)
  structure(
    FALSE,
    msg = "{.arg {(.arg)}} must be a path (character scalar) or {.code NULL},
          but it is {.type {x}}.",
    env = environment()
  )
}

all_named <- function(x) {
  if (length(names(x)) == length(x) && all(names(x) != "")) return(TRUE)
  structure(
    FALSE,
    msg = "All elements in {.arg {(.arg)}} must be named.",
    env = environment()
  )
}

is_existing_file <- function(x) {
  assert_that(is_path(x))
  if (!file.exists(x)) {
    structure(
      FALSE,
      msg = "Path {.path {x}} (from {.arg {(.arg)}}) does not exist",
      env = environment()
    )
  } else if (file.info(x)$isdir) {
    structure(
      FALSE,
      msg = "File {.path {x}} (from {.arg {(.arg)}}) must be a
             regular file, not a directory",
      env = environment()
    )
  } else {
    TRUE
  }
}

is_platform_list <- function(x) {
  if (!is.character(x)) {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must be a list of platforms, a non-empty
             character vector, but it is {.type {x}}",
      env = environment()
    )
  } else if (length(x) == 0) {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must be a list of platforms, a non-empry
             character vector",
      env = environment()
    )
  } else if (anyNA(x)) {
    structure(
      FALSE,
      msg = "{.arg ((.arg)}} must be a list of platforms, a character
             vector without missing values, but it has {sum(is.na(x))}
             missing value{?s}.",
      env = environment()
    )
  } else {
    TRUE
  }
}

is_dependencies <- function(x) {
  dep_types <- c(pkg_dep_types(), c("soft", "hard", "all"))
  valid <- function(x) {
    x %in% dep_types | x %in% extra_config_fields(x)
  }
  if (
    is_na_scalar(x) ||
      isTRUE(x) ||
      identical(x, FALSE) ||
      (is_character(x) && all(valid(x))) ||
      (is.list(x) &&
        all(names(x) == c("direct", "indirect")) &&
        all(valid(unlist(x))))
  ) {
    return(TRUE)
  }

  structure(
    FALSE,
    msg = c(
      "{.arg {(.arg)}} must be one of the following: {.code NA},
         {.code TRUE}, {.code FALSE}, a character vector of dependency types,
         a named list with entries {.code direct} and {.code indirect},
         both character vectors of dependency types.",
      "i" = "valid dependency types are: {.val {dep_types}}, and
         {.code config/needs/*} types"
    ),
    env = environment()
  )
}

is_r_version_list <- function(x) {
  if (!is.character(x)) {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must be a list of R versions, a character
             vector, but it is {.type {x}}.",
      env = environment()
    )
  } else if (length(x) == 0) {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must be a non-empty list of R versions, a
             character vector, but it is empty.",
      env = environment()
    )
  } else if (anyNA(xv <- package_version(x, strict = FALSE))) {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must be a list of R versions, but it has
             {sum(is.na(cv))} invalid version numbers: {x[is.na(cv)]}.",
      env = environment()
    )
  } else {
    TRUE
  }
}

is_difftime <- function(x) {
  if (inherits(x, "difftime")) return(TRUE)
  structure(
    FALSE,
    msg = "{.arg {(.arg)}} must be a {.cls difftime} object, but it is
           {.type {x}}.",
    env = environment()
  )
}

is_count <- function(x, min = 0L) {
  if (
    is.numeric(x) &&
      length(x) == 1 &&
      !is.na(x) &&
      as.integer(x) == x &&
      x >= min
  ) {
    return(TRUE)
  }
  if (!is.numeric(x) || length(x) != 1 || (!is.na(x) && as.integer(x) != x)) {
    structure(
      FALSE,
      msg = c(
        if (min == 0) {
          "{.arg {(.arg)}} must be a count, a non-negative integer scalar."
        } else {
          "{.arg {(.arg)}} must be a count, an integer scalar,
           at least {min}."
        },
        i = "It is {.type {x}}."
      ),
      env = environment()
    )
  } else if (!is.na(x) && x < min) {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must be at least {min}.",
      env = environment()
    )
  } else {
    structure(
      FALSE,
      msg = "{.arg {(.arg)}} must not be a missing value ({.code NA}).",
      env = environment()
    )
  }
}
