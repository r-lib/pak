#' Format and interpolate a string
#'
#' Expressions enclosed by braces will be evaluated as R code. Long strings are
#' broken by line and concatenated together. Leading whitespace and blank lines
#' from the first and last lines are automatically trimmed.
#'
#' @param .x \[`listish`]\cr An environment, list, or data frame used to lookup values.
#' @param ... \[`expressions`]\cr Unnamed arguments are taken to be expression
#'     string(s) to format. Multiple inputs are concatenated together before formatting.
#'     Named arguments are taken to be temporary variables available for substitution.
#' @param .sep \[`character(1)`: \sQuote{""}]\cr Separator used to separate elements.
#' @param .envir \[`environment`: `parent.frame()`]\cr Environment to evaluate each expression in. Expressions are
#'   evaluated from left to right. If `.x` is an environment, the expressions are
#'   evaluated in that environment and `.envir` is ignored. If `NULL` is passed, it is equivalent to [emptyenv()].
#' @param .open \[`character(1)`: \sQuote{\\\{}]\cr The opening delimiter. Doubling the
#'   full delimiter escapes it.
#' @param .close \[`character(1)`: \sQuote{\\\}}]\cr The closing delimiter. Doubling the
#'   full delimiter escapes it.
#' @param .transformer \[`function]`\cr A function taking three parameters `code`, `envir` and
#'   `data` used to transform the output of each block before, during, or after
#'   evaluation. For example transformers see `vignette("transformers")`.
#' @param .na \[`character(1)`: \sQuote{NA}]\cr Value to replace `NA` values
#'   with. If `NULL` missing values are propagated, that is an `NA` result will
#'   cause `NA` output. Otherwise the value is replaced by the value of `.na`.
#' @param .null \[`character(1)`: \sQuote{character()}]\cr Value to replace
#'   NULL values with. If `character()` whole output is `character()`. If
#'   `NULL` all NULL values are dropped (as in `paste0()`). Otherwise the
#'   value is replaced by the value of `.null`.
#' @param .comment \[`character(1)`: \sQuote{#}]\cr Value to use as the comment
#'   character.
#' @param .literal \[`boolean(1)`: \sQuote{FALSE}]\cr Whether to treat single or
#'   double quotes, backticks, and comments as regular characters (vs. as
#'   syntactic elements), when parsing the expression string. Setting `.literal
#'   = TRUE` probably only makes sense in combination with a custom
#'   `.transformer`, as is the case with `glue_col()`. Regard this argument
#'   (especially, its name) as experimental.
#' @param .trim \[`logical(1)`: \sQuote{TRUE}]\cr Whether to trim the input
#'   template with [trim()] or not.
#' @seealso <https://www.python.org/dev/peps/pep-0498/> and
#'   <https://www.python.org/dev/peps/pep-0257/> upon which this is based.
#' @examples
#' name <- "Fred"
#' age <- 50
#' anniversary <- as.Date("1991-10-12")
#' glue('My name is {name},',
#'   'my age next year is {age + 1},',
#'   'my anniversary is {format(anniversary, "%A, %B %d, %Y")}.')
#'
#' # single braces can be inserted by doubling them
#' glue("My name is {name}, not {{name}}.")
#'
#' # Named arguments can be used to assign temporary variables.
#' glue('My name is {name},',
#'   ' my age next year is {age + 1},',
#'   ' my anniversary is {format(anniversary, "%A, %B %d, %Y")}.',
#'   name = "Joe",
#'   age = 40,
#'   anniversary = as.Date("2001-10-12"))
#'
#' # `glue()` can also be used in user defined functions
#' intro <- function(name, profession, country){
#'   glue("My name is {name}, a {profession}, from {country}")
#' }
#' intro("Shelmith", "Senior Data Analyst", "Kenya")
#' intro("Cate", "Data Scientist", "Kenya")
#'
#' # `glue_data()` is useful in magrittr pipes
#' if (require(magrittr)) {
#'
#' mtcars %>% glue_data("{rownames(.)} has {hp} hp")
#'
#' # Or within dplyr pipelines
#' if (require(dplyr)) {
#'
#' head(iris) %>%
#'   mutate(description = glue("This {Species} has a petal length of {Petal.Length}"))
#'
#' }}
#'
#' # Alternative delimiters can also be used if needed
#' one <- "1"
#' glue("The value of $e^{2\\pi i}$ is $<<one>>$.", .open = "<<", .close = ">>")
#' @useDynLib glue glue_
#' @name glue
#' @export
glue_data <- function(.x, ..., .sep = "", .envir = parent.frame(),
  .open = "{", .close = "}", .na = "NA", .null = character(),
  .comment = "#", .literal = FALSE, .transformer = identity_transformer, .trim = TRUE) {

  if (is.null(.envir)) {
    .envir <- emptyenv()
  }

  # Perform all evaluations in a temporary environment
  if (is.null(.x)) {
    parent_env <- .envir
  } else if (is.environment(.x)) {
    parent_env <- .x
  } else {
    parent_env <- list2env(as.list(.x), parent = .envir)
  }

  # Capture unevaluated arguments
  dots <- eval(substitute(alist(...)))
  named <- has_names(dots)

  # Evaluate named arguments, add results to environment
  env <- bind_args(dots[named], parent_env)

  # Concatenate unnamed arguments together
  unnamed_args <- lapply(
    which(!named),
    function(x) {
      # Any evaluation to `NULL` is replaced with `.null`:
      # - If `.null == character()` then if any output's length is 0 the
      # whole output should be forced to be `character(0)`.
      # - If `.null == NULL` then it is allowed and any such argument will be
      # silently dropped.
      # - In other cases output is treated as it was evaluated to `.null`.
      eval(call("force", as.symbol(paste0("..", x)))) %||% .null
    }
  )
  unnamed_args <- drop_null(unnamed_args)

  if (length(unnamed_args) == 0) {
    # This is equivalent to `paste0(NULL)`
    return(as_glue(character(0)))
  }

  lengths <- lengths(unnamed_args)
  if (any(lengths == 0)) {
    return(as_glue(character(0)))
  }
  if (any(lengths != 1)) {
    stop("All unnamed arguments must be length 1", call. = FALSE)
  }
  if (any(is.na(unnamed_args))) {
    if (is.null(.na)) {
      return(as_glue(NA_character_))
    } else {
      unnamed_args[is.na(unnamed_args)] <- .na
    }
  }

  unnamed_args <- paste0(unnamed_args, collapse = .sep)
  if (isTRUE(.trim)) {
    unnamed_args <- trim(unnamed_args)
  }

  f <- function(expr){
    eval_func <- .transformer(expr, env) %||% .null

    # crayon functions *can* be used, so we use tryCatch()
    # to give as.character() a chance to work
    tryCatch(
      # Output can be `NULL` only if `.null` is `NULL`. Then it should be
      # returned as is, because `as.character(NULL)` is `character()`.
      if (is.null(eval_func)) NULL else as.character(eval_func),
      error = function(e) {
        # if eval_func is a function, provide informative error-message
        if (is.function(eval_func)) {
          message <- paste0(
            "glue cannot interpolate functions into strings.\n",
            "* object '",
            expr,
            "' is a function."
          )

          stop(message, call. = FALSE)
        }

        # default stop
        stop(e)
      }
    )
  }

  # Parse any glue strings
  res <- .Call(glue_, unnamed_args, f, .open, .close, .comment, .literal)

  res <- drop_null(res)

  if (any(lengths(res) == 0)) {
    return(as_glue(character(0)))
  }

  if (!is.null(.na)) {
    res[] <- lapply(res, function(x) replace(x, is.na(x), .na))
  } else {
    na_rows <- na_rows(res)
  }

  res <- do.call(paste0, recycle_columns(res))

  if (is.null(.na)) {
    res <- replace(res, na_rows, NA)
  }

  as_glue(res)
}

#' @export
#' @rdname glue
glue <- function(..., .sep = "", .envir = parent.frame(), .open = "{", .close = "}", .na = "NA", .null = character(), .comment = "#", .literal = FALSE, .transformer = identity_transformer, .trim = TRUE) {
  glue_data(.x = NULL, ..., .sep = .sep, .envir = .envir, .open = .open, .close = .close, .na = .na, .null = .null, .comment = .comment, .literal = .literal, .transformer = .transformer, .trim = .trim)
}

#' Collapse a character vector
#'
#' `glue_collapse()` collapses a character vector of any length into a length 1 vector.
#' `glue_sql_collapse()` does the same but returns a `[DBI::SQL()]`
#' object rather than a glue object.
#'
#' @param x The character vector to collapse.
#' @param width The maximum string width before truncating with `...`.
#' @param last String used to separate the last two items if `x` has at least
#' 2 items.
#' @inheritParams base::paste
#' @examples
#' glue_collapse(glue("{1:10}"))
#'
#' # Wide values can be truncated
#' glue_collapse(glue("{1:10}"), width = 5)
#'
#' glue_collapse(1:4, ", ", last = " and ")
#' #> 1, 2, 3 and 4
#' @export
glue_collapse <- function(x, sep = "", width = Inf, last = "") {
  if (length(x) == 0) {
    return(as_glue(character()))
  }
  if (any(is.na(x))) {
    return(as_glue(NA_character_))
  }

  if (nzchar(last) && length(x) > 1) {
    res <- glue_collapse(x[seq(1, length(x) - 1)], sep = sep, width = Inf)
    return(glue_collapse(paste0(res, last, x[length(x)]), width = width))
  }
  x <- paste0(x, collapse = sep)
  if (width < Inf) {
    x_width <- nchar(x, "width")
    too_wide <- x_width > width
    if (too_wide) {
      x <- paste0(substr(x, 1, width - 3), "...")
    }
  }
  as_glue(x)
}

#' Trim a character vector
#'
#' This trims a character vector according to the trimming rules used by glue.
#' These follow similar rules to [Python Docstrings](https://www.python.org/dev/peps/pep-0257/),
#' with the following features.
#' - Leading and trailing whitespace from the first and last lines is removed.
#' - A uniform amount of indentation is stripped from the second line on, equal
#' to the minimum indentation of all non-blank lines after the first.
#' - Lines can be continued across newlines by using `\\`.
#' @param x A character vector to trim.
#' @export
#' @examples
#' glue("
#'     A formatted string
#'     Can have multiple lines
#'       with additional indention preserved
#'     ")
#'
#' glue("
#'   \ntrailing or leading newlines can be added explicitly\n
#'   ")
#'
#' glue("
#'     A formatted string \\
#'     can also be on a \\
#'     single line
#'     ")

#' @useDynLib glue trim_
trim <- function(x) {
  has_newline <- function(x) any(grepl("\\n", x))
  if (length(x) == 0 || !has_newline(x)) {
    return(x)
  }
  .Call(trim_, x)
}

#' @export
print.glue <- function(x, ..., sep = "\n") {
  x[is.na(x)] <- style_na(x[is.na(x)])

  if (length(x) > 0) {
    cat(x, ..., sep = sep)
  }

  invisible(x)
}

#' Coerce object to glue
#' @param x object to be coerced.
#' @param ... further arguments passed to methods.
#' @export
as_glue <- function(x, ...) {
  UseMethod("as_glue")
}

#' @export
as_glue.default <- function(x, ...) {
  as_glue(as.character(x))
}

#' @export
as_glue.glue <- function(x, ...) {
  x
}

#' @export
as_glue.character <- function(x, ...) {
  class(x) <- c("glue", "character")
  enc2utf8(x)
}

#' @export
as.character.glue <- function(x, ...) {
  unclass(x)
}

#' @export
`[.glue` <- function(x, i, ...) {
  as_glue(NextMethod())
}

#' @export
`[[.glue` <- function(x, i, ...) {
  as_glue(NextMethod())
}

#' @export
`+.glue` <- function(e1, e2) {
  glue(e1, e2, .envir = parent.frame())
}

#' @importFrom methods setOldClass
setOldClass(c("glue", "character"))


#' Deprecated Functions
#'
#' These functions are Deprecated in this release of glue, they will be removed
#' in a future version.
#' @name glue-deprecated
#' @keywords internal
NULL
