assert_that <- function(..., env = parent.frame(), msg = NULL) {
  asserts <- eval(substitute(alist(...)))

  for (assertion in asserts) {
    res <- tryCatch(
      {
        eval(assertion, env)
      },
      assertError = function(e) {
        structure(FALSE, msg = e$message)
      }
    )
    check_result(res)
    if (res) next

    if (is.null(msg)) {
      msg <- get_message(res, assertion, env)
      evalenv <- attr(res, "env") %||% env
    } else {
      evalenv <- env
    }
    throw(
      assert_error(
        assertion,
        res,
        msg,
        call. = sys.call(-1),
        .envir = evalenv,
      ),
      frame = env
    )
  }

  invisible(TRUE)
}

assert_error <- function(
  assertion,
  result,
  msg,
  .data = NULL,
  .class = NULL,
  .envir = parent.frame(),
  call. = TRUE
) {
  myenv <- new.env(parent = .envir)
  myenv$.arg <- if (length(assertion) >= 2) deparse(assertion[[2]])
  myenv$.arg2 <- if (length(assertion) >= 3) deparse(assertion[[3]])
  .hide_from_trace <- TRUE
  cnd <- new_error(
    call. = call.,
    cli::format_error(
      .envir = myenv,
      msg
    )
  )

  if (length(.data)) cnd[names(.data)] <- .data
  if (length(class)) class(cnd) <- unique(c(.class, "assertError", class(cnd)))

  cnd
}
check_result <- function(x) {
  if (!is.logical(x)) {
    throw(pkg_error(
      "{.fun assert_that}: assertion must return a logical value.",
      "i" = "it returned {.type {x}} instead."
    ))
  }

  if (length(x) != 1) {
    throw(pkg_error(
      "{.fun assert_that}: assertion must return a scalar.",
      "i" = "it returned a vector of length {length(x)}."
    ))
  }

  if (any(is.na(x))) {
    throw(pkg_error(
      "{.fun assert_that}: assertion must not return {.code NA}."
    ))
  }

  TRUE
}

get_message <- function(res, call, env = parent.frame()) {
  if (has_attr(res, "msg")) {
    return(attr(res, "msg"))
  }

  f <- eval(call[[1]], env)
  if (is.call(call) && !is.primitive(f)) call <- match.call(f, call)
  fname <- deparse(call[[1]])

  base_fs[[fname]] %||% fail_default(call, env)
}

# The default failure message works in the same way as stopifnot, so you can
# continue to use any function that returns a logical value: you just won't
# get a friendly error message.
# The code below says you get the first 60 characters plus a ...
fail_default <- function(call, env) {
  call_string <- deparse(call, width.cutoff = 60L)
  if (length(call_string) > 1L) {
    call_string <- paste0(call_string[1L], "...")
  }

  paste0(call_string, " is not true")
}

has_attr <- function(x, which) {
  if (!is.null(attr(x, which, exact = TRUE))) return(TRUE)
  structure(
    FALSE,
    msg = "{.arg {(.arg)}} must have attribute {.code {which}}.",
    env = environment()
  )
}
"%has_attr%" <- has_attr

base_fs <- new.env(parent = emptyenv())

# nocov start

logical_is_not <- function(failed) {
  paste0("{.arg {(.arg)}} must ", failed, " {.arg {(.arg2)}}.")
}

base_fs$"==" <- logical_is_not("equal")
base_fs$"<" <- logical_is_not("be less than")
base_fs$">" <- logical_is_not("be greater than")
base_fs$">=" <- logical_is_not("be greater than or equal to")
base_fs$"<=" <- logical_is_not("be less than or equal to")
base_fs$"!=" <- logical_is_not("not be equal to")

is_not <- function(thing) {
  paste0("{.arg {(.arg)}} must be ", thing, ".")
}

# nocov end

# Vectors
base_fs$is.atomic <- is_not("an atomic vector")
base_fs$is.character <- is_not("a character vector")
base_fs$is.complex <- is_not("a complex vector")
base_fs$is.double <- is_not("a numeric vector")
base_fs$is.integer <- is_not("an integer vector")
base_fs$is.numeric <- is_not("a numeric or integer vector")
base_fs$is.raw <- is_not("a raw vector")
base_fs$is.vector <- is_not("an atomic vector without attributes")

# Factors
base_fs$is.factor <- is_not("a factor")
base_fs$is.ordered <- is_not("an ordered factor")

# More complicated data structures
base_fs$is.array <- is_not("an array")
base_fs$is.data.frame <- is_not("a data frame")
base_fs$is.list <- is_not("a list")
base_fs$is.matrix <- is_not("a matrix")
base_fs$is.null <- is_not("{.code NULL}")

# Functions and environments
base_fs$is.environment <- is_not("an environment")
base_fs$is.function <- is_not("a function")
base_fs$is.primitive <- is_not("a primitive function")

# Computing on the language
base_fs$is.call <- is_not("a quoted call")
base_fs$is.expression <- is_not("an expression object")
base_fs$is.name <- is_not("a name")
base_fs$is.pairlist <- is_not("a pairlist")
base_fs$is.recursive <- is_not("a recursive object")
base_fs$is.symbol <- is_not("a name")

# Catch all
base_fs$"&&" <-
  "{.arg {(.arg)}} and {.arg {(.arg2)}} must both be true."

base_fs$"||" <-
  "One of {.arg {(.arg)}} and {.arg {(.arg2)}} must be true."

base_fs$any <-
  "At least one of {.arg {(.arg)}} must be true."

base_fs$all <-
  "All of {.arg {(.arg)}} must be true."

base_fs$file.exists <-
  "Path {.arg {(.arg)}} must exist."

base_fs$identical <-
  "{.arg {(.arg)}} must be identical to {.arg {(.arg2)}}."
