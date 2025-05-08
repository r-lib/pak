assert_that <- function(..., env = parent.frame(), msg = NULL) {
  res <- see_if(..., env = env, msg = msg)
  if (res) return(TRUE)

  throw(new_assert_error(attr(res, "msg")))
}

new_assert_error <- function(message, call = NULL) {
  cond <- new_error(message, call. = call)
  class(cond) <- c("assert_error", class(cond))
  cond
}

see_if <- function(..., env = parent.frame(), msg = NULL) {
  asserts <- eval(substitute(alist(...)))

  for (assertion in asserts) {
    res <- tryCatch(
      {
        eval(assertion, env)
      },
      error = function(e) {
        structure(FALSE, msg = e$message)
      }
    )
    check_result(res)

    # Failed, so figure out message to produce
    if (!res) {
      if (is.null(msg)) msg <- get_message(res, assertion, env)
      return(structure(FALSE, msg = msg))
    }
  }

  res
}

check_result <- function(x) {
  if (!is.logical(x))
    throw(new_assert_error(
      "assert_that: assertion must return a logical value"
    ))
  if (any(is.na(x)))
    throw(new_assert_error("assert_that: missing values present in assertion"))
  if (length(x) != 1) {
    throw(new_assert_error("assert_that: length of assertion is not 1"))
  }

  TRUE
}

get_message <- function(res, call, env = parent.frame()) {
  stopifnot(is.call(call), length(call) >= 1)

  if (has_attr(res, "msg")) {
    return(attr(res, "msg"))
  }

  f <- eval(call[[1]], env)
  if (!is.primitive(f)) call <- match.call(f, call)
  fname <- deparse(call[[1]])

  fail <- on_failure(f) %||% base_fs[[fname]] %||% fail_default
  fail(call, env)
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

  paste0(call_string, " is not TRUE")
}

on_failure <- function(x) attr(x, "fail")

"on_failure<-" <- function(x, value) {
  stopifnot(is.function(x), identical(names(formals(value)), c("call", "env")))
  attr(x, "fail") <- value
  x
}

has_attr <- function(x, which) !is.null(attr(x, which, exact = TRUE))
on_failure(has_attr) <- function(call, env) {
  paste0(deparse(call$x), " does not have attribute ", eval(call$which, env))
}
"%has_attr%" <- has_attr

base_fs <- new.env(parent = emptyenv())
