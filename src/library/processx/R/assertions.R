
is_string <- function(x) {
  is.character(x) &&
  length(x) == 1 &&
  !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string (length 1 character)")
}

is_string_or_null <- function(x) {
  is.null(x) || is_string(x)
}

on_failure(is_string_or_null) <- function(call, env) {
  paste0(deparse(call$x), " must be a string (length 1 character) or NULL")
}

is_flag <- function(x) {
  is.logical(x) &&
  length(x) == 1 &&
  !is.na(x)
}

on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " is not a flag (length 1 logical)")
}

is_integerish_scalar <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && round(x) == x
}

on_failure(is_integerish_scalar) <- function(call, env) {
  paste0(deparse(call$x), " is not a length 1 integer")
}

is_pid <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && round(x) == x
}

on_failure(is_pid) <- function(call, env) {
  paste0(deparse(call$x), " is not a process id (length 1 integer)")
}

is_flag_or_string <- function(x) {
  is_string(x) || is_flag(x)
}

on_failure(is_flag_or_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a flag or a string")
}

is_existing_file <- function(x) {
  is_string(x) && file.exists(x)
}

on_failure(is_existing_file) <- function(call, env) {
  paste0("File ", deparse(call$x), " does not exist")
}

is_time_interval <- function(x) {
  (inherits(x, "difftime") && length(x) == 1) ||
    (is.numeric(x) && length(x) == 1 && !is.na(x))
}

on_failure(is_time_interval) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid time interval")
}

is_list_of_pollables <- function(x) {
  if (!is.list(x)) return(FALSE)
  proc <- vapply(x, inherits, FUN.VALUE = logical(1), "process")
  conn <- vapply(x, is_connection, logical(1))
  curl <- vapply(x, inherits, FUN.VALUE = logical(1), "processx_curl_fds")
  all(proc | conn | curl)
}

on_failure(is_list_of_pollables) <- function(call, env) {
  paste0(deparse(call$x), " is not a list of pollable objects")
}

is_named_character <- function(x) {
  is.character(x) && !any(is.na(x)) && is_named(x)
}

on_failure(is_named_character) <- function(call, env) {
  paste0(deparse(call$x), " must be a named character vector")
}

is_named <- function(x) {
  length(names(x)) == length(x) && all(names(x) != "")
}

on_failure(is_named) <- function(call, env) {
  paste0(deparse(call$x), " must have non-empty names")
}

is_connection <- function(x) {
  inherits(x, "processx_connection")
}

on_failure(is_connection) <- function(call, env) {
  paste0(deparse(call$x), " must be a processx connection")
}

is_connection_list <- function(x) {
  all(vapply(x, is_connection, logical(1)))
}

on_failure(is_connection_list) <- function(call, env) {
  paste0(deparse(call$x), " must be a list of processx connections")
}

is_env_vector <- function(x) {
  if (is_named_character(x)) return(TRUE)
  if (!is.character(x) || anyNA(x)) return(FALSE)
  if (is.null(names(x))) {
    all(x == "current")
  } else {
    all(x[names(x) == ""] == "current")
  }
}

on_failure(is_env_vector) <- function(call, env) {
  paste0(
    "all elements, except \"current\" must be named in ",
    deparse(call$x)
  )
}

is_std_conn <- function(x) {
  is.null(x) || is_string(x) || is_connection(x)
}

on_failure(is_std_conn) <- function(call, env) {
  paste0(
    deparse(call$x),
    " must be `NULL`, a string or a processx connection"
  )
}
