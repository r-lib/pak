
#' Create an error object
#'
#' There are two kinds of errors, both have class `callr_error`:
#' 1. the first one is thrown after a timeout: `callr_timeout_error`.
#' 2. the second one is thrown after an R error (in the other session):
#'    `callr_status_error`.
#'
#' @param out The object returned by [run()].
#' @param msg An extra message to add to the error message.
#' @keywords internal

new_callr_crash_error <- function(out, msg = NULL) {
  error_msg <- paste0(
    if (out$timeout) "callr timed out" else "callr subprocess failed",
    if (!is.null(msg)) paste0(": ", msg) else if (!out$timeout) ":"
  )

  cond <- new_error(paste(error_msg))

  class(cond) <- c(
    if (out$timeout) "callr_timeout_error" else "callr_status_error",
    "callr_error",
    class(cond))

  cond$status <- out$status
  cond$stdout <- out$stdout
  cond$stderr <- out$stderr

  cond
}

callr_remote_error <- function(remerr, out) {
  if (inherits(remerr[[3]], "interrupt")) {
    err <- new_error("interrupt sugnal in callr subprocess", call. = FALSE)
    class(err) <- c("callr_timeout_error", "callr_error", class(err))
    err$message <- "callr subprocess interrupted"
  } else {
    err <- new_error("in callr subprocess.", call. = FALSE)
    class(err) <- c("callr_status_error", "callr_error", class(err))
  }

  err$status <- out$status
  err$stdout <- out$stdout
  err$stderr <- out$stderr

  err$parent_trace <- remerr[[2]]$trace
  err
}

callr_remote_error_with_stack <- function(remerr, out) {
  err <- new_error("in callr subprocess.", call. = FALSE)
  class(err) <- c("callr_status_error", "callr_error", class(err))

  err$status <- out$status
  err$stdout <- out$stdout
  err$stderr <- out$stderr

  err$stack <- clean_stack(remerr[[3]])
  err
}

#' @export

format.callr_status_error <- function(x, trace = FALSE, class = FALSE,
                                      advice = !trace, ...) {
  class(x) <- setdiff(class(x), "callr_status_error")

  lines <- NextMethod(
    object = x,
    trace = FALSE,
    class = class,
    advice = FALSE,
    ...
  )

  info <- if (err$.internal$has_cli()) {
    cli::col_cyan(cli::symbol$info)
  } else {
    "i"                                                             # nocov
  }

  if (!is.null(x$stack)) {
    lines <- c(
      lines,
      paste0(info, " With remote `$stack`, use `utils::debugger()` to debug it.")
    )
  }

  notempty <- function(x) !is.null(x) && sum(nchar(x)) > 0
  hasout <- notempty(x$stdout)
  haserr <- notempty(x$stderr)
  if (hasout || haserr) {
    if (err$.internal$is_interactive()) {
      lines <- c(
        lines,
        if (hasout && haserr) {
          paste0(info, " See `$stdout` and `$stderr` for standard output and error.")
        } else if (hasout) {
          paste0(info, " See `$stdout` for standard output.")
        } else {
          paste0(info, " See `$stderr` for standard error.")
        }
      )
    } else {
      lines <- c(
        lines,
        if (hasout) {
          c(
            "---",
            "Standard output:",
            trimws(x$stdout)
          )
        },
        if (haserr) {
          c("---",
            "Standard error:",
            trimws(x$stderr)
          )
        }
      )
    }
  }

  lines <- c(
    lines,
    if (advice) err$format$advice(),
    if (trace && !is.null(x$trace)) {
      c(
        "---",
        "Backtrace:",
        err$format$trace(x$trace)
      )
    }
  )

  cond <- x
  while (trace && !is.null(cond$parent_trace)) {
    lines <- c(lines, c("---", "Subprocess backtrace:", format(cond$parent_trace)))
    cond <- cond$parent
  }

  lines
}

#' @export

print.callr_status_error <- function(x, trace = TRUE, class = TRUE,
                                     advice = !trace, ...) {
  writeLines(format(x, trace = trace, class = class, advice = advice, ...))
}
