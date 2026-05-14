#' Utility functions for ts language implementations (internal)
#'
#' These functions are for packages implementing new parsers based on the ts
#' package. It is very unlikely that you will need to call these functions
#' directly.
#'
#' @param ... Arguments collapsed and interpolated into a condition message.
#' @param class A character vector of classes for the condition.
#' @param call Environment of the call to associate with the condition,
#'   defaults to the caller's environment.
#' @param .envir The environment in which to evaluate the `...` expressions,
#'   defaults to the parent frame.
#' @return `ts_cnd()` returns an error condition object.
#'
#' @details `ts_cnd()` creates a condition object. It interpolates its
#' arguments into a single message.
#'
#' @keywords internal
#' @rdname internal
#' @export

ts_cnd <- function(
  ...,
  class = NULL,
  call = ts_caller_env(),
  .envir = parent.frame()
) {
  call <- frame_get(call, sys.call)
  structure(
    list(message = glue(..., .envir = .envir), call = call),
    class = c(class, "error", "condition")
  )
}

#' @rdname internal
#' @param arg Argument name.
#' @details `ts_caller_arg()` captures the expression used as an argument
#'   to a function, for use in error messages.
#' @return `ts_caller_arg()` returns the captured expression as a
#'   ts_caller_arg object.
#' @export

ts_caller_arg <- function(arg) {
  arg <- substitute(arg)
  expr <- do.call(substitute, list(arg), envir = ts_caller_env())
  structure(list(expr), class = "ts_caller_arg")
}

#' @rdname internal
#' @param x Object to use as a caller argument.
#' @details `as_ts_caller_arg()` converts an object into a caller argument
#'   object. This is useful when referring to parts of the caller argument
#'   in downstream error messages.
#' @return `as_ts_caller_arg()` returns a ts_caller_arg object.
#' @export

as_ts_caller_arg <- function(x) {
  structure(list(x), class = "ts_caller_arg")
}

#' @rdname internal
#' @param x A ts_caller_arg object.
#' @details `as.character.ts_caller_arg()` formats a caller argument
#'   object as a short string for use in error messages. Multi-line
#'   expressions are truncated after the first line.
#' @return `as.character.ts_caller_arg()` returns a short string
#'   representation of the caller argument, a character scalar.
#' @export

as.character.ts_caller_arg <- function(x, ...) {
  lbl <- paste(format(x[[1]]), collapse = "\n")
  gsub("\n.*$", "...", lbl)
}

# older R does not have a format method for names
format.name <- function(x, ...) {
  deparse(x, backtick = FALSE)
}

#' @rdname internal
#' @param n Number of frames to go up to find the caller environment.
#' @details `ts_caller_env()` returns the environment of the caller function,
#'   `n` levels up the call stack. This is useful for associating error
#'   conditions with the correct call.
#' @return `ts_caller_env()` returns an environment, or `NULL` is called
#'   from the global environment.
#' @export

ts_caller_env <- function(n = 1) {
  parent.frame(n + 1)
}

frame_get <- function(frame, accessor) {
  if (identical(frame, .GlobalEnv)) {
    return(NULL)
  }
  frames <- evalq(sys.frames(), frame)
  for (i in seq_along(frames)) {
    if (identical(frames[[i]], frame)) {
      return(accessor(i))
    }
  }
  NULL
}

#' @rdname internal
#' @param arg Argument to check.
#' @param frame Frame number to inspect, defaults to the caller's frame.
#' @details `ts_check_named_arg()` checks whether an argument was supplied
#'   with a name. If not, it raises an error.
#' @return `ts_check_named_arg()` returns `TRUE` invisibly if the argument
#'   was named, otherwise it raises an error.
#' @export

ts_check_named_arg <- function(arg, frame = -1) {
  arg <- as.character(substitute(arg))
  if (!arg %in% names(sys.call(frame)[-1])) {
    stop(ts_cnd(
      paste0("The `", arg, "` argument must be fully named."),
      call = ts_caller_env()
    ))
  }
  invisible(TRUE)
}

#' @rdname internal
#' @param tree A `ts_tree` object as returned by [ts_tree_new()].
#' @param text Raw vector, the original text used to parse the tree.
#' @param call Environment of the call to associate with the error
#'   condition, defaults to the caller's environment.
#' @details `ts_parse_error_cnd()` creates a parse error condition
#'   associated with a `ts_tree` object and the original text.
#'   The error message includes information about the location of
#'   parse errors in the text. It also has `format()` and `print()`
#'   methods to display the error together with the relevant lines
#'   of the original text.
#' @return `ts_parse_error_cnd()` returns a ts_parse_error condition
#' @export

ts_parse_error_cnd <- function(tree, text, call = ts_caller_env()) {
  call <- frame_get(call, sys.call)
  cnd <- structure(
    list(
      call = call,
      tree = tree,
      text = strsplit(rawToChar(text), "\r?\n")[[1]]
    ),
    class = c("ts_parse_error", "error", "condition")
  )
  cnd$message <- paste0(
    format_ts_parse_error_(cnd),
    collapse = "\n"
  )
  cnd
}

# TODO: taking the first row of the error is not always ideal, because
# ERROR nodes may contain a leading comma before the element that has the
# actual error, and if there is a line break after the comma, then we
# mark the comma as the place of the error, instead of the real error in
# next line.
#
# One workaround is to mark all lines of the ERROR node.
# Another is to ignore the first child of the ERROR node if it is a comma,
# but I am unsure if that's always correct. Sometimes that error may be
# at the comma. Plus maybe we'd need to ignore more than just a first comma.
# So maybe we should mark all rows of the ERROR node.

format_ts_parse_error_ <- function(x, n = 2, ...) {
  file <- attr(x$tree, "file") %||% "<text>"
  nodes <- utils::head(which(x$tree$type == "ERROR" | x$tree$is_missing), n)
  # if two errors start at the same position only show the first
  nodes <- nodes[!duplicated(x$tree$start_byte[nodes])]

  rows <- x$tree$start_row[nodes] + 1L
  cols <- x$tree$start_column[nodes] + 1L
  erows <- x$tree$end_row[nodes] + 1L

  ecols <- ifelse(
    rows == erows,
    x$tree$end_column[nodes] + 1L,
    nchar(x$text[rows])
  )

  lang <- toupper(get_tree_lang(x$tree))
  unlist(mapply(
    format_ts_parse_error_1,
    lang,
    file,
    rows,
    cols,
    ecols,
    MoreArgs = list(text = x$text)
  ))
}

format_ts_parse_error_1 <- function(lang, text, file, row, col, ecol) {
  head <- sprintf("%s parse error `%s`:%d:%d", toupper(lang), file, row, col)

  nchar <- function(x) {
    cli::ansi_nchar(x, type = "width")
  }

  rows <- c(if (row > 1L) row - 1L, row, if (row < length(text)) row + 1L)
  linum <- structure(cli::col_grey(sprintf("%d| ", rows)), names = rows)
  nlinum <- max(nchar(linum))

  # assume a minimum width of 40, otherwise it is hard to format
  width <- max(cli::console_width(), 40L) - nlinum

  prefix <- substr(text[row], 1L, col - 1L)
  error <- substr(text[row], col, ecol)
  suffix <- substr(text[row], ecol + 1L, nchar(text[row]))

  npre <- nchar(prefix)
  nerr <- nchar(error)
  nsuf <- nchar(suffix)
  truncated <- FALSE
  if (npre + nerr + nsuf > width) {
    truncated <- TRUE
    dots <- cli::col_grey("...")
    # need to truncate, first suffix
    if (nsuf > 10) {
      suffix <- paste0(dots, substr(suffix, nchar(suffix) - 7, nchar(suffix)))
      nsuf <- nchar(suffix)
    }
    if (npre + nerr + nsuf > width) {
      # need to truncate, then prefix
      if (npre > 10) {
        prefix <- paste0(substr(prefix, 1, 7), dots)
        npre <- nchar(prefix)
      }
      if (npre + nerr + nsuf > width) {
        # need to truncate error
        err_width <- width - npre - nsuf
        error <- paste0(
          substr(error, 1, err_width %/% 2),
          dots,
          substr(error, nchar(error) - err_width %/% 2 + 4, nchar(error))
        )
      }
    }
  }

  # if we truncated the line, then do not show the context lines
  ctx_before <- ctx_after <- NULL
  if (truncated) {
    linum <- linum[as.character(row)]
  } else {
    ctx_before <- if (row > 1) {
      paste0(linum[as.character(row - 1)], text[row - 1])
    }
    ctx_after <- if (row < length(text)) {
      paste0(linum[as.character(row + 1)], text[row + 1])
    }
  }

  mark <- paste0(
    strrep(" ", nlinum + nchar(prefix)),
    cli::col_red(strrep("^", max(nchar(error), 1)))
  )

  c(
    head,
    ctx_before,
    paste0(linum[as.character(row)], prefix, error, suffix),
    mark,
    ctx_after
  )
}

#' @export

format.ts_parse_error <- function(x, ...) {
  c("<ts_parse_error>", format_ts_parse_error_(x, ...))
}

#' @export

print.ts_parse_error <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}
