has_names <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !(is.na(nms) | nms == "")
  }
}

bind_args <- function(args, parent) {
  assign_env <- parent
  nms <- names(args)
  for (i in seq_along(args)) {
    eval_env <- assign_env
    assign_env <- new.env(parent = eval_env)
    delayed_assign(nms[[i]], args[[i]], eval.env = eval_env, assign.env = assign_env)
  }
  assign_env
}

# From tibble::recycle_columns
recycle_columns <- function (x) {
    if (length(x) == 0) {
        return(x)
    }
    lengths <- vapply(x, NROW, integer(1))
    if (any(lengths) == 0) {
      return(character())
    }

    max <- max(lengths)
    bad_len <- lengths != 1L & lengths != max
    if (any(bad_len)) {
      stop(call. = FALSE,
        ngettext(max,
          "Variables must be length 1",
          paste0("Variables must be length 1 or ", max), domain = NA))
    }
    short <- lengths == 1
    if (max != 1L && any(short)) {
        x[short] <- lapply(x[short], rep, max)
    }
    x
}

# From https://github.com/hadley/colformat/blob/0a35999e7d77b9b3a47b4a04662d1c2625f929d3/R/styles.R#L19-L25
colour_na <- function() {
  grDevices::rgb(5, 5, 2, maxColorValue = 5)
}

style_na <- function(x) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    crayon::style(x, bg = colour_na())
  } else {
    x # nocov
  }
}

lengths <- function(x) {
  vapply(x, length, integer(1L))
}

na_rows <- function(res) {
  Reduce(`|`, lapply(res, is.na))
}

"%||%" <- function(x, y) if (is.null(x)) y else x # nocov

drop_null <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

# A version of delayedAssign which does _not_ use substitute
delayed_assign <- function(x, value, eval.env = parent.frame(1), assign.env = parent.frame(1)) {
  do.call(delayedAssign, list(x, value, eval.env, assign.env))
}

## @export
compare.glue <- function(x, y, ...) {
  if (identical(class(y), "character")) {
    class(x) <- NULL
  }
  NextMethod("compare")
}

## @export
compare_proxy.glue <- function(x, path = "x") {
  class(x) <- NULL
  NextMethod("compare_proxy")
}
