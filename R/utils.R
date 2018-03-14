
`%||%` <- function(l, r) if (is.null(l)) r else l

# Adapted from withr:::merge_new
merge_new <- function(old, new, action = c("replace", "prepend", "append")) {
  action <- match.arg(action, c("replace", "prepend", "append"))

  switch(action,
    prepend = c(new, old),
    append = c(old, new),
    replace = new
  )
}

names2 <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep("", length(x))
  } else {
    nms[is.na(nms)] <- ""
    nms
  }
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = character(1), ...)
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = logical(1), ...)
}

viapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = integer(1), ...)
}

vdapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = double(1), ...)
}

is_verbose <- function() {
  getOption("pkg.show_progress") %||% interactive()
}

#' @importFrom glue collapse backtick

format_items <- function (x) {
  paste0(collapse(backtick(x), sep = ", ", last = " and "))
}
