
`%||%` <- function(l, r) if (is.null(l)) r else l

isFALSE <- function(x) identical(x, FALSE)

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

format_items <- function (x) {
  paste0(glue::collapse(glue::backtick(x), sep = ", ", last = " and "))
}

str_trim <- function (x) {
  sub("^\\s+", "", sub("\\s+$", "", x))
}

## This only works properly for packages without S3 and S4!

unload_package <- function(pkg) {
  unloadNamespace(pkg)
  libs <- .dynLibs()
  pkg_dll <- vcapply(libs, "[[", "name") == pkg
  if (any(pkg_dll)) {
    for (i in which(pkg_dll)) dyn.unload(libs[[i]][["path"]])
    .dynLibs(libs[!pkg_dll])
  }
  invisible()
}

with_package <- function(pkg, expr) {
  if (! pkg %in% loadedNamespaces()) on.exit(unload_package(pkg), add = TRUE)
    expr
}

list_files <- function(path) {
  if (!file.exists(path)) return(character())
  fs <- dir(path, full.names = TRUE)
  basename(fs[! is_dir(fs)])
}

file_mtime <- function(...) {
  file.info(..., extra_cols = FALSE)$mtime
}

is_dir <- function(...) {
  file.info(..., extra_cols = FALSE)$isdir
}
