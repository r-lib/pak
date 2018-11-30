
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
  env <- Sys.getenv("R_PKG_SHOW_PROGRESS", "")
  if (env != "") {
    tolower(env) == "true"
  } else {
    opt <- getOption("pkg.show_progress")
    if (!is.null(opt)) {
      return(isTRUE(opt))
    } else {
      interactive()
    }
  }
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

get_current_r_version <- function() {
  as.character(getRversion())
}

get_minor_r_version <- function(x = getRversion()) {
  x <- package_version(x)
  vapply(unclass(x), function(x) paste(x[1:2], collapse = "."), character(1))
}

get_os <- function() {
  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    "unknown"
  }
}

user_cache_dir <- function(appname, version) {
  switch(
    get_os(),
    win = file_path(win_path_local(), appname, version, "Cache"),
    mac = file_path("~/Library/Caches", appname, version),
    unix = file_path(Sys.getenv("XDG_CACHE_HOME", "~/.cache"), appname,
                     version),
    unknown = file_path(tempdir(), "r-pkg-cache", appname, version)
  )
}

file_path <- function(...) {
  normalizePath(do.call("file.path", as.list(c(...))), mustWork = FALSE)
}

win_path_local <- function() {
  if (nzchar(lapp <- Sys.getenv("LOCALAPPDATA", ""))) {
    lapp

  } else if (nzchar(usrprof <- Sys.getenv("USERPROFILE", ""))) {
    file.path(usrprof, "AppData", "Local")

  } else {
    file.path(tempdir(), "r-pkg-cache")
  }
}
