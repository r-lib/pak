
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
  paste0(glue::glue_collapse(glue::backtick(x), sep = ", ", last = " and "))
}

str_trim <- function (x) {
  sub("^\\s+", "", sub("\\s+$", "", x))
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

user_cache_dir <- function(appname) {
  if (nzchar(cache <- Sys.getenv("R_PKG_CACHE_DIR", ""))) return(cache)
  switch(
    get_os(),
    win = file_path(win_path_local(), appname, "Cache"),
    mac = file_path("~/Library/Caches", appname),
    unix = file_path(Sys.getenv("XDG_CACHE_HOME", "~/.cache"), appname),
    unknown = file_path(tempdir(), "r-pkg-cache", appname)
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

cat0 <- function(..., sep = "") {
  cat(..., sep = sep)
}

get_num_workers <- function() {
  n <- tryCatch(
    suppressWarnings(as.integer(getOption("Ncpus", NA_integer_))),
    error = function(e) NA_integer_)

  if (length(n) != 1 || is.na(n)) {
    n <- tryCatch(
      ps::ps_cpu_count(logical = TRUE),
      error = function(e) NA_integer_)
  }

  if (is.na(n)) n <- 1L
  
  n    
}

to_package_name <- function(x) {
  x <- gsub("[^a-zA-Z0-9\\.]", "", x)
  if (nchar(x) < 2) {
    "unknown.package"
  } else if (!grepl("^[a-zA-Z]", x)) {
    paste0("X", x)
  } else {
    x
  }
}

strrep <- function(x, times) {
  x = as.character(x)
  if (length(x) == 0L)
    return(x)
  unlist(.mapply(function(x, times) {
    if (is.na(x) || is.na(times))
      return(NA_character_)
    if (times <= 0L)
      return("")
    paste0(replicate(times, x), collapse = "")
  }, list(x = x, times = times), MoreArgs = list()), use.names = FALSE)
}

testthat_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

norm_path <- function(x) {
  normalizePath(x, winslash = "/")
}
