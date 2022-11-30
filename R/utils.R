
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
  if (nzchar(cache <- Sys.getenv("R_PKG_CACHE_DIR", ""))) {
    return(cache)
  }
  if (nzchar(cache <- Sys.getenv("R_USER_CACHE_DIR", ""))) {
    return(file.path(cache, "R", appname))
  }
  switch(
    get_os(),
    win = file_path(win_path_local(), "R", "Cache", appname),
    mac = file_path("~/Library/Caches", "org.R-project.R", "R", appname),
    unix = file_path(Sys.getenv("XDG_CACHE_HOME", "~/.cache"), "R", appname),
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

catln <- function(..., sep = "") {
  cat(..., "\n", sep = "")
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

drop_nulls <- function(x) {
  is_null <- vlapply(x, is.null)
  x[!is_null]
}

mkdirp <- function(dir, msg = NULL) {
  s <- vlapply(dir, dir.create, recursive = TRUE, showWarnings = FALSE)
  if (any(s) && !is.null(msg) && is_verbose()) {
    cli::cli_alert_info("{msg}: {.path {format_items(dir[s])}}")
  }
  invisible(s)
}

fix_macos_path_in_rstudio <- function() {
  ## Only in RStudio
  if (Sys.getenv("RSTUDIO") != "1") return()
  ## Only on macOS
  if (Sys.info()["sysname"] != "Darwin") return()

  if (!file.exists("/etc/paths")) return()

  path <- Sys.getenv("PATH")
  new_path <- readLines("/etc/paths", n = 1000)
  Sys.setenv(PATH = paste0(path, ":", paste(new_path, collapse = ":")))

  invisible()
}

append_union <- function(path, cnt, msg_new = NULL, msg_done = NULL) {
  lines <- readLines(path)
  new_cnt <- setdiff(cnt, lines)
  if (length(new_cnt)) {
    new_lines <- c(lines, new_cnt)
    if (!is.null(msg_new)) cli::cli_alert_info(msg_new)
    writeLines(new_lines, path)
  } else {
    if (!is.null(msg_done)) cli::cli_alert_info(msg_done)
  }
  invisible()
}

try_add_to_git <- function(path) {
  tryCatch({
    processx::run("git", c("add", path), timeout = 10)
    cli::cli_alert_info("Add {.path {path}} to git.")
  }, error = function(x) x)
}

rimraf <- function(...) {
  x <- file.path(...)
  if ("~" %in% x) stop("Cowardly refusing to delete `~`")
  unlink(x, recursive = TRUE, force = TRUE)
}

msg <- function(..., domain = NULL, appendLF = TRUE) {
  msg <- .makeMessage(..., domain = domain, appendLF = appendLF)

  output <- if (is_interactive()) stdout() else stderr()

  withRestarts(muffleMessage = function() NULL, {
    signalCondition(simpleMessage(msg))
    cat(msg, file = output, sep = "")
  })
}

is_interactive <- function() {
  opt <- getOption("rlib_interactive")
  if (isTRUE(opt)) {
    TRUE
  } else if (identical(opt, FALSE)) {
    FALSE
  } else if (tolower(getOption("knitr.in.progress", "false")) == "true") {
    FALSE
  } else if (tolower(getOption("rstudio.notebook.executing", "false")) == "true") {
    FALSE
  } else if (identical(Sys.getenv("TESTTHAT"), "true")) {
    FALSE
  } else {
    interactive()
  }
}

loaded_packages <- function(lib) {
  lib <- normalizePath(lib)
  pkgs <- setdiff(loadedNamespaces(), base_packages())
  libs <- vcapply(pkgs, function(x) dirname(getNamespaceInfo(x, "path")))

  bad <- normalizePath(libs) == lib
  pkgs[bad]
}

lapply_with_names <- function(X, FUN, ...) {
  structure(lapply(X, FUN, ...), names = X)
}

na_omit <- function(x) {
  x[!is.na(x)]
}

## TODO: in theory the set of base packages can change over time,
## so we would need an R version specific vector here.
## Not an issue currently, might be in the future.

base_packages <- function() {

  if (is.null(pkg_data$base_packages)) {
    pkg_data$base_packages <-
      c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
        "methods", "parallel", "splines", "stats", "stats4", "tcltk",
        "tools", "utils"
        )
  }
  pkg_data$base_packages
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

map_named <- function(x, fun) {
  mapply(names(x), x, SIMPLIFY = FALSE, FUN = fun)
}
