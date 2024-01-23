# Needed for pkgcache
#' @rawNamespace if (getRversion() >= "4.0.0") importFrom(tools, R_user_dir)
NULL

`%||%` <- function(l, r) if (is.null(l)) r else l

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

str_trim <- function(x) {
  x <- sub("(*UCP)\\s+$", "", x, perl = TRUE)
  x <- sub("(*UCP)^\\s+", "", x, perl = TRUE)
  x
}

get_minor_r_version <- function(x = getRversion()) {
  x <- package_version(x)
  vapply(unclass(x), function(x) paste(x[1:2], collapse = "."), character(1))
}

# nocov start
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
# nocov end

user_cache_dir <- function(appname = utils::packageName()) {
  if (nzchar(cache <- Sys.getenv("R_PKG_CACHE_DIR", ""))) {
    return(cache)
  }
  if (nzchar(cache <- Sys.getenv("R_USER_CACHE_DIR", ""))) {
    return(file.path(cache, "R", appname))
  }
  switch(get_os(),
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

mkdirp <- function(dir, msg = NULL) {
  s <- vlapply(dir, dir.create, recursive = TRUE, showWarnings = FALSE)
  if (any(s) && !is.null(msg)) {
    load_all_private()
    cli <- pkg_data[["ns"]][["cli"]]
    cli$cli_alert_info("{msg}: {.path {dir[s]}}.")
  }
  invisible(s)
}

fix_macos_path_in_rstudio <- function() {
  ## Only in RStudio
  if (Sys.getenv("RSTUDIO") != "1") {
    return()
  }
  ## Only on macOS
  if (Sys.info()["sysname"] != "Darwin") {
    return()
  }

  if (!file.exists("/etc/paths")) {
    return()
  }

  path <- Sys.getenv("PATH")
  new_path <- readLines("/etc/paths", n = 1000)
  Sys.setenv(PATH = paste0(path, ":", paste(new_path, collapse = ":")))

  invisible()
}

rimraf <- function(...) {
  x <- file.path(...)
  if ("~" %in% x) stop("Cowardly refusing to delete `~`") # nocov coward...
  unlink(x, recursive = TRUE, force = TRUE)
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
      c(
        "base", "compiler", "datasets", "graphics", "grDevices", "grid",
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

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0 &&
    as.integer(x) == x
}

map_named <- function(x, fun) {
  mapply(names(x), x, SIMPLIFY = FALSE, FUN = fun)
}

cisort <- function(x) {
  x[order(tolower(x))]
}

read_char <- function(path) {
  bin <- readBin(path, "raw", file.info(path)$size)
  txt <- rawToChar(bin)
  Encoding(txt) <- "UTF-8"
  txt
}
