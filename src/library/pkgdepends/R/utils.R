zwnj <- function() {
  if (cli::is_utf8_output()) "\u200c" else ""
}

pkgd_data <- new.env(parent = emptyenv())

`%||%` <- function(l, r) if (is.null(l)) r else l

`%|0|%` <- function(l, r) if (length(l) == 0) r else l

`%|z|%` <- function(l, r) if (is.null(l) || identical(l, "")) r else l

`%&z&%` <- function(l, r) if (length(l) > 0 && l != "") r else ""

get_private <- function(x) x$.__enclos_env__$private

default_cran_mirror <- function() {
  mirror <- getOption("repos")["CRAN"]
  if (is.null(mirror) || is.na(mirror) || mirror == "@CRAN@") {
    "https://cran.rstudio.com"
  } else {
    mirror
  }
}

current_r_version <- function() {
  as.character(getRversion())
}

get_minor_r_version <- function(x) {
  x <- package_version(x)
  vapply(unclass(x), function(x) paste(x[1:2], collapse = "."), character(1))
}

str_trim <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

## TODO: in theory the set of base packages can change over time,
## so we would need an R version specific vector here.
## Not an issue currently, might be in the future.

## CRAN does not want me to call installed.packages at all, so let's
## hardcode this for now.

base_packages <- function() {
  if (is.null(pkgd_data$base_packages)) {
    pkgd_data$base_packages <- c(
      "base",
      "compiler",
      "datasets",
      "graphics",
      "grDevices",
      "grid",
      "methods",
      "parallel",
      "splines",
      "stats",
      "stats4",
      "tcltk",
      "tools",
      "utils"
    )
  }
  pkgd_data$base_packages
}

recommended_packages <- function() {
  if (is.null(pkgd_data$recommended_packages)) {
    pkgd_data$recommended_packages <- c(
      "boot",
      "class",
      "cluster",
      "codetools",
      "foreign",
      "KernSmooth",
      "lattice",
      "MASS",
      "Matrix",
      "mgcv",
      "nlme",
      "nnet",
      "rpart",
      "spatial",
      "survival"
    )
  }
  pkgd_data$recommended_packages
}

lapply_with_names <- function(X, FUN, ...) {
  structure(
    lapply(X, FUN, ...),
    names = names(X) %||% (if (is.character(X)) X)
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

update_named_vector <- function(old, new) {
  assert_that(all_named(old), all_named(new))
  comm <- intersect(names(old), names(new))
  add <- setdiff(names(new), names(old))
  old[comm] <- new[comm]
  old <- c(old, new[add])
  old
}

make_dl_status <- function(status, url, target, bytes, error = NULL) {
  obj <- list(
    status = status,
    url = url,
    target = target,
    bytes = NA_real_,
    error = NULL
  )

  if (status == "Got") {
    obj$bytes <- as.double(bytes)
  } else if (status == "Failed") {
    obj$error <- error
  } else if (grepl("^Had", status)) {
    obj$bytes <- as.double(bytes)
  }

  obj
}

comma_wrap <- function(x, indent = 2, exdent = indent, sep = ", ") {
  w <- strwrap(paste(x, collapse = sep), indent = indent, exdent = exdent)
  paste(w, collapse = "\n")
}

add_class <- function(obj, classes, where = c("start", "end")) {
  where <- match.arg(where)
  nc <- c(
    if (where == "start") classes,
    class(obj),
    if (where == "end") classes
  )
  class(obj) <- unique(nc)
  obj
}

is_na_scalar <- function(x) {
  length(x) == 1 && is.na(x)
}

omit_cols <- function(df, omit) {
  if (!length(omit)) {
    df
  } else {
    df[, setdiff(names(df), omit), drop = FALSE]
  }
}

same_sha <- function(s1, s2) {
  assert_that(
    is.character(s1),
    length(s1) == 1,
    is.character(s2),
    length(s2) == 1
  )
  if (is.na(s1) || is.na(s2)) return(FALSE)
  assert_that(
    is_string(s1),
    is_string(s2)
  )

  len <- min(nchar(s1), nchar(s2))
  substr(s1, 1, len) == substr(s2, 1, len)
}

format_iso_8601 <- function(date) {
  format(as.POSIXlt(date, tz = "UTC"), "%Y-%m-%dT%H:%M:%S+00:00")
}

cat0 <- function(..., sep = "") {
  cat(..., sep = sep)
}

lapply_rows <- function(df, fun, ...) {
  lapply(seq_len(nrow(df)), function(i) fun(df[i, ], ...))
}

detect_download_cache_dir <- local({
  dir <- NULL
  function() {
    if (is.null(dir)) dir <<- tempfile()
    dir
  }
})

rbind_expand <- function(..., .list = list()) {
  data <- c(list(...), .list)
  cols <- unique(unlist(lapply(data, function(x) colnames(x))))
  for (i in seq_along(data)) {
    miss_cols <- setdiff(cols, colnames(data[[i]]))
    if (length(miss_cols)) {
      na_df <- as_data_frame(structure(
        replicate(
          length(miss_cols),
          if (nrow(data[[i]])) NA else logical(),
          simplify = FALSE
        ),
        names = miss_cols
      ))
      data[[i]] <- as_data_frame(cbind(data[[i]], na_df))
    }
  }

  do.call(rbind, data)
}

drop_nulls <- function(x) {
  x[!vlapply(x, is.null)]
}

## R CMD check fixes
self <- private <- "foobar"
dummy <- function() {
  callr::r
}

get_num_workers <- function() {
  n <- tryCatch(
    suppressWarnings(as.integer(getOption("Ncpus", NA_integer_))),
    error = function(e) NA_integer_
  )

  if (length(n) != 1 || is.na(n)) {
    n <- tryCatch(
      ps::ps_cpu_count(logical = TRUE),
      error = function(e) NA_integer_
    )
  }

  if (is.na(n)) n <- 1L

  n
}

is_rcmd_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}

is_online <- local({
  online <- TRUE
  expires <- Sys.time()
  function() {
    if (is_rcmd_check()) return(FALSE)
    t <- Sys.time()
    if (t >= expires) {
      online <<- pingr::is_online()
      expires <<- t + as.difftime(10, units = "secs")
    }
    online
  }
})

# `reset` is useful for testing
once_per_session <- local({
  seen <- character()
  function(expr, reset = FALSE) {
    if (reset) {
      seen <<- character()
      return(invisible())
    } else {
      h <- cli::hash_obj_md5(substitute(expr))
      if (!h %in% seen) {
        seen <<- c(seen, h)
        expr
      }
    }
  }
})

# nocov start

synchronise <- synchronize <- sy <- function(...) {
  asNamespace("pkgcache")$synchronise(...)
}

async_constant <- function(...) {
  asNamespace("pkgcache")$async_constant(...)
}

http_get <- function(...) {
  asNamespace("pkgcache")$http_get(...)
}

http_post <- function(...) {
  asNamespace("pkgcache")$http_post(...)
}

http_delete <- function(...) {
  asNamespace("pkgcache")$http_delete(...)
}

when_all <- function(...) {
  asNamespace("pkgcache")$when_all(...)
}

async <- function(...) {
  asNamespace("pkgcache")$async(...)
}

async_map <- function(...) {
  asNamespace("pkgcache")$async_map(...)
}

async_timeout <- function(...) {
  asNamespace("pkgcache")$async_timeout(...)
}

download_file <- function(...) {
  asNamespace("pkgcache")$download_file(...)
}

download_one_of <- function(...) {
  asNamespace("pkgcache")$download_one_of(...)
}

http_stop_for_status <- function(...) {
  asNamespace("pkgcache")$http_stop_for_status(...)
}

new_async_timer <- function(...) {
  asNamespace("pkgcache")$async_timer$new(...)
}

async_delay <- function(...) {
  asNamespace("pkgcache")$delay(...)
}

external_process <- function(...) {
  asNamespace("pkgcache")$external_process(...)
}

get_user_cache_dir <- function(...) {
  asNamespace("pkgcache")$get_user_cache_dir(...)
}

# nocov end

format_error_with_stdout <- function(x, ...) {
  msg <- conditionMessage(x)
  if (is.null(x$stdout)) {
    paste0(msg, " (output not available)")
  } else {
    out <- last_stdout_lines(x$stdout, "stdout + stderr", "OE> ")
    c(paste0(msg, out[1]), out[-1])
  }
}

last_stdout_lines <- function(lines, std, prefix = "E> ") {
  if (err$is_interactive()) {
    pref <- paste0(
      ", ",
      std,
      if (length(lines) > 10) " (last 10 lines)",
      ":"
    )
    out <- paste0(prefix, utils::tail(lines, 10))
    c(pref, "", out)
  } else {
    out <- paste0(prefix, lines)
    c(paste0(", ", std, ":"), "", out)
  }
}

# I am a coward to test this :P
# nocov start

rimraf <- function(...) {
  x <- file.path(...)
  if ("~" %in% x) {
    throw(pkg_error(
      "Cowardly refusing to delete {.path ~}.",
      "i" = paste0(
        "You have a file or directory named {.path ~} and ",
        "because of an R bug deleting that could delete your ",
        "whole home directory."
      )
    ))
  }
  unlink(x, recursive = TRUE, force = TRUE)
}

# nocov end

is_windows <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "windows")
}

# used in coverage condition

is_linux <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "linux") # nocov
}

# This is a workaround for some RStudio bugs:
# https://github.com/r-lib/pkginstall/issues/42
# https://github.com/rstudio/rstudio/issues/2387
# https://github.com/rstudio/rstudio/issues/7278

is_older_rstudio <- function() {
  rs <- rstudio$detect()
  rs$type == "rstudio_console" &&
    !is.null(rs$version) &&
    rs$version <= "1.4.800"
}

ansi_align_width <- function(text) {
  if (length(text) == 0) return(text)
  width <- max(cli::ansi_nchar(text, type = "width"))
  cli::ansi_align(text, width = width)
}

get_id <- local({
  id <- 0L
  function() {
    id <<- id + 1L
    id
  }
})

# tools::md5sum has issues with UTF=8 file names on Windows, <= R 4.0
# TODO: switch to cli::hash_md5_file, once released. I tested that it works
# well on Windows non-ascii file names

safe_md5sum <- function(path) {
  assert_that(is_path(path))
  tryCatch(
    tools::md5sum(path),
    error = function(err) {
      tmp <- tempfile()
      on.exit(unlink(tmp, force = TRUE, recursive = TRUE), add = TRUE)
      file.copy(path, tmp)
      structure(tools::md5sum(tmp), names = path)
    }
  )
}

# TODO: rewrite this in C in ps

get_euid <- function() {
  euid <- tryCatch(
    as.integer(processx::run("id", "-u")$stdout),
    error = function(e) NA_integer_
  )
  if (length(euid) != 1 || is.na(euid)) euid <- NA_integer_
  euid
}

zip_list <- function(zipfile) {
  utils::unzip(zipfile, list = TRUE, unzip = "internal")[, 1]
}

os_type <- function() .Platform$OS.type # nocov

is.dir <- function(path) {
  assert_that(is_string(path), file.exists(path))
  file.info(path)$isdir
}

map_named <- function(x, fun) {
  mapply(names(x), x, SIMPLIFY = FALSE, FUN = fun)
}

sort_by_name <- function(x) {
  x[order(names(x))]
}

is_pak <- function() {
  Sys.getenv("THIS_IS_PAK") == "true" ||
    Sys.getenv("R_PKG_PKG_WORKER") == "true"
}

pak_or_pkgdepends <- function() {
  if (is_pak()) "pak" else "pkgdepends"
}

pakx_version <- function() {
  if (is_pak()) utils::packageVersion("pak") else
    utils::packageVersion("pkgdepends")
}

remove_entry <- function(l, n) {
  l[names(l) != n]
}

path_norm <- function(x) {
  normalizePath(x, mustWork = FALSE)
}

parse_platform <- function(x) {
  pcs <- strsplit(x, "-", fixed = TRUE)
  data_frame(
    cpu = vcapply(pcs, "[", 1),
    vendor = vcapply(pcs, "[", 2),
    os = vcapply(pcs, function(y) {
      if (length(y) < 3) NA_character_ else paste(y[-(1:2)], collapse = "-")
    })
  )
}

single_quote <- function(x) {
  encodeString(x, quote = "'", na.encode = FALSE)
}

backtick <- function(x) {
  encodeString(x, quote = "`", na.encode = FALSE)
}

collapse <- function(x, ...) {
  cli::ansi_collapse(x, ...)
}

na_omit <- function(x) {
  x[!is.na(x)]
}

file_ext <- function(x) {
  re_match(x, "[.]([[:alnum:]]+)$")[[".match"]]
}

# drop a prefix and a postfix, vectorized
omit_pre_post <- function(x, pre = 0, post = 0) {
  substr(x, 1L + pre, nchar(x) - post)
}

truthy_strings <- c("true", "t", "1", "on", "yes")

is_truthy <- function(x) {
  (is.logical(x) && length(x) >= 1 && !is.na(x[[1]]) && x[[1]]) ||
    (is.character(x) && length(x) >= 1 && tolower(x) %in% truthy_strings)
}

# check that all `path` are inside `root`
check_inside_dir <- function(root, path) {
  # we can assume that `root` and all `path` do exist
  # normalizePath removes trailing '/'
  nroot <- normalizePath(root, winslash = "/")
  npath <- normalizePath(path, winslash = "/")
  if (any(bad <- nroot != npath & !startsWith(paste0(npath, "/"), nroot))) {
    throw(pkg_error(
      "{.path {path[bad]}} {?is/are} outside of project
       root {.path {root}}."
    ))
  }
}
