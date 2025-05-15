#' @export

format.package_packaging_error <- function(x, ...) {
  format_error_with_stdout(x, ...)
}

#' @export

print.package_packaging_error <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' @export

format.package_uncompress_error <- function(x, ...) {
  out <- conditionMessage(x)
  if (!is.null(x$data$stdout)) {
    stdout <- last_stdout_lines(x$stdout, "", prefix = "O> ")[-(1:2)]
    out <- c(out, "", "Standard output:", stdout)
  }
  out
}

#' @export

print.package_uncompress_error <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' @export

format.package_build_error <- function(x, ...) {
  format_error_with_stdout(x, ...)
}

#' @export

print.package_build_error <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

is_loaded <- function(package) {
  package %in% loadedNamespaces()
}

create_temp_dir <- function(..., tmpdir = tempdir()) {
  f <- tempfile(tmpdir = tmpdir, ...)
  dir.create(f)
  f
}

library_cache <- function(lib) {
  lib_cache <- file.path(lib, "_cache")
  dir.create(lib_cache, recursive = TRUE, showWarnings = FALSE)
  lib_cache
}

lock_cache <- function(cache, pkg_name, lock = TRUE) {
  use_lock <- !identical(lock, FALSE)
  my_lock <- NULL
  if (use_lock) {
    lockfile <- file.path(cache, sprintf("%s.lock", pkg_name))
    # TODO: timeout and fail?
    my_lock <- filelock::lock(lockfile)
  }
  my_lock
}

unlock <- function(lock) {
  if (is.null(lock)) {
    return()
  }
  filelock::unlock(lock)
}


sysname <- function() {
  res <- tolower(Sys.info()[["sysname"]])
  map <- c(darwin = "mac", "sunos" = "solaris")[res]
  res[!is.na(map)] <- map
  res
}

rep_list <- function(n, expr) {
  lapply(integer(n), eval.parent(substitute(function(...) expr)))
}

cut_into_lines <- function(x) {
  x <- do.call(paste0, as.list(x))
  x <- gsub("\r\n", "\n", x, fixed = TRUE)
  x <- strsplit(x, "\n", fixed = TRUE)[[1]]
  if (length(x)) x else ""
}
