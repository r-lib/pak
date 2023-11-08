
mkdirp <- function(dir, msg = NULL) {
  s <- vlapply(dir, dir.create, recursive = TRUE, showWarnings = FALSE)
  invisible(s)
}

file_get_time <- function(path) {
  file.info(path)$mtime
}

file_set_time <- function(path, time = Sys.time()) {
  assert_that(
    is_character(path),
    inherits(time, "POSIXct"))
  vlapply(path, Sys.setFileTime, time = time)
}

## file.copy is buggy when to is a vector

file_copy_with_time <- function(from, to) {
  mkdirp(dirname(to))
  if (length(to) > 1) {
    mapply(file.copy, from, to,
           MoreArgs = list(overwrite =  TRUE, copy.date = TRUE),
           USE.NAMES = FALSE)
  } else {
    file.copy(from, to, overwrite = TRUE, copy.date = TRUE)
  }
}
