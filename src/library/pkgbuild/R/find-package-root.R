find_package_root <- function(path = ".") {
  is_root <- function(path) {
    identical(
      normalizePath(path, winslash = "/"),
      normalizePath(dirname(path), winslash = "/")
    )
  }

  if (!file.exists(path)) {
    stop("Path does not exist: ", path)
  }
  cur_path <- normalizePath(path, winslash = "/")
  errmsg <- paste0(
    "Could not find R package in `",
    path,
    "` or its parent directories."
  )
  max_depth <- 100
  for (i in 1:max_depth) {
    dsc_path <- file.path(cur_path, "DESCRIPTION")
    if (
      file.exists(dsc_path) &&
        any(grepl("^Package: ", readLines(dsc_path)))
    ) {
      return(cur_path)
    } else if (is_root(cur_path)) {
      stop(errmsg)
    } else {
      cur_path <- dirname(cur_path)
    }
  }
  stop(errmsg, " Checked ", max_depth, " parent directories.") # nocov
}
