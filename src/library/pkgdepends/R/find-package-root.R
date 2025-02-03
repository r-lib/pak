project_root_anchors <-
  c("DESCRIPTION", ".git", ".Rproj.user", "renv.lock", "renv")

find_package_root <- function(path = ".") {
  find_project_root(path = path, anchors = "DESCRIPTION")
}

is_package_root <- function(path = ".") {
  file.exists(file.path(path, "DESCRIPTION"))
}

find_project_root <- function(path = ".", anchors = project_root_anchors) {
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
    if (any(file.exists(file.path(cur_path, anchors)))) {
      return(cur_path)
    } else if (is_root(cur_path)) {
      stop(errmsg)
    } else {
      cur_path <- dirname(cur_path)
    }
  }
  stop(errmsg, " Checked ", max_depth, " parent directories.") # nocov
}
