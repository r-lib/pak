`%||%` <- function(l, r) if (is.null(l)) r else l

get_zip_data <- function(files, recurse, keep_path, include_directories) {
  list <- if (keep_path) {
    get_zip_data_path(files, recurse)
  } else {
    get_zip_data_nopath(files, recurse)
  }

  if (!include_directories) {
    list <- list[!list$dir, ]
  }

  list
}

get_zip_data_path <- function(files, recurse) {
  if (recurse && length(files)) {
    data <- do.call(rbind, lapply(files, get_zip_data_path_recursive))
    dup <- duplicated(data$files)
    if (any(dup)) data <- data <- data[!dup, drop = FALSE]
    data
  } else {
    files <- ignore_dirs_with_warning(files)
    data_frame(
      key = files,
      files = files,
      dir = rep(FALSE, length(files))
    )
  }
}

warn_for_dotdot <- function(files) {
  if (any(grepl("^[.][/\\\\]", files))) {
    warning("Some paths start with `./`, creating non-portable zip file")
  }
  if (any(grepl("^[.][.][/\\\\]", files))) {
    warning(
      "Some paths reference parent directory, ",
      "creating non-portable zip file"
    )
  }
  files
}

warn_for_colon <- function(files) {
  if (any(grepl(":", files, fixed = TRUE))) {
    warning(
      "Some paths include a `:` character, this might cause issues ",
      "when uncompressing the zip file on Windows."
    )
  }
}

fix_absolute_paths <- function(files) {
  if (any(startsWith(files, "/"))) {
    warning(
      "Dropping leading `/` from paths, all paths in a zip file ",
      "must be relative paths."
    )
    files <- sub("^/", "", files)
  }
  files
}

get_zip_data_nopath <- function(files, recurse) {
  if ("." %in% files) {
    files <- c(setdiff(files, "."), dir(all.files = TRUE, no.. = TRUE))
  }
  if (recurse && length(files)) {
    data <- do.call(rbind, lapply(files, get_zip_data_nopath_recursive))
    dup <- duplicated(data$files)
    if (any(dup)) data <- data[!dup, drop = FALSE]
    data
  } else {
    files <- ignore_dirs_with_warning(files)
    data_frame(
      key = basename(files),
      file = files,
      dir = rep(FALSE, length(files))
    )
  }
}

ignore_dirs_with_warning <- function(files) {
  info <- file.info(files)
  if (any(info$isdir)) {
    warning("directories ignored in zip file, specify recurse = TRUE")
    files <- files[!info$isdir]
  }
  files
}

get_zip_data_path_recursive <- function(x) {
  if (file.info(x)$isdir) {
    files <- c(
      x,
      dir(
        x,
        recursive = TRUE,
        full.names = TRUE,
        all.files = TRUE,
        include.dirs = TRUE,
        no.. = TRUE
      )
    )
    dir <- file.info(files)$isdir
    data_frame(
      key = ifelse(dir, paste0(files, "/"), files),
      file = normalizePath(files),
      dir = dir
    )
  } else {
    data_frame(
      key = x,
      file = normalizePath(x),
      dir = FALSE
    )
  }
}

get_zip_data_nopath_recursive <- function(x) {
  if ("." %in% x) {
    x <- c(setdiff(x, "."), dir(all.files = TRUE, no.. = TRUE))
  }
  x <- normalizePath(x)
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(dirname(x))
  bnx <- basename(x)

  files <- dir(
    bnx,
    recursive = TRUE,
    all.files = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )

  key <- c(bnx, file.path(bnx, files))
  files <- c(x, file.path(dirname(x), bnx, files))
  dir <- file.info(files)$isdir
  key <- ifelse(dir, paste0(key, "/"), key)

  data_frame(
    key = key,
    file = normalizePath(files),
    dir = dir
  )
}

mkdirp <- function(x, ...) {
  dir.create(x, showWarnings = FALSE, recursive = TRUE, ...)
}

need_packages <- function(pkgs, what = "this function") {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(sprintf("The `%s` package is needed for %s", p, what))
    }
  }
}

enc2c <- function(x) {
  if (.Platform$OS.type == "windows") {
    enc2utf8(x)
  } else {
    enc2native(x)
  }
}
