`%||%` <- function(l, r) if (is.null(l)) r else l

`%&&%` <- function(l, r) if (is.null(l)) NULL else r

get_zip_data <- function(
  files,
  recurse,
  keep_path,
  include_directories,
  keys = NULL
) {
  list <- if (is.null(keys)) {
    if (keep_path) {
      get_zip_data_path(files, recurse)
    } else {
      get_zip_data_nopath(files, recurse)
    }
  } else {
    get_zip_data_keys(files, keys, recurse, keep_path)
  }

  if (!include_directories) {
    list <- list[!list$dir, ]
  }

  list
}

get_zip_data_keys <- function(files, keys, recurse, keep_path) {
  is_dir <- file.info(files)$isdir
  results <- vector("list", length(files))
  any_dir_ignored <- FALSE

  for (i in seq_along(files)) {
    file <- files[[i]]
    key <- keys[[i]]
    dir <- is_dir[[i]]

    if (dir && !recurse) {
      any_dir_ignored <- TRUE
      next
    }

    if (dir) {
      if (!keep_path && file == ".") {
        contents <- get_zip_data_nopath(".", recurse)
        if (nrow(contents) > 0) {
          contents$key <- paste0(key, "/", contents$key)
        }
        sub_data <- rbind(
          data_frame(
            key = paste0(key, "/"),
            file = normalizePath("."),
            dir = TRUE
          ),
          contents
        )
      } else if (keep_path) {
        sub_data <- get_zip_data_path_recursive(file)
        old_prefix <- file
        sub_data$key <- remap_key_prefix(sub_data$key, old_prefix, key)
      } else {
        sub_data <- get_zip_data_nopath_recursive(file)
        old_prefix <- basename(normalizePath(file))
        sub_data$key <- remap_key_prefix(sub_data$key, old_prefix, key)
      }
    } else {
      sub_data <- data_frame(
        key = key,
        file = normalizePath(file),
        dir = FALSE
      )
    }

    results[[i]] <- sub_data
  }

  if (any_dir_ignored) {
    warning("directories ignored in zip file, specify recurse = TRUE")
  }

  result <- do.call(rbind, Filter(Negate(is.null), results))
  dup <- duplicated(result$file)
  if (any(dup)) {
    result <- result[!dup, ]
  }
  result
}

remap_key_prefix <- function(keys, old_prefix, new_prefix) {
  paste0(new_prefix, substr(keys, nchar(old_prefix) + 1L, nchar(keys)))
}

get_zip_data_path <- function(files, recurse) {
  if (recurse && length(files)) {
    data <- do.call(rbind, lapply(files, get_zip_data_path_recursive))
    dup <- duplicated(data$files)
    if (any(dup)) {
      data <- data <- data[!dup, drop = FALSE]
    }
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
    if (any(dup)) {
      data <- data[!dup, drop = FALSE]
    }
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

raw_to_hex <- function(x) {
  paste0(sprintf("%02x", as.integer(x)), collapse = "")
}

resolve_password <- function(password) {
  password <- password %||% getOption("zip_password")
  if (is.null(password)) {
    return(NULL)
  }
  if (is.function(password)) {
    password <- password()
  }
  if (is.raw(password)) {
    if (length(password) == 0) {
      stop("`password` must not be empty")
    }
    return(password)
  }
  if (!is.character(password) || length(password) != 1 || is.na(password)) {
    stop(
      "`password` must be a string, a raw vector, or a function returning one"
    )
  }
  if (!nzchar(password)) {
    stop("`password` must not be empty")
  }
  charToRaw(enc2utf8(password))
}

encryption_code <- function(encryption) {
  encryption <- match.arg(encryption, c("aes256", "aes128", "zipcrypto"))
  switch(
    encryption,
    aes256 = 3L,
    aes128 = 1L,
    zipcrypto = 4L
  )
}

is_true_option <- function(name) {
  opt <- getOption(name)
  if (is.null(opt)) {
    NULL
  } else if (isTRUE(opt)) {
    TRUE
  } else if (isFALSE(opt)) {
    FALSE
  } else {
    stop(sprintf("Option `%s` must be TRUE, FALSE", name))
  }
}

true_values <- c("true", "on", "yes", "1", "yeah", "yep", "y")
false_values <- c("false", "off", "no", "0", "nope", "nay", "n")

is_true_env_var <- function(name) {
  env <- tolower(Sys.getenv(name, unset = NA))
  if (is.na(env)) {
    NULL
  } else if (env %in% true_values) {
    TRUE
  } else if (env %in% false_values) {
    FALSE
  } else {
    stop(sprintf("Environment variable `%s` must be TRUE or FALSE", name))
  }
}

is_progress_enabled <- function() {
  enabled <- is_true_option("zip_progress") %||%
    is_true_option("zip.progress") %||%
    is_true_env_var("ZIP_PROGRESS") %||%
    FALSE
  enabled && requireNamespace("cli", quietly = TRUE)
}
