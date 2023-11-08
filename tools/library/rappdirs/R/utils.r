check_os <- function(os) {
  if (is.null(os)) {
    get_os()
  } else {
    if (length(os) != 1 || !is.character(os)) {
      stop("`os` must be a string", call. = FALSE)
    }
    if (!os %in% c("win", "mac", "unix")) {
      stop("`os` must be one of 'win', 'mac', 'unix'", call. = FALSE)
    }
    os
  }
}

get_os <- function() {
  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  } else {
    "unix"
  }
}

file_path <- function(...) {
  paste(c(...), collapse = .Platform$file.sep)
}

"%||%" <- function(a, b) if (is.null(a)) b else a

base_path <- function(os, type, win, mac, unix) {
  name <- paste0("R_USER_", type, "_DIR")
  val <- Sys.getenv(name)

  if (!identical(val, "")) {
    val
  } else {
    switch(check_os(os), win = win, mac = mac, unix = unix)
  }
}

win_path <- function(type_appdata = "common") {
  CSIDL_APPDATA <- 26L
  CSIDL_COMMON_APPDATA <- 35L
  CSIDL_LOCAL_APPDATA <- 28L

  switch(type_appdata,
    roaming = win_path_csidl(CSIDL_APPDATA) %||% win_path_env("roaming"),
    local = win_path_csidl(CSIDL_LOCAL_APPDATA) %||% win_path_env("local"),
    common = win_path_csidl(CSIDL_COMMON_APPDATA) %||% win_path_env("common")
  )
}

#' @useDynLib rappdirs, .registration=TRUE
win_path_csidl <- function(csidl) {
  stopifnot(is.integer(csidl), length(csidl) == 1)
  path <- .Call(win_path_, csidl, PACKAGE = "rappdirs")
  path
}

# How to get reasonable window paths via environmental variables
win_path_env <- function(type) {
  if (type == "roaming") {
    env_fallback("APPDATA")
  } else if (type == "local") {
    path <- Sys.getenv("LOCALAPPDATA", unset = NA)
    if (is.na(path)) { # environmental variable not defined in XP
      path <- file.path(
        env_fallback("USERPROFILE"),
        "Local Settings", "Application Data"
      )
    }
    path
  } else if (type == "common") {
    path <- Sys.getenv("PROGRAMDATA", unset = NA)
    if (is.na(path)) {
      path <- file.path(env_fallback("ALLUSERPROFILE"), "Application Data")
    }
    path
  } else {
    stop("invalid `type` argument")
  }
}

env_fallback <- function(env) {
  val <- Sys.getenv(env)

  if (identical(val, "")) {
    if (get_os() == "win") {
      stop("Can't find envvar '", env, "'", call. = FALSE)
    } else {
      # Fall back so examples still work when not on windows
      paste0("<", env, ">")
    }
  } else {
    val
  }
}

# version -----------------------------------------------------------------

check_version <- function(version, appname, expand = FALSE) {
  if (is.null(appname) && !is.null(version)) {
    warning("version is ignored when appname is null", call. = FALSE)
    NULL
  } else {
    if (expand) {
      version <- expand_r_libs_specifiers(version)
    }
    version
  }
}

expand_r_libs_specifiers <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  rversion <- getRversion()
  x <- gsub_special("%V", rversion, x)
  x <- gsub_special("%v", paste(rversion$major, rversion$minor, sep = "."), x)
  x <- gsub_special("%p", R.version$platform, x)
  x <- gsub_special("%o", R.version$os, x)
  x <- gsub_special("%a", R.version$arch, x)
  x <- gsub("%%", "%", x)
  x
}

gsub_special <- function(pattern, replacement, x) {
  gsub(paste0("([^%]|^)", pattern), paste0("\\1", replacement), x)
}
