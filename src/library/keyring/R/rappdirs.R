rappdirs <- local({
  user_config_dir <- function(
    appname = "r-keyring",
    appauthor = appname,
    version = NULL,
    roaming = TRUE,
    expand = TRUE,
    os = NULL
  ) {
    if (nzchar(conf <- Sys.getenv("R_PKG_CONFIG_DIR", ""))) {
      return(conf)
    }
    if (nzchar(conf <- Sys.getenv("R_USER_CONFIG_DIR", ""))) {
      return(file.path(conf, "R"))
    }
    os <- os %||% get_os()
    version <- check_version(version, appname, expand)

    base <- switch(
      os,
      win = win_path(ifelse(roaming, "roaming", "local")),
      mac = "~/Library/Application Support",
      unix = Sys.getenv("XDG_CONFIG_HOME", "~/.config")
    )
    file_path(base, if (os == "win") appauthor, appname, version)
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
    normalizePath(do.call("file.path", as.list(c(...))), mustWork = FALSE)
  }

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
    x <- gsub_special(
      "%v",
      paste(rversion$major, rversion$minor, sep = "."),
      x
    )
    x <- gsub_special("%p", R.version$platform, x)
    x <- gsub_special("%o", R.version$os, x)
    x <- gsub_special("%a", R.version$arch, x)
    x <- gsub("%%", "%", x)
    x
  }

  gsub_special <- function(pattern, replacement, x) {
    gsub(paste0("([^%]|^)", pattern), paste0("\\1", replacement), x)
  }

  win_path <- function(type_appdata = "common") {
    CSIDL_APPDATA <- 26L
    CSIDL_COMMON_APPDATA <- 35L
    CSIDL_LOCAL_APPDATA <- 28L
    switch(
      type_appdata,
      roaming = win_path_csidl(CSIDL_APPDATA) %||% win_path_env("roaming"),
      local = win_path_csidl(CSIDL_LOCAL_APPDATA) %||% win_path_env("local"),
      common = win_path_csidl(CSIDL_COMMON_APPDATA) %||% win_path_env("common")
    )
  }

  win_path_env <- function(type) {
    if (type == "roaming") {
      env_fallback("APPDATA")
    } else if (type == "local") {
      path <- Sys.getenv("LOCALAPPDATA", unset = NA)
      if (is.na(path)) {
        path <- file.path(
          env_fallback("USERPROFILE"),
          "Local Settings",
          "Application Data"
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

  win_path_csidl <- function(csidl) {
    stopifnot(is.integer(csidl), length(csidl) == 1)
    path <- .Call(win_path_, csidl)
    path
  }

  env_fallback <- function(env) {
    val <- Sys.getenv(env)
    if (identical(val, "")) {
      if (get_os() == "win") {
        stop("Can't find envvar '", env, "'", call. = FALSE)
      } else {
        paste0("<", env, ">")
      }
    } else {
      val
    }
  }

  list(
    .env = environment(),
    user_config_dir = user_config_dir
  )
})

user_config_dir <- rappdirs$user_config_dir
