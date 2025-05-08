dir.exists <- function(x) {
  res <- file.exists(x) & file.info(x)$isdir
  stats::setNames(res, x)
}

pkg_path <- function(path = ".") {
  find_package_root(path)
}

pkg_name <- function(path = ".") {
  desc::desc_get("Package", pkg_path(path))[[1]]
}

gcc_arch <- function() {
  if (Sys.getenv("R_ARCH") == "/i386") "32" else "64"
}

is_windows <- function() {
  .Platform$OS.type == "windows"
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_na <- function(x) {
  identical(x, NA) ||
    identical(x, NA_integer_) ||
    identical(x, NA_character_) ||
    identical(x, NA_real_) ||
    identical(x, NA_complex_)
}

is_dir <- function(x) {
  isTRUE(file.info(x)$isdir)
}

# This is tools::makevars_user, provided here for backwards compatibility with older versions of R
makevars_user <- function() {
  m <- character()
  if (.Platform$OS.type == "windows") {
    if (!is.na(f <- Sys.getenv("R_MAKEVARS_USER", NA_character_))) {
      if (file.exists(f)) {
        m <- f
      }
    } else if (
      (Sys.getenv("R_ARCH") == "/x64") &&
        file.exists(f <- path.expand("~/.R/Makevars.win64"))
    ) {
      m <- f
    } else if (file.exists(f <- path.expand("~/.R/Makevars.win"))) {
      m <- f
    } else if (file.exists(f <- path.expand("~/.R/Makevars"))) {
      m <- f
    }
  } else {
    if (!is.na(f <- Sys.getenv("R_MAKEVARS_USER", NA_character_))) {
      if (file.exists(f)) {
        m <- f
      }
    } else if (
      file.exists(
        f <- path.expand(paste0(
          "~/.R/Makevars-",
          Sys.getenv("R_PLATFORM")
        ))
      )
    ) {
      m <- f
    } else if (file.exists(f <- path.expand("~/.R/Makevars"))) {
      m <- f
    }
  }
  m
}

last_char <- function(x) {
  l <- nchar(x)
  substr(x, l, l)
}

cat0 <- function(..., sep = "") {
  cat(..., sep = "")
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

flag_true_values <- c("true", "yes", "on", "1")
flag_false_values <- c("false", "no", "off", "0")

interpret_envvar_flag <- function(name, default = "false") {
  env <- tolower(Sys.getenv(name, default))
  if (env %in% flag_true_values) {
    return(TRUE)
  }
  if (env %in% flag_false_values) {
    return(FALSE)
  }
  if (is.na(env)) {
    return(NA)
  }

  stop(cli::format_error(
    "The {.envvar {name}} environment variable must be {.code true} or
     {.code false}, if set."
  ))
}

get_config_flag_value <- function(name, default = FALSE) {
  option_name <- paste0("pkg.build_", tolower(name))
  opt <- getOption(option_name, NULL)
  if (!is.null(opt)) {
    if (!is_flag(opt)) {
      stop(cli::format_error(
        "The {.code {option_name}} option must be {.code TRUE} or
          {.code FALSE}, if set."
      ))
    }
    return(opt)
  }

  envvar_name <- paste0("PKG_BUILD_", toupper(name))
  interpret_envvar_flag(envvar_name, default = tolower(as.character(default)))
}

should_stop_for_warnings <- function() {
  get_config_flag_value("stop_for_warnings")
}

isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

should_add_compiler_flags <- function() {
  val <- getOption("pkg.build_extra_flags", NULL)
  if (isTRUE(val)) {
    return(TRUE)
  }
  if (isFALSE(val)) {
    return(FALSE)
  }
  if (identical(val, "missing")) {
    return(length(makevars_user()) == 0)
  }
  if (!is.null(val)) {
    if (!is_string(val)) {
      stop(cli::format_error(c(
        "Invalid {.code pkg.build_extra_flags} option.",
        i = "It must be {.code TRUE}, {.code FALSE} or {.str missing}, not
             {.type {val}}."
      )))
    } else {
      stop(cli::format_error(c(
        "Invalid {.code pkg_build_extra_flags} option.",
        i = "It must be {.code TRUE}, {.code FALSE} or {.str missing}, not
            {.str {val}}."
      )))
    }
  }

  val <- Sys.getenv("PKG_BUILD_EXTRA_FLAGS", "true")
  if (val %in% flag_true_values) {
    return(TRUE)
  }
  if (val %in% flag_false_values) {
    return(FALSE)
  }
  if (val %in% "missing") {
    return(length(makevars_user()) == 0)
  }

  stop(cli::format_error(c(
    "Invalid {.envvar PKG_BUILD_EXTRA_FLAGS} environment variable.",
    i = "Must be one of {.code true}, {.code false} or {.code missing}."
  )))
}

get_desc_config_flag <- function(path, name) {
  name <- paste0("Config/build/", name)
  val <- desc::desc_get(name, file = path)
  if (is.na(val)) {
    return(NULL)
  }
  lval <- tolower(val)
  if (lval %in% flag_true_values) {
    return(TRUE)
  }
  if (lval %in% flag_false_values) {
    return(FALSE)
  }

  stop(cli::format_error(
    "The {.code {name}} entry in {.path DESCRIPTION} must be {.code TRUE}
     or {.code FALSE}.",
    "i" = "It is {.val {val}}."
  ))
}

mkdirp <- function(path, mode = NULL) {
  if (file.exists(path)) {
    if (file.info(path)$isdir) {
      if (is.null(mode)) {
        return()
      }
      mode <- as.octmode(mode)
      emode <- as.octmode(file.info(path)$mode)
      if (emode == mode) {
        return()
      }
      ret <- Sys.chmod(path, mode, use_umask = FALSE)
      if (!ret) {
        stop(cli::format_error(c(
          "Path {.path {path}} exists, but could not update mode to
             {.code {mode}} from {.code {emode}}."
        )))
      }
      return()
    }
    stop(cli::format_error(c(
      "Could not create directory {.path {path}}.",
      i = "Path already exists, but it is not a directory."
    )))
  }

  if (is.null(mode)) mode <- "0777"
  wrg <- NULL
  withCallingHandlers(
    ret <- dir.create(
      path,
      showWarnings = TRUE,
      recursive = TRUE,
      mode = mode
    ),
    warning = function(w) {
      wrg <<- w
      if (!is.null(findRestart("muffleWarning"))) {
        invokeRestart("muffleWarning")
      }
    }
  )

  if (!ret) {
    stop(cli::format_error(c(
      "Could not create directory {.path {path}}.",
      i = if (!is.null(wrg)) {
        "From {.fn dir.create}: {conditionMessage(wrg)}."
      } else {
        "For reasons unknown."
      }
    )))
  }
}

verb_for_cli <- function(x) {
  x <- gsub("\n", "\f", x, fixed = TRUE)
  x <- gsub(" ", "\u00a0", x, fixed = TRUE)
  x
}
