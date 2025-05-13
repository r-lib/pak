if_fail <- function(expr, fn) {
  withCallingHandlers(expr, expectation_failure = fn)
}

test_temp_file <- function(
  fileext = "",
  pattern = "test-file-",
  envir = parent.frame(),
  create = TRUE
) {
  tmp <- tempfile(pattern = pattern, fileext = fileext)
  if (identical(envir, .GlobalEnv)) {
    message("Temporary files will _not_ be cleaned up")
  } else {
    withr::defer(
      try(unlink(tmp, recursive = TRUE, force = TRUE), silent = TRUE),
      envir = envir
    )
  }
  if (create) {
    cat("", file = tmp)
    normalizePath(tmp)
  } else {
    tmp
  }
}

test_temp_dir <- function(pattern = "test-dir-", envir = parent.frame()) {
  tmp <- test_temp_file(pattern = pattern, envir = envir, create = FALSE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  normalizePath(tmp)
}

httpbin_url <- function() {
  "eu.httpbin.org"
}

is_offline <- (function() {
  offline <- NULL
  function() {
    if (is.null(offline)) {
      offline <<- tryCatch(
        is.na(pingr::ping_port(httpbin_url(), port = 443, count = 1L)),
        error = function(e) TRUE
      )
    }
    offline
  }
})()

skip_if_offline <- function() {
  skip_on_cran()
  if (is_offline()) skip("Offline")
}

set_user_in_url <- function(url, username = "username", password = NULL) {
  psd <- parse_url(url)
  paste0(
    psd$protocol,
    "://",
    username,
    if (!is.null(password)) paste0(":", password),
    "@",
    psd$host,
    psd$path
  )
}

transform_tempfile <- function(x) {
  x <- sub("file[a-zA-Z0-9]+", "<tempfile>", x)
  x
}
