options(
  cranyon.enabled = FALSE,
  cli.hyperlink = FALSE,
  cli.hyperlink_run = FALSE,
  cli.hyperlink_help = FALSE,
  cli.hyperlink_vignette = FALSE,
  cli.dynamic = FALSE,
  cli.unicode = FALSE,
  cli.condition_width = Inf,
  cli.num_colors = 1L,
  useFancyQuotes = FALSE,
  lifecycle_verbosity = "warning",
  OutDec = ".",
  rlang_interactive = FALSE,
  max.print = 99999
)
Sys.unsetenv("RSTUDIO")


if (Sys.getenv("PAK_EXTRA_TESTS") != "true") {
  stop("Set PAK_EXTRA_TESTS=true to run these tests")
}

attach(asNamespace("pak"), name = "pak-internals")
library(pak)

if (Sys.which("docker") == "") {
  stop("No 'docker', giving up now")
}

cnt <- system2("docker", c("ps", "-q", "--filter", "name=fake"), stdout = TRUE)
if (length(cnt) == 1) {
  message("Fake container already running")
} else {
  message("Starting fake container")
  system2("docker", c("rm", "fake"))
  if (system2("docker", c(
    "run", "-d", "-p", "3100:3100", "-p", "3101:3101",
    "-p", "3102:3102", "-p", "3103:3103", "-p", "3104:3104",
    "-p", "3105:3105", "-p", "3106:3106", "--name", "fake", "fake"
  )) != 0) {
    stop("Could not start docker container")
  }
}

Sys.setenv("R_PKG_CACHE_DIR" = tempfile())

test_that <- function(label, code) {
  tryCatch(
    withCallingHandlers(
      {
        code
        message("  OK ")
      },
      error = function(cnd) {
        message("  FAIL")
      }
    ),
    skip = function(cnd) {
      message("  SKIP: ", conditionMessage(cnd))
    }
  )
}

expect_equal <- function(object, expected) {
  stopifnot(all.equal(object, expected))
}

expect_true <- function(expr) {
  stopifnot(isTRUE(expr))
}

expect_false <- function(expr) {
  stopifnot(identical(expr, FALSE))
}

expect_snapshot <- function(x, error = FALSE, transform = NULL) {
  if (error) {
    try(x)
  } else {
    x
  }
}

skip_on_cran <- function(...) {
  # do nothing
}

skip <- function(message = "Skipping") {
  message <- paste0(message, collapse = "\n")
  cond <- structure(list(message = paste0("Reason: ", message)),
    class = c("skip", "condition")
  )
  stop(cond)
}

setup_fake_apps <- function(...) {
  options(
    repos = c(CRAN = "http://127.0.0.1:3100"),
    cran_metadata_url = "http://127.0.0.1:3100"
  )
  Sys.setenv(
    R_PKG_CRAN_METADATA_URL = "http://127.0.0.1:3100",
    R_BIOC_CONFIG_URL = "http://127.0.0.1:3101/config.yaml",
    R_BIOC_MIRROR = "http://127.0.0.1:3101",
    R_PKG_GITHUB_API_URL = "http://127.0.0.1:3105"
  )
  Sys.unsetenv("R_BIOC_VERSION")
}

stub <- function(...) {
  # need to skip for now
  cond <- structure(
    list(message = "no mockery"),
    class = c("skip", "condition")
  )
  stop(cond)
}

expect_error <- function(object, regexp, ...) {
  tryCatch(
    object,
    error = function(cnd) {
      stopifnot(grepl(regexp, conditionMessage(cnd), ...))
    }
  )
}

local_tempdir <- function() {
  dir.create(tmp <- tempfile(), showWarnings = FALSE, recursive = TRUE)
  tmp
}
