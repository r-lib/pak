library(testthat)
library(pak)

do <- function() {
  old <- Sys.getenv("R_PKG_CACHE_DIR", NA_character_)
  if (is.na(old)) {
    on.exit(Sys.unsetenv("R_PKG_CACHE_DIR"))
  } else {
    on.exit(Sys.setenv("R_PKG_CACHE_DIR" = old))
  }
  Sys.setenv("R_PKG_CACHE_DIR" = tempfile())

  test_check("pak")
}

do()
