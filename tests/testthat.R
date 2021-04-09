
do <- function() {
  old <- Sys.getenv("R_PKG_CACHE_DIR", NA_character_)
  if (is.na(old)) {
    on.exit(Sys.unsetenv("R_PKG_CACHE_DIR"), add = TRUE)
  } else {
    on.exit(Sys.setenv("R_PKG_CACHE_DIR" = old), add = TRUE)
  }
  Sys.setenv("R_PKG_CACHE_DIR" = tempfile())

  print(asNamespace("pak")$pak_sitrep_data)
  library(testthat)
  library(pak)
  test_check("pak")
}

do()
