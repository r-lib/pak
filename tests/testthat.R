
do <- function() {
  old <- Sys.getenv("R_PKG_CACHE_DIR", NA_character_)
  if (is.na(old)) {
    on.exit(Sys.unsetenv("R_PKG_CACHE_DIR"), add = TRUE)
  } else {
    on.exit(Sys.setenv("R_PKG_CACHE_DIR" = old), add = TRUE)
  }
  Sys.setenv("R_PKG_CACHE_DIR" = tempfile())

  try({
    pkg_dir <- find.package("pak")
    lib <- file.path(pkg_dir, "library")
    library(testthat)
    library(pak)
    asNamespace("pak")$create_dev_lib(lib)
    test_check("pak")
  })
}

do()
