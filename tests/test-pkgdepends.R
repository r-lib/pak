
do <- function() {
  old <- Sys.getenv("R_PKG_CACHE_DIR", NA_character_)
  if (is.na(old)) {
    on.exit(Sys.unsetenv("R_PKG_CACHE_DIR"), add = TRUE)
  } else {
    on.exit(Sys.setenv("R_PKG_CACHE_DIR" = old), add = TRUE)
  }
  Sys.setenv("R_PKG_CACHE_DIR" = tempfile())

  if (file.exists("pkgdepends")) {
    library(testthat)
    library(pak)
    test <- function() {
      package <- "pak"
      env_test <- asNamespace("testthat")$env_test
      env_test$in_test <- TRUE
      env_test$package <- package
      on.exit({
        env_test$in_test <- FALSE
        env_test$package <- NULL
      }, add = TRUE)
      test_path <- "pkgdepends"
      asNamespace("testthat")$test_package_dir(
        package = package, test_path = test_path,
        filter = NULL, reporter = "check")
    }
    test()
  }
}

do()
