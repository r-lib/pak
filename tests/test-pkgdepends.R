
if (file.exists("pkgdepends")) {
  library(testthat)
  library(pkgman)
  test <- function() {
    package <- "pkgman"
    env_test <- asNamespace("testthat")$env_test
    env_test$in_test <- TRUE
    env_test$package <- package
    on.exit({
      env_test$in_test <- FALSE
      env_test$package <- NULL
    })
    test_path <- "pkgdepends"
    asNamespace("testthat")$test_package_dir(
      package = package, test_path = test_path,
      filter = NULL, reporter = "check")
  }
  test()
}
