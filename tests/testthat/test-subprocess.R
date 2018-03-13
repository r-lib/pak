
context("subprocess related")

test_that("no dependencies are loaded with pkgman", {

  skip_on_cran()

  new_pkgs <- callr::r(
    function() {
      orig <- loadedNamespaces()
      library(pkgman)
      new <- loadedNamespaces()
      setdiff(new, orig)
    },
    timeout = 5
  )

  expect_identical(new_pkgs, "pkgman")
})
