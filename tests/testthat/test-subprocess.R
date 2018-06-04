
context("subprocess related")

test_that("no dependencies are loaded with pkgman", {

  skip_on_cran()

  new_pkgs <- callr::r(
    function() {
      withr::with_options(list(pkgman.subprocess = FALSE), {
        orig <- loadedNamespaces()
        library(pkgman)
        new <- loadedNamespaces()
      })
      setdiff(new, orig)
    },
    timeout = 5
  )

  if (length(new_pkgs) > 1) print(new_pkgs)
  expect_identical(new_pkgs, "pkgman")
})
