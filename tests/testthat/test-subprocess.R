
context("subprocess related")

test_that("no dependencies are loaded with pkgman", {

  skip_on_cran()

  ## Skip this is covr, because covr loads a bunch of other packages
  ## for some reason
  skip_if(Sys.getenv("R_COVR", "") == "true", "not run in covr")

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

  if_fail(
    expect_true(all(new_pkgs %in% c("pkgman", base_packages()))),
    function(e) print(new_pkgs)
  )
})

test_that("remote", {
  pid <- remote(function() Sys.getpid())
  expect_equal(pid, pkgman_data$remote$get_pid())
  expect_equal(remote(function() 4 + 4), 8)
})
