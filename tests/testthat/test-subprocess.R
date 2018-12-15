
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
