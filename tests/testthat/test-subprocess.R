
test_that("no dependencies are loaded with pak", {

  skip_on_cran()

  ## Skip this is covr, because covr loads a bunch of other packages
  ## for some reason
  skip_if(Sys.getenv("R_COVR", "") == "true", "not run in covr")

  new_pkgs <- callr::r(
    function() {
      withr::with_options(list(pkg.subprocess = FALSE), {
        orig <- loadedNamespaces()
        library(pak)
        new <- loadedNamespaces()
      })
      setdiff(new, orig)
    },
    timeout = 5
  )

  if_fail(
    expect_true(all(new_pkgs %in% c("pak", "rstudioapi", base_packages()))),
    function(e) print(new_pkgs)
  )
})

test_that("remote", {
  pid <- remote(function() Sys.getpid())
  expect_equal(pid, pkg_data$remote$get_pid())
  expect_equal(remote(function() 4 + 4), 8)
})

test_that("remote messages", {
  skip_on_cran()
  expect_snapshot(
    invisible(remote(function() cli::cli_alert_info("just once")))
  )
  expect_snapshot(
    withCallingHandlers(
      invisible(remote(function() cli::cli_alert_info("just once"))),
      message = function(m) print(m)
    )
  )
})
