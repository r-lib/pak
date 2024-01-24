test_that("handle_package_not_found", {
  mockery::stub(handle_package_not_found, "is_interactive", FALSE)
  expect_silent(handle_package_not_found())

  mockery::stub(handle_package_not_found, "is_interactive", TRUE)
  mockery::stub(handle_package_not_found, "sink.number", 1)
  expect_silent(handle_package_not_found())

  mockery::stub(handle_package_not_found, "sink.number", 0)
  mockery::stub(handle_package_not_found, "findRestart", NULL)
  mockery::stub(handle_package_not_found, "get_answer", "2")
  expect_snapshot(
    handle_package_not_found(err = list(package = "foo", lib.loc = "/lib"))
  )

  mockery::stub(handle_package_not_found, "get_answer", "1")
  mockery::stub(
    handle_package_not_found,
    "pkg_install",
    function(x, ...) {
      message("Installing...")
      pkg <<- x
    }
  )
  pkg <- NULL
  expect_snapshot(
    handle_package_not_found(err = list(package = "foo", lib.loc = "/lib"))
  )
  expect_equal(pkg, "foo")

  mockery::stub(handle_package_not_found, "findRestart", "foo")
  mockery::stub(
    handle_package_not_found,
    "invokeRestart",
    function(...) restart <<- TRUE
  )
  restart <- NULL
  expect_snapshot(
    handle_package_not_found(err = list(package = "foo", lib.loc = "/lib"))
  )
  expect_true(restart)
})
