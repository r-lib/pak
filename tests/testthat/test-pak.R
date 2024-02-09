test_that("pak", {
  mode <- NULL
  mockery::stub(
    pak,
    "local_install_dev_deps",
    function(...) mode <<- list("li", ...)
  )
  mockery::stub(
    pak,
    "pkg_install",
    function(...) mode <<- list("pi", ...)
  )
  pak()
  expect_equal(mode, list("li"))
  pak(arg = "arg")
  expect_equal(mode, list("li", arg = "arg"))
  pak("pkg1")
  expect_equal(mode, list("pi", "pkg1"))
  pak("pkg1", arg = "arg")
  expect_equal(mode, list("pi", "pkg1", arg = "arg"))
})
