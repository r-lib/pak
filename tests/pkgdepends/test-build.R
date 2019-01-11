
context("build")

test_that("build_package", {
  args <- NULL
  mockery::stub(build_package, "pkgbuild::build", function(...) args <<- list(...))
  build_package(tmp <- tempfile())
  expect_equal(args$path, tmp)
})
