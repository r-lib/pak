
context("package integrity")

test_that("is_valid_package, etc", {
  expect_true(is_valid_package(get_fixture("foobar_1.0.0.tar.gz")))
  expect_true(is_valid_package(get_fixture("foobar_1.0.0.zip")))
  expect_false(is_valid_package(get_fixture("foobar_1.0.1.tar.gz")))
  expect_false(is_valid_package(get_fixture("foobar_1.0.2.tar.gz")))
  expect_false(is_valid_package(get_fixture("foobar_1.0.1.zip")))
  expect_false(is_valid_package(get_fixture("foobar_1.0.2.zip")))
})
