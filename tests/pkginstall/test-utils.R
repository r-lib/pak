
context("utils")

test_that("warn", {
  foo <- "bar"
  expect_warning(
    warn("this is {foo}"),
    "this is 'bar'"
  )
})
