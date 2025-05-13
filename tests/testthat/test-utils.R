test_that("merge_new", {
  expect_identical(merge_new("x", "y"), "y")
  expect_identical(merge_new("x", "y", "replace"), "y")
  expect_identical(merge_new("x", "y", "prepend"), c("y", "x"))
  expect_identical(merge_new("x", "y", "append"), c("x", "y"))
  expect_error(merge_new("x", "y", "foobar"))

  ## Some special values
  expect_identical(merge_new("x", NULL), NULL)
  expect_identical(merge_new(NULL, "x"), "x")
  expect_identical(merge_new("x", NULL, "append"), "x")
  expect_identical(merge_new(NULL, "x", "append"), "x")
  expect_identical(merge_new("x", NULL, "prepend"), "x")
  expect_identical(merge_new(NULL, "x", "prepend"), "x")
})
