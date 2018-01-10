
context("utils")

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

test_that("names2", {

  cases <- list(
    list(NULL, character()),
    list(list(), character()),
    list("x", ""),
    list(1:4, rep("", 4)),
    list(c(0, a = 1, b = 2, 3, d = 4), c("", "a", "b", "", "d")),
    list(structure(1:4, names = c("a", NA, "c")), c("a", "", "c", ""))
  )

  for (c in cases) expect_identical(names2(c[[1]]), c[[2]])
})
