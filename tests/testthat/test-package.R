
context("package")

test_that("total_num_deps", {
  ## linear chain
  plan <- tibble::tibble(
    package = c("a", "b", "c"),
    dependencies = list("b", "c", character()))
  expect_identical(total_num_deps(plan), c(3, 2, 1))

  ## loops
  plan <- tibble::tibble(
    package = c("a", "b", "c"),
    dependencies = list("b", "c", "a"))
  expect_identical(total_num_deps(plan), c(3, 3, 3))

  ## empty
  plan <- tibble::tibble(
    package = character(),
    dependencies = list())
  expect_identical(total_num_deps(plan), numeric())
})
