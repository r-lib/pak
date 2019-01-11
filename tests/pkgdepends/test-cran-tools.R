
context("cran utility functions")

test_that("parse_deps", {

  expect_equal(
    parse_deps(character(), character()),
    list()
  )

  expect_equal(
    parse_deps("", "Imports"),
    list(tibble::tibble(
      type = character(), package = character(), op = character(),
      version = character()
    ))
  )

  expect_equal(
    parse_deps("foobar", "Imports"),
    list(tibble::tibble(
      type = "Imports",
      package = "foobar",
      op = "",
      version = ""
    ))
  )

  expect_equal(
    parse_deps("foobar (>= 1.0-5)", "Imports"),
    list(tibble::tibble(
      type = "Imports",
      package = "foobar",
      op = ">=",
      version = "1.0-5"
    ))
  )

  expect_equal(
    parse_deps("foobar\n (>=\n 1.0-5), foobar2", "Imports"),
    list(tibble::tibble(
      type = rep("Imports", 2),
      package = c("foobar", "foobar2"),
      op = c(">=", ""),
      version = c("1.0-5", "")
    ))
  )
})
