
context("assertions")

test_that("is_character", {
  pos <- list("", "NA", "foobar", character(), letters, c(a = "b"))
  neg <- list(1, 1L, NA, NA_character_, c("x", NA_character_), NULL)
  for (p in pos) expect_true(is_character(p))
  for (n in neg) expect_false(is_character(n))
})

test_that("is_string", {
  pos <- list("", "x", "NA", "foobar", c(a = "b"))
  neg <- list(1, 1L, 1:10, NA, NA_character_, letters, letters[1:2],
              character(), NULL)
  for (p in pos) expect_true(is_string(p))
  for (n in neg) expect_false(is_string(n))
})

test_that("is_string_or_null", {
  pos <- list("", "x", "NA", "foobar", c(a = "b"), NULL)
  neg <- list(1, 1L, 1:10, NA, NA_character_, letters, letters[1:2],
              character())
  for (p in pos) expect_true(is_string_or_null(p))
  for (n in neg) expect_false(is_string_or_null(n))
})

test_that("is_path", {
  pos <- list("", "x", "NA", "foobar", c(a = "b"))
  neg <- list(1, 1L, 1:10, NA, NA_character_, letters, letters[1:2],
              character(), NULL)
  for (p in pos) expect_true(is_path(p))
  for (n in neg) expect_false(is_path(n))
})

test_that("is_path_or_null", {
  pos <- list("", "x", "NA", "foobar", c(a = "b"), NULL)
  neg <- list(1, 1L, 1:10, NA, NA_character_, letters, letters[1:2],
              character())
  for (p in pos) expect_true(is_path_or_null(p))
  for (n in neg) expect_false(is_path_or_null(n))
})

test_that("all_named", {
  pos <- list(character(), list(), c(a = "b"), c(a = 1, b = 2), NULL)
  neg <- list(1, 1L, 1:10, NA, c(a = 1, 2), list(a = 1, 1:5))
  for (p in pos) expect_true(all_named(p))
  for (n in neg) expect_false(all_named(n))
})

test_that("is_existing_file", {
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  expect_false(is_existing_file(tmp))
  expect_false(is_existing_file(file.path(tmp, "foo")))

  cat("foo\n", file = file.path(tmp, "foo"))
  expect_true(is_existing_file(file.path(tmp, "foo")))
})

test_that("is_platform_list", {
  pos <- list("", "NA", "foobar", letters, c(a = "b"))
  neg <- list(1, 1L, NA, NA_character_, c("x", NA_character_), NULL,
              character())
  for (p in pos) expect_true(is_platform_list(p))
  for (n in neg) expect_false(is_platform_list(n))
})

test_that("is_dependencies", {
  pos <- list(TRUE, FALSE, NA, NA_character_, character(),
              "Depends", c("Depends", "Imports"), dep_types())
  neg <- list(1, 1:5, "foo", c("Depends", NA), "linkingto")
  for (p in pos) expect_true(is_dependencies(p))
  for (n in neg) expect_false(is_dependencies(n))
})

test_that("is_r_version_list", {
  pos <- list("1.2.3", c("3.2-2", "3.4"))
  neg <- list(character(), "foobar", NULL, 1, 1:5, c("1.4.5", NA),
              c("1.4.5", "foobar"))
  for (p in pos) expect_true(is_r_version_list(p), info = p)
  for (n in neg) expect_false(is_r_version_list(n), info = n)
})
