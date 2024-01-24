test_that("extra_paalkages", {
  expect_snapshot(extra_packages())
})

test_that("pak_install_extra", {
  pkg <- NULL
  mockery::stub(
    pak_install_extra,
    "pak::pkg_install",
    function(x, ...) pkg <<- x
  )
  expect_snapshot(pak_install_extra())
  expect_equal(pkg, extra_packages())
})

test_that("load_extra", {
  mockery::stub(load_extra, "requireNamespace", FALSE)
  mockery::stub(load_extra, "show_extra", TRUE)
  mockery::stub(load_extra, "once_per_session", function(x) x)
  expect_snapshot(load_extra("foobar"))

  mockery::stub(load_extra, "show_extra", FALSE)
  expect_snapshot(load_extra("foobar"))

  mockery::stub(load_extra, "show_extra", TRUE)
  mockery::stub(load_extra, "once_per_session", function(x) NULL)
  expect_snapshot(load_extra("foobar"))
})

test_that("show_extra", {
  withr::local_envvar("CI" = "true")
  expect_false(show_extra())

  withr::local_envvar("CI" = NA_character_)
  withr::local_options(pak.no_extra_messages = TRUE)
  expect_false(show_extra())

  withr::local_envvar("CI" = NA_character_)
  withr::local_options(pak.no_extra_messages = NULL)
  expect_true(show_extra())
})

test_that("hash", {
  # hash differes on different locale, R version, so mild check
  h <- hash(1:10)
  expect_match(h, "^[0-9a-f]{32}$", perl = TRUE)
  expect_equal(hash(1:10), h)
})

test_that("once_per_session", {
  environment(once_per_session)$seen <- character()
  expect_equal(once_per_session(1L + 1L + 1L), 3L)
  expect_null(once_per_session(1L + 1L + 1L))
})

test_that("pkg_is_installed", {
  rpkg <- basename(tempfile())
  expect_equal(
    pkg_is_installed(c("pak", "stats", rpkg)),
    structure(c(TRUE, TRUE, FALSE), names = c("pak", "stats", rpkg))
  )
})
