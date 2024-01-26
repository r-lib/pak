test_that("pak_cleanup", {
  mockery::stub(pak_cleanup, "interactive", FALSE)
  expect_error(pak_cleanup(), "Refused to clean up")

  mockery::stub(pak_cleanup, "pak_cleanup_package_cache", NULL)
  mockery::stub(pak_cleanup, "pak_cleanup_metadata_cache", NULL)
  expect_silent(pak_cleanup(force = TRUE))

  mockery::stub(pak_cleanup, "interactive", TRUE)
  expect_silent(pak_cleanup())
})

test_that("pak_cleanup_package_cache", {
  cache_clean()
  mockery::stub(pak_cleanup_package_cache, "get_confirmation2", FALSE)
  expect_snapshot(
    pak_cleanup_package_cache(force = FALSE),
    transform = function(x) sub("in '.*'", "in '<cache-path>'", x)
  )

  cache <- cache_summary()[["cachepath"]]
  root <- dirname(cache)
  mockery::stub(pak_cleanup_package_cache, "get_confirmation2", TRUE)
  expect_snapshot(pak_cleanup_package_cache(force = TRUE))
  expect_false(file.exists(cache))
  expect_true(file.exists(root))
})

test_that("pak_cleanup_metadata_cache", {
  suppressMessages(meta_clean(force = TRUE))
  mockery::stub(pak_cleanup_metadata_cache, "get_confirmation2", FALSE)
  expect_snapshot(
    pak_cleanup_metadata_cache(force = FALSE),
    transform = function(x) sub("in '.*'", "in '<cache-path>'", x)
  )

  cache <- meta_summary()[["cachepath"]]
  root <- dirname(cache)
  mockery::stub(pak_cleanup_metadata_cache, "get_confirmation2", TRUE)
  expect_snapshot(pak_cleanup_metadata_cache(force = TRUE))
  expect_false(file.exists(cache))
  expect_true(file.exists(root))
})
