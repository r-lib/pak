test_that("cache_summary", {
  cs <- cache_summary()
  expect_equal(names(cs), c("cachepath", "files", "size"))
  expect_true(is_string(cs[["cachepath"]]))
  expect_true(is_count(cs[["files"]]))
  expect_true(is_count(cs[["size"]]))
})

test_that("cache_list, cache_delete", {
  skip_on_cran()
  cache_clean()
  cl <- cache_list()
  expect_equal(class(cl), c("tbl", "data.frame"))
  expect_equal(nrow(cl), 0L)

  local <- withr::local_tempdir()
  setup_fake_apps()
  suppressMessages(pkg_download("pkg2", local, dependencies = TRUE))
  cl <- cache_list()
  expect_equal(sort(cl$package), sort(c("pkg1", "pkg2")))
  expect_true(all(file.exists(cl$fullpath)))

  cache_delete(package = "pkg1")
  cl <- cache_list()
  expect_equal(cl$package, "pkg2")

  cache_clean()
  cl <- cache_list()
  expect_equal(nrow(cl), 0L)
})

test_that("meta_list, meta_summary, meta_update", {
  skip_on_cran()
  suppressMessages(meta_clean(force = TRUE))
  ms <- meta_summary()
  expect_equal(
    names(ms),
    c("cachepath", "current_db", "raw_files", "db_files", "size")
  )
  expect_equal(ms$size, 0L)

  local <- withr::local_tempdir()
  setup_fake_apps()
  ml <- meta_list()
  expect_equal(class(ml), c("tbl", "data.frame"))
  expect_true("pkg3" %in% ml$package)

  repo <- dcf("
    Package: pkgx
    Version: 1.0.0
  ")
  setup_fake_apps(cran_repo = repo)
  suppressMessages(meta_update())
  ml <- meta_list()
  expect_true("pkgx" %in% ml$package)
  expect_false("pkg3" %in% ml$package)
})

test_that("meta_clean confirmation", {
  mockery::stub(meta_clean, "get_confirmation2", FALSE)
  expect_error(meta_clean(), "aborted")
})
