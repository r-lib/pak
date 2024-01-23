test_that("pkg_install", {
  skip_on_cran()
  local <- withr::local_tempdir()
  setup_fake_apps()
  suppressMessages(pkg_install("pkg2", lib = local))
  expect_true(file.exists(file.path(local, "pkg2")))
  # depedency is installed as well
  expect_true(file.exists(file.path(local, "pkg1")))
})

test_that("pkg_status", {
  ps <- pkg_status("stats", lib = .Library)
  expect_equal(ps$priority, "base")

  ps <- pkg_status(c("stats", "utils"), lib = .Library)
  expect_equal(ps$priority, c("base", "base"))
})

test_that("pkg_remove", {
  skip_on_cran()
  local <- withr::local_tempdir()
  setup_fake_apps()
  suppressMessages(pkg_install("pkg2", lib = local))
  expect_true(file.exists(file.path(local, "pkg2")))
  # depedency is installed as well
  expect_true(file.exists(file.path(local, "pkg1")))
  pkg_remove("pkg2", lib = local)
  expect_false(file.exists(file.path(local, "pkg2")))
})

test_that("pkg_deps", {
  skip_on_cran()
  local <- withr::local_tempdir()
  setup_fake_apps()
  suppressMessages(pd <- pkg_deps("pkg2"))
  expect_equal(sort(pd$package), sort(c("pkg1", "pkg2")))
})

test_that("pkg_deps_tree", {
  skip_on_cran()
  local <- withr::local_tempdir()
  setup_fake_apps()
  suppressMessages(pdt <- pkg_deps_tree("pkg2"))
  expect_equal(sort(pdt$package), sort(c("pkg1", "pkg2")))
  expect_snapshot(print(pdt), transform = transform_bytes)
  # [] will drop the tree, keeps the data
  expect_false("pak_pkg_deps_tree" %in% class(pdt[]))
  expect_equal(sort(pdt$package), sort(c("pkg1", "pkg2")))
})

test_that("pkg_list", {
  skip_on_cran()
  local <- withr::local_tempdir()
  setup_fake_apps()
  suppressMessages(pkg_install("pkg2", lib = local))
  pl <- pkg_list(lib = local)
  expect_equal(sort(pl$package), sort(c("pkg1", "pkg2")))
})

test_that("pkg_download", {
  skip_on_cran()
  local <- withr::local_tempdir()
  setup_fake_apps()
  suppressMessages(ret <- pkg_download("pkg2", local, dependencies = TRUE))
  expect_equal(sort(ret$package), sort(c("pkg1", "pkg2")))
  expect_true(all(file.exists(ret$fulltarget)))
})
