test_that("local_install", {
  skip_on_cran()
  lib <- withr::local_tempdir()
  dld <- withr::local_tempdir()
  cache_clean()
  setup_fake_apps()

  suppressMessages(dl <- pkg_download("pkg3", dld))
  file.copy(dl$fulltarget, dld)
  pkg <- file.path(dld, basename(dl$fulltarget))
  utils::untar(pkg, exdir = dld)
  suppressMessages(local_install(file.path(dld, "pkg3"), lib = lib))
  expect_true(file.exists(file.path(lib, "pkg1")))
  expect_true(file.exists(file.path(lib, "pkg2")))
  expect_true(file.exists(file.path(lib, "pkg3")))
})

test_that("local_install_deps", {
  skip_on_cran()
  lib <- withr::local_tempdir()
  dld <- withr::local_tempdir()
  cache_clean()
  setup_fake_apps()

  suppressMessages(dl <- pkg_download("pkg4", dld))
  file.copy(dl$fulltarget, dld)
  pkg <- file.path(dld, basename(dl$fulltarget))
  utils::untar(pkg, exdir = dld)
  suppressMessages(local_install_deps(
    file.path(dld, "pkg4"),
    lib = lib
  ))
  expect_true(file.exists(file.path(lib, "pkg1")))
  expect_true(file.exists(file.path(lib, "pkg2")))
  expect_false(file.exists(file.path(lib, "pkg3")))
  expect_false(file.exists(file.path(lib, "pkg4")))
})

test_that("local_install_dev_deps", {
  skip_on_cran()
  lib <- withr::local_tempdir()
  dld <- withr::local_tempdir()
  cache_clean()
  setup_fake_apps()

  suppressMessages(dl <- pkg_download("pkg4", dld))
  file.copy(dl$fulltarget, dld)
  pkg <- file.path(dld, basename(dl$fulltarget))
  utils::untar(pkg, exdir = dld)
  suppressMessages(local_install_dev_deps(
    file.path(dld, "pkg4"),
    lib = lib
  ))
  expect_true(file.exists(file.path(lib, "pkg1")))
  expect_true(file.exists(file.path(lib, "pkg2")))
  expect_true(file.exists(file.path(lib, "pkg3")))
  expect_false(file.exists(file.path(lib, "pkg4")))
})

test_that("local_deps & co", {
  skip_on_cran()
  lib <- withr::local_tempdir()
  dld <- withr::local_tempdir()
  cache_clean()
  setup_fake_apps()

  suppressMessages(dl <- pkg_download("pkg4", dld))
  file.copy(dl$fulltarget, dld)
  pkg <- file.path(dld, basename(dl$fulltarget))
  utils::untar(pkg, exdir = dld)
  expect_snapshot(local_deps(file.path(dld, "pkg4"))$package)
  expect_snapshot(local_dev_deps(file.path(dld, "pkg4"))$package)

  # otherwise long temp dir might be cut off
  withr::local_options(cli.width = 500)
  expect_snapshot(
    local_deps_tree(file.path(dld, "pkg4")),
    transform = function(x) transform_tempdir(transform_bytes(x))
  )
  expect_snapshot(
    local_dev_deps_tree(file.path(dld, "pkg4")),
    transform = function(x) transform_tempdir(transform_bytes(x))
  )
  expect_snapshot(local_deps_explain("pkg1", file.path(dld, "pkg4")))
  expect_snapshot(local_dev_deps_explain("pkg3", file.path(dld, "pkg4")))
})
