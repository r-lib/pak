test_that("lockfile_create, lockfile_install", {
  skip_on_cran()
  dld <- withr::local_tempdir()
  withr::local_options(pkg.sysreqs_platform = "")
  cache_clean()
  setup_fake_apps()

  suppressMessages(dl <- pkg_download("pkg4", dld))
  file.copy(dl$fulltarget, dld)
  pkg <- file.path(dld, basename(dl$fulltarget))
  utils::untar(pkg, exdir = dld)
  withr::local_dir(file.path(dld, "pkg4"))
  lockfile_create(lockfile = "deps.lock")
  expect_snapshot(
    writeLines(readLines("deps.lock")),
    transform = transform_lockfile
  )

  lockfile_create(lockfile = "dev.lock", dependencies = TRUE)
  expect_snapshot(
    writeLines(readLines("dev.lock")),
    transform = transform_lockfile
  )

  lib <- withr::local_tempdir()
  suppressMessages(lockfile_install("deps.lock", lib = lib))
  expect_true(file.exists(file.path(lib, "pkg1")))
  expect_true(file.exists(file.path(lib, "pkg2")))
  expect_false(file.exists(file.path(lib, "pkg3")))
  expect_false(file.exists(file.path(lib, "pkg4")))

  suppressMessages(lockfile_install("dev.lock", lib = lib))
  expect_true(file.exists(file.path(lib, "pkg1")))
  expect_true(file.exists(file.path(lib, "pkg2")))
  expect_true(file.exists(file.path(lib, "pkg3")))
  expect_false(file.exists(file.path(lib, "pkg4")))
})
