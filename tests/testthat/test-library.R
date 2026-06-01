test_that("lib_default() returns first library path by default", {
  skip_on_cran()
  lib1 <- normalizePath(test_temp_dir(), winslash = "/")
  lib2 <- normalizePath(test_temp_dir(), winslash = "/")
  withr::local_envvar(c(PKG_LIBRARY = NA, PKG_IGNORE_DEV_LIBRARY = NA))
  withr::local_options(list(pkg.library = NULL, pkg.ignore_dev_library = NULL))
  expect_equal(
    normalizePath(lib_default(c(lib1, lib2, .libPaths())), winslash = "/"),
    lib1
  )
})

test_that("lib_default() honors PKG_LIBRARY env var", {
  skip_on_cran()
  custom <- normalizePath(test_temp_dir(), winslash = "/")
  other <- normalizePath(test_temp_dir(), winslash = "/")
  withr::local_envvar(c(PKG_LIBRARY = custom, PKG_IGNORE_DEV_LIBRARY = NA))
  withr::local_options(list(pkg.library = NULL, pkg.ignore_dev_library = NULL))
  expect_equal(
    normalizePath(lib_default(c(other, .libPaths())), winslash = "/"),
    custom
  )
})

test_that("lib_default() honors pkg.library option", {
  skip_on_cran()
  custom <- normalizePath(test_temp_dir(), winslash = "/")
  other <- normalizePath(test_temp_dir(), winslash = "/")
  withr::local_envvar(c(PKG_LIBRARY = NA, PKG_IGNORE_DEV_LIBRARY = NA))
  withr::local_options(list(
    pkg.library = custom,
    pkg.ignore_dev_library = NULL
  ))
  expect_equal(
    normalizePath(lib_default(c(other, .libPaths())), winslash = "/"),
    custom
  )
})

test_that("lib_default() ignores __dev_lib__ libraries by default", {
  skip_on_cran()
  dev_parent <- test_temp_dir()
  dev_lib <- file.path(dev_parent, "__dev_lib__")
  mkdirp(dev_lib)
  dev_lib <- normalizePath(dev_lib, winslash = "/")
  reg_lib <- normalizePath(test_temp_dir(), winslash = "/")
  withr::local_envvar(c(PKG_LIBRARY = NA, PKG_IGNORE_DEV_LIBRARY = NA))
  withr::local_options(list(pkg.library = NULL, pkg.ignore_dev_library = NULL))
  expect_equal(
    normalizePath(
      lib_default(c(dev_lib, reg_lib, .libPaths())),
      winslash = "/"
    ),
    reg_lib
  )
})

test_that("lib_default() keeps __dev_lib__ with PKG_IGNORE_DEV_LIBRARY=false", {
  skip_on_cran()
  dev_parent <- test_temp_dir()
  dev_lib <- file.path(dev_parent, "__dev_lib__")
  mkdirp(dev_lib)
  dev_lib <- normalizePath(dev_lib, winslash = "/")
  reg_lib <- normalizePath(test_temp_dir(), winslash = "/")
  withr::local_envvar(c(PKG_LIBRARY = NA, PKG_IGNORE_DEV_LIBRARY = "false"))
  withr::local_options(list(pkg.library = NULL, pkg.ignore_dev_library = NULL))
  expect_equal(
    normalizePath(
      lib_default(c(dev_lib, reg_lib, .libPaths())),
      winslash = "/"
    ),
    dev_lib
  )
})

test_that("lib_default() keeps __dev_lib__ with pkg.ignore_dev_library=FALSE", {
  skip_on_cran()
  dev_parent <- test_temp_dir()
  dev_lib <- file.path(dev_parent, "__dev_lib__")
  mkdirp(dev_lib)
  dev_lib <- normalizePath(dev_lib, winslash = "/")
  reg_lib <- normalizePath(test_temp_dir(), winslash = "/")
  withr::local_envvar(c(PKG_LIBRARY = NA, PKG_IGNORE_DEV_LIBRARY = NA))
  withr::local_options(list(pkg.library = NULL, pkg.ignore_dev_library = FALSE))
  expect_equal(
    normalizePath(
      lib_default(c(dev_lib, reg_lib, .libPaths())),
      winslash = "/"
    ),
    dev_lib
  )
})
