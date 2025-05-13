test_that("loading package from private lib", {
  skip_on_cran()
  on.exit(pkg_data$ns <- list(), add = TRUE)
  pkg_data$ns$processx <- NULL
  gc()

  ## Load
  load_private_package("processx", "c_")
  pkgdir <- normalizePath(pkg_data$ns$processx[["__pkg-dir__"]])

  ## Check if loaded
  expect_true(is.function(pkg_data$ns$processx$run))
  expect_true(file.exists(pkgdir))
  paths <- normalizePath(sapply(.dynLibs(), "[[", "path"))
  expect_true(any(grepl(pkgdir, paths, fixed = TRUE)))
})

test_that("cleanup of temp files", {
  skip("cleanup not working")
  skip_on_cran()
  on.exit(pkg_data$ns <- list(), add = TRUE)
  pkg_data$ns$processx <- NULL
  gc()

  ## Load
  load_private_package("processx", "c_")
  pkgdir <- normalizePath(pkg_data$ns$processx[["__pkg-dir__"]])

  ## Check if loaded
  expect_true(is.function(pkg_data$ns$processx$run))
  expect_true(file.exists(pkgdir))
  paths <- normalizePath(sapply(.dynLibs(), "[[", "path"))
  expect_true(any(grepl(pkgdir, paths, fixed = TRUE)))

  pkg_data <- asNamespace("pak")$pkg_data
  pkg_data$ns$processx <- NULL
  gc()
  gc()

  expect_false(file.exists(pkgdir))
  paths <- sapply(.dynLibs(), "[[", "path")
  expect_false(any(grepl(pkgdir, paths, fixed = TRUE)))
})

test_that("no interference", {
  skip_on_cran()
  on.exit(pkg_data$ns <- list(), add = TRUE)
  pkg_data$ns$processx <- NULL
  gc()

  asNamespace("ps")
  expect_true("ps" %in% loadedNamespaces())
  expect_true("ps" %in% sapply(.dynLibs(), "[[", "name"))

  load_private_package("ps")
  expect_true(is.function(pkg_data$ns$ps$ps))
  expect_true(is.function(asNamespace("ps")$ps))

  pkg_data$ns$ps <- NULL
  gc()
  gc()

  expect_true("ps" %in% loadedNamespaces())
  expect_true("ps" %in% sapply(.dynLibs(), "[[", "name"))
})
