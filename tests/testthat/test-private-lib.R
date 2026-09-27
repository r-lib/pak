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

test_that("R does not crash at exit after processx and parallel forks", {
  # parallel is loaded first, so its exit finalizer runs after pak's.
  # It reinstalls processx's SIGCHLD handler, which must still be mapped.
  skip_on_cran()
  skip_on_os("windows")
  code <- c(
    if (Sys.getenv("_R_CHECK_PACKAGE_NAME_") == "") {
      sprintf("pkgload::load_all(%s, quiet = TRUE)", deparse(find_package_root()))
    },
    "loadNamespace('parallel')",
    "pak:::load_private_package('processx', 'c_')",
    "p <- pak:::pkg_data$ns$processx$process$new('true'); p$wait()",
    "j <- parallel::mcparallel(1); invisible(parallel::mccollect(j))"
  )
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(code, script)
  status <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    shQuote(script),
    stdout = FALSE,
    stderr = FALSE
  ))
  expect_equal(status, 0L)
})
