
test_that("loading package from private lib", {
  skip_on_cran()

  do <- function() {
    pak <- asNamespace("pak")
    pak$load_private_package("processx", "c_")
    pkgdir <- normalizePath(pak$pkg_data$ns$processx[["__pkg-dir__"]])
    paths <- normalizePath(sapply(.dynLibs(), "[[", "path"))
    list(
      pkgdir = file.exists(pkgdir),
      isfun = is.function(pak$pkg_data$ns$processx$run),
      dyn = any(grepl(basename(pkgdir), paths, fixed = TRUE)),
      dd = pkgdir,
      pp = paths
    )
  }

  ret <- callr::r(do, env = c(callr::rcmd_safe_env(), R_ENABLE_JIT = "0"))
  expect_true(ret$pkgdir)
  expect_true(ret$isfun)
  expect_true(ret$dyn)
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
  gc(); gc()

  expect_false(file.exists(pkgdir))
  paths <- sapply(.dynLibs(), "[[", "path")
  expect_false(any(grepl(pkgdir, paths, fixed = TRUE)))
})

test_that("no interference", {
  skip_on_cran()

  do <- function() {
    pak <- asNamespace("pak")
    asNamespace("ps")
    p1 <- "ps" %in%  loadedNamespaces()
    p2 <- "ps" %in% sapply(.dynLibs(), "[[", "name")

    pak$load_private_package("ps")
    p3 <- is.function(pak$pkg_data$ns$ps$ps)
    p4 <- is.function(asNamespace("ps")$ps)

    ns <- pak$pkg_data$ns
    ns$ps <- NULL
    gc(); gc()

    p5 <- "ps" %in% loadedNamespaces()
    p6 <- "ps" %in% sapply(.dynLibs(), "[[", "name")
    list(p1, p2, p3, p4, p5, p6)
  }

  ret <- callr::r(do, env = c(callr::rcmd_safe_env(), R_ENABLE_JIT = "0"))
  expect_true(ret[[1]])
  expect_true(ret[[2]])
  expect_true(ret[[3]])
  expect_true(ret[[4]])
  expect_true(ret[[5]])
  expect_true(ret[[6]])
})
