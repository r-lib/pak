
context("local ref type")

test_that("parse_remote", {
  path <- get_fixture("foobar_1.0.0.tar.gz")
  ref <- paste0("local::", path)
  pr <- parse_remotes(ref)[[1]]
  expect_true(is.list(pr))
  expect_equal(pr$path, path)
  expect_equal(pr$ref, ref)
  expect_equal(pr$type, "local")
})

test_that("resolve_remote", {

  skip_if_offline()
  skip_on_cran()

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = pkgcache::get_cranlike_metadata_cache())

  ## Absolute path
  path <- get_fixture("foobar_1.0.0.tar.gz")
  ref <- paste0("local::", path)
  res <- asNamespace("pkgcache")$synchronise(
    resolve_remote_local(parse_remotes(ref)[[1]], TRUE, conf,
                         cache, dependencies = FALSE)
  )

  expect_true(is.list(res))
  expect_true(res$ref == ref)
  expect_true(res$type == "local")
  expect_true(res$direct)
  expect_true(res$status == "OK")
  expect_true(res$package == "foobar")
  expect_true(res$version == "1.0.0")
  expect_equal(res$unknown_deps, character(0))
  expect_equal(res$sources[[1]], paste0("file://", normalizePath(path)))
  expect_equal(res$metadata[["RemoteType"]], "local")
  expect_equal(res$metadata[["RemoteRef"]], ref)

  ## Relative path?
  fix_dir <- fixture_dir()
  ref2 <- paste0("local::", "foobar_1.0.0.tar.gz")
  withr::with_dir(
    fix_dir,
    res <- asNamespace("pkgcache")$synchronise(
      resolve_remote_local(parse_remotes(ref2)[[1]], TRUE, conf,
                         cache, dependencies = FALSE)
    )
  )

  expect_true(is.list(res))
  expect_true(res$ref == ref2)
  expect_true(res$type == "local")
  expect_true(res$direct)
  expect_true(res$status == "OK")
  expect_true(res$package == "foobar")
  expect_true(res$version == "1.0.0")
  expect_equal(res$sources[[1]], paste0("file://", normalizePath(path)))
  expect_equal(res$metadata[["RemoteType"]], "local")
  expect_equal(res$metadata[["RemoteRef"]], ref2)
})

test_that("resolution error", {

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = pkgcache::get_cranlike_metadata_cache())

  path <- get_fixture("foobar_10.0.0.tar.gz")
  ref <- paste0("local::", path)
  err <- tryCatch(asNamespace("pkgcache")$synchronise(
    resolve_remote_local(parse_remotes(ref)[[1]], TRUE, conf,
                         cache, dependencies = FALSE)
  ), error = function(x) x)

  expect_s3_class(err, "error")
})

test_that("download_remote", {
  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  dir.create(tmp2 <- tempfile())
  on.exit(unlink(c(tmp, tmp2), recursive = TRUE), add = TRUE)

  conf <- remotes_default_config()
  conf$platforms <- "macos"
  conf$cache_dir <- tmp
  conf$package_cache_dir <- tmp2
  cache <- list(
    package = pkgcache::package_cache$new(conf$package_cache_dir),
    metadata = pkgcache::get_cranlike_metadata_cache())

  ## Absolute path
  path <- get_fixture("foobar_1.0.0.tar.gz")
  ref <- paste0("local::", path)

  rem <- remotes()$new(ref)
  rem$resolve()
  res <- rem$get_resolution()

  target <- file.path(conf$cache_dir, res$target[1])
  mkdirp(dirname(target))
  download <- function(res) {
    download_remote_local(res, target, conf, cache, on_progress = NULL)
  }
  dl1 <- asNamespace("pkgcache")$synchronise(download(res[1,]))
  expect_equal(dl1, "Had")
  expect_true(file.exists(target))

  ## Relative path?
  fix_dir <- fixture_dir()
  ref2 <- paste0("local::", "foobar_1.0.0.tar.gz")

  withr::with_dir(fix_dir, {
    rem <- remotes()$new(ref2)
    rem$resolve()
    res <- rem$get_resolution()
  })

  target <- file.path(conf$cache_dir, res$target[1])
  mkdirp(dirname(target))
  dl1 <- download_remote_local(res, target, conf, cache, on_progress = NULL)

  expect_equal(dl1, "Had")
  expect_true(file.exists(target))
})

test_that("download_remote error", {
  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp2 <- tempfile())
  on.exit(unlink(tmp2, recursive = TRUE), add = TRUE)

  path <- get_fixture("foobar_1.0.0.tar.gz")
  file.copy(path, tmp2)
  ref <- paste0("local::", path2 <- file.path(tmp2, basename(path)))
  r <- remotes()$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))
  expect_error(r$resolve(), NA)
  unlink(path2)
  expect_error(r$download_resolution(), NA)
  dl <- r$get_resolution_download()

  expect_false(file.exists(dl$fulltarget))
  expect_s3_class(dl, "remotes_downloads")
  expect_true(all(dl$ref == ref))
  expect_true(all(dl$type == "local"))
  expect_true(all(dl$direct))
  expect_true(all(dl$status == "OK"))
  expect_true(all(dl$package == "foobar"))
  expect_true(all(dl$download_status == "Failed"))
})

test_that("satisfy", {
  ## Always FALSE, independently of arguments
  expect_false(satisfy_remote_local())
})
