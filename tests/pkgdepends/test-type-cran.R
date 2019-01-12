
context("type cran")

test_that("resolve_remote", {

  skip_if_offline()
  skip_on_cran()

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = pkgcache::get_cranlike_metadata_cache())

  res <- asNamespace("pkgcache")$synchronise(
    resolve_remote_cran(parse_remotes("cran::crayon")[[1]], TRUE, conf, cache,
                        dependencies = FALSE))

  expect_true(tibble::is_tibble(res))
  expect_true(all(res$ref == "cran::crayon"))
  expect_true(all(res$type == "cran"))
  expect_true(all(res$direct))
  expect_true(all(res$status == "OK"))
  expect_true(all(res$package == "crayon"))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteType")== "cran"))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteRef") == "cran::crayon"))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteSha") == res$version))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteRepos") == res$mirror))
})

test_that("resolve_remote, multiple", {

  skip_if_offline()
  skip_on_cran()

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = pkgcache::get_cranlike_metadata_cache())

  rem <- parse_remotes(c("cran::crayon", "cran::glue"))
  res <- asNamespace("pkgcache")$synchronise(
    resolve_remote_cran(rem, TRUE, conf, cache, dependencies = FALSE))

  expect_true(tibble::is_tibble(res))
  expect_true(all(res$ref %in% c("cran::crayon",  "cran::glue")))
  expect_true(all(res$type == "cran"))
  expect_true(all(res$direct))
  expect_true(all(res$status == "OK"))
  expect_true(all(res$package %in% c("crayon", "glue")))
})

test_that("failed resolution", {

  skip_if_offline()
  skip_on_cran()

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = pkgcache::get_cranlike_metadata_cache())

  nonpkg <- paste0("cran::", basename(tempfile()))
  res <- asNamespace("pkgcache")$synchronise(
    resolve_remote_cran(parse_remotes(nonpkg)[[1]], TRUE, conf, cache,
                        dependencies = FALSE))

  expect_true(all(res$status == "FAILED"))
  expect_equal(conditionMessage(res$error[[1]]),
               "Cannot find standard package")

  ## Existing package, non-existing version

  skip("TODO")

  r <- remotes()$new(
    "cran::crayon@0.0", config = list(cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_true(all(res$status == "FAILED"))
})

test_that("failed resolution, multiple", {

  skip_if_offline()
  skip_on_cran()

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = pkgcache::get_cranlike_metadata_cache())

  nonpkg <- paste0("cran::", basename(tempfile()))
  rem <- parse_remotes(c(nonpkg, "cran::crayon"))
  res <- asNamespace("pkgcache")$synchronise(
    resolve_remote_cran(rem, TRUE, conf, cache, dependencies = FALSE))

  expect_true("FAILED" %in% res$status)
  err <- res$error[res$ref != "cran::crayon"][[1]]
  expect_equal(conditionMessage(err), "Cannot find standard package")

})

test_that("resolve current version", {
  skip_if_offline()
  skip_on_cran()

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = pkgcache::get_cranlike_metadata_cache())

  do <- function(ref) {
    resolve_remote_cran(parse_remotes(ref)[[1]], TRUE,
                        conf, cache, dependencies = FALSE)
  }

  res <- asNamespace("pkgcache")$synchronise(do("cran::crayon@current"))
  res2 <- asNamespace("pkgcache")$synchronise(do("cran::crayon"))

  expect_true(tibble::is_tibble(res))
  expect_true(all(res$type == "cran"))
  expect_true(all(res$direct))
  expect_true(all(res$status == "OK"))
  expect_true(all(res$package == "crayon"))

  expect_equal(res$version, res2$version)
})

test_that("resolve an old version", {
  skip_if_offline()
  skip_on_cran()
  skip("TODO")

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes()$new(
    "cran::crayon@1.1.0",
    config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_s3_class(res, "remotes_resolution")
  expect_true(all(res$data$ref == "cran::crayon@1.1.0"))
  expect_true(all(res$data$type == "cran"))
  expect_true(all(res$data$direct))
  expect_true(all(res$data$status == "OK"))
  expect_true(all(res$data$package == "crayon"))
  expect_true(all(res$data$version == "1.1.0"))
})

test_that("resolve current version, specified via version number", {
  skip_if_offline()
  skip_on_cran()
  skip("TODO")

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes()$new(
    "cran::crayon@current",
    config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  ver <- res$version[1]

  ref <- paste0("cran::crayon@", ver)
  r2 <- remotes()$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r2$resolve(), NA))
  res2 <- r2$get_resolution()

  expect_true(all(res2$version == ver))
  expect_true(all(res2$status == "OK"))
})

test_that("resolve a version range", {
  skip_if_offline()
  skip_on_cran()
  skip("TODO")

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  r <- remotes()$new(
    "cran::crayon@>=1.3.2",
    config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_s3_class(res, "remotes_resolution")
  expect_true(all(res$ref == "cran::crayon@>=1.3.2"))
  expect_true(all(res$type == "cran"))
  expect_true(all(res$direct))
  expect_true(all(res$status == "OK"))
  expect_true(all(res$package == "crayon"))
  expect_true(all(package_version(res$data$version) >= "1.3.2"))
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

  resolve <- function() {
    resolve_remote_cran(parse_remotes("cran::crayon")[[1]], TRUE, conf, cache,
                        dependencies = FALSE)
  }
  res <- asNamespace("pkgcache")$synchronise(resolve())

  target <- file.path(conf$cache_dir, res$target[1])
  download <- function(res) {
    download_remote_cran(res, target, conf, cache, on_progress = NULL)
  }
  dl1 <- asNamespace("pkgcache")$synchronise(download(res[1,]))
  expect_equal(dl1, "Got")
  expect_true(file.exists(target))

  unlink(target)
  dl2 <- asNamespace("pkgcache")$synchronise(download(res[1,]))
  expect_true(dl2 %in% c("Had", "Current"))
  expect_true(file.exists(target))
})

test_that("satisfies_remote", {

  res <- make_fake_resolution(`cran::crayon@>=1.0.0` = list())

  ## GitHub type is never good
  bad1 <- make_fake_resolution(`github::r-lib/crayon` = list())
  expect_false(ans <- satisfy_remote_cran(res, bad1))
  expect_match(attr(ans, "reason"), "Type must be")

  ## Missing DESCRIPTION for installed type
  bad2 <- make_fake_resolution(`installed::foobar` = list())
  expect_false(ans <- satisfy_remote_cran(res, bad2))
  expect_match(attr(ans, "reason"), "not from CRAN")

  ## installed, but not from CRAN
  fake_desc <- desc::desc("!new")
  fake_desc$set(Repository ="Not CRAN")
  bad3 <- make_fake_resolution(`installed::foobar` = list(
    extra = list(list(description = fake_desc))))
  expect_false(ans <- satisfy_remote_cran(res, bad3))
  expect_match(attr(ans, "reason"), "not from CRAN")

  ## CRAN type, but package name does not match
  bad4 <- make_fake_resolution(`cran::crayon2` = list())
  expect_false(ans <- satisfy_remote_cran(res, bad4))
  expect_match(attr(ans, "reason"), "names differ")

  ## installed type, but package name does not match
  fake_desc <- desc::desc("!new")
  fake_desc$set(Repository ="CRAN")
  bad5 <- make_fake_resolution(`installed::foobar` = list(
    package = "crayon2",
    extra = list(list(description = fake_desc))))
  expect_false(ans <- satisfy_remote_cran(res, bad5))
  expect_match(attr(ans, "reason"), "names differ")

  ## CRAN type, but version is not good enough
  bad6 <- make_fake_resolution(`cran::crayon` = list(version = "0.0.1"))
  expect_false(ans <- satisfy_remote_cran(res, bad6))
  expect_match(attr(ans, "reason"), "Insufficient version")

  ## Same version, CRAN
  ok1 <- make_fake_resolution(`cran::crayon` = list())
  expect_true(satisfy_remote_cran(res, ok1))

  ## Newer version, CRAN
  ok2 <- make_fake_resolution(`cran::crayon` = list(version = "2.0.0"))
  expect_true(satisfy_remote_cran(res, ok2))

  ## Same version, installed
  fake_desc <- desc::desc("!new")
  fake_desc$set(Repository ="CRAN")
  ok3 <- make_fake_resolution(`installed::foobar` = list(
    package = "crayon",
    extra = list(list(description = fake_desc))))
  expect_true(satisfy_remote_cran(res, ok3))

  ## Newer version, installed
  fake_desc <- desc::desc("!new")
  fake_desc$set(Repository ="CRAN")
  ok4 <- make_fake_resolution(`installed::foobar` = list(
    package = "crayon",
    version = "2.0.0",
    extra = list(list(description = fake_desc))))
  expect_true(satisfy_remote_cran(res, ok4))
})
