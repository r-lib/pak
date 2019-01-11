
context("standard ref type")

test_that("resolve_remote", {

  skip_if_offline()
  skip_on_cran()

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = pkgcache::get_cranlike_metadata_cache())

  ## CRAN package is found
  res <- asNamespace("pkgcache")$synchronise(
    resolve_remote_cran(parse_remotes("crayon")[[1]], TRUE, conf, cache,
                        dependencies = FALSE))

  expect_true(tibble::is_tibble(res))
  expect_true(all(res$ref == "crayon"))
  expect_true(all(res$type == "standard"))
  expect_true(all(res$direct))
  expect_true(all(res$status == "OK"))
  expect_true(all(res$package == "crayon"))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteType")== "standard"))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteRef") == "crayon"))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteSha") == res$version))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteRepos") == res$mirror))

  ## BioC package is found
  res <- asNamespace("pkgcache")$synchronise(
    resolve_remote_cran(parse_remotes("Biobase")[[1]], TRUE, conf, cache,
                        dependencies = FALSE))

  expect_true(tibble::is_tibble(res))
  expect_true(all(res$ref == "Biobase"))
  expect_true(all(res$type == "standard"))
  expect_true(all(res$direct))
  expect_true(all(res$status == "OK"))
  expect_true(all(res$package == "Biobase"))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteType")== "standard"))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteRef") == "Biobase"))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteSha") == res$version))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteRepos") == res$mirror))

  ## Proper error for non-existing package
  nonpkg <- basename(tempfile())
  res <- asNamespace("pkgcache")$synchronise(
    resolve_remote_cran(parse_remotes(nonpkg)[[1]], TRUE, conf, cache,
                        dependencies = FALSE))

  expect_true(all(res$status == "FAILED"))
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

  res <- asNamespace("pkgcache")$synchronise(
    resolve_remote_bioc(parse_remotes("crayon")[[1]], TRUE, conf, cache,
                        dependencies = FALSE))

  target <- file.path(conf$cache_dir, res$target[1])
  dl <- asNamespace("pkgcache")$synchronise(
    download_remote_bioc(res[1,], target, conf, cache, on_progress = NULL))

  expect_equal(dl, "Got")
  expect_true(file.exists(target))

  unlink(target)
  dl2 <- asNamespace("pkgcache")$synchronise(
    download_remote_bioc(res[1,], target, conf, cache, on_progress = NULL))
  expect_true(dl2 %in% c("Had", "Current"))
  expect_true(file.exists(target))
})

test_that("satisfy_remote", {

  res <- make_fake_resolution(`crayon@>=1.0.0` = list(type = "standard"))

  ## Package names differ
  bad1 <- make_fake_resolution(`crayon2` = list())
  expect_false(ans <- satisfy_remote_standard(res, bad1))
  expect_match(attr(ans, "reason"),  "Package names differ")

  ## Insufficient version
  bad2 <- make_fake_resolution(`crayon` = list(version = "0.0.1"))
  expect_false(ans <- satisfy_remote_standard(res, bad2))
  expect_match(attr(ans, "reason"),  "Insufficient version")

  ## Version is OK
  ok1 <- make_fake_resolution(`local::foobar` = list(package = "crayon"))
  expect_true(satisfy_remote_standard(res, ok1))

  ## No version req
  res <- make_fake_resolution(`crayon` = list(type = "standard"))
  ok2 <- make_fake_resolution(`local::foobar` = list(
    package = "crayon", version = "0.0.1"))
  expect_true(satisfy_remote_standard(res, ok2))
})
