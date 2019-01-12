
context("type bioc")

test_that("parse_remote", {

  pr <- parse_remotes("bioc::Biobase")[[1]]
  expect_equal(pr$package, "Biobase")
  expect_equal(pr$atleast, "")
  expect_equal(pr$version, "")
  expect_equal(pr$ref, "bioc::Biobase")
  expect_equal(pr$type, "bioc")
  expect_true("remote_ref_bioc" %in% class(pr))
  expect_true("remote_ref" %in% class(pr))
})

test_that("resolve_remote", {

  skip_if_offline()
  skip_on_cran()

  conf <- remotes_default_config()
  cache <- list(package = NULL,
                metadata = pkgcache::get_cranlike_metadata_cache())

  res <- asNamespace("pkgcache")$synchronise(
    resolve_remote_bioc(parse_remotes("bioc::Biobase")[[1]], TRUE, conf,
                        cache, dependencies = FALSE)
  )

  expect_true(tibble::is_tibble(res))
  expect_true(all(res$ref == "bioc::Biobase"))
  expect_true(all(res$type == "bioc"))
  expect_true(all(res$direct))
  expect_true(all(res$status == "OK"))
  expect_true(all(res$package == "Biobase"))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteType")== "bioc"))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteRef") == "bioc::Biobase"))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteSha") == res$version))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteRepos") == res$mirror))
})

test_that("failed resolution", {

  skip_if_offline()
  skip_on_cran()

  conf <- remotes_default_config()
  cache <- list(package = NULL, metadata = pkgcache::get_cranlike_metadata_cache())

  ref <- paste0("bioc::", basename(tempfile()))
  res <- asNamespace("pkgcache")$synchronise(
    resolve_remote_bioc(parse_remotes(ref)[[1]], TRUE, conf,
                        cache, dependencies = FALSE)
  )

  expect_true(all(res$status == "FAILED"))

  ## Existing package, non-existing version

  skip("TODO")

  r <- remotes()$new(
    "bioc::Biobase@0.0", config = list(cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_true(all(res$data$status == "FAILED"))
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
    resolve_remote_bioc(parse_remotes("bioc::Biobase")[[1]], TRUE, conf, cache,
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

test_that("satisfies_remote", {

  res <- make_fake_resolution(`bioc::eisa@>=1.0.0` = list())

  ## GitHub type is never good
  bad1 <- make_fake_resolution(`github::r-lib/eisa` = list())
  expect_false(ans <- satisfy_remote_bioc(res, bad1))
  expect_match(attr(ans, "reason"), "Type must be")

  ## Missing DESCRIPTION for installed type
  bad2 <- make_fake_resolution(`installed::foobar` = list())
  expect_false(ans <- satisfy_remote_bioc(res, bad2))
  expect_match(attr(ans, "reason"), "not from BioC")

  ## installed, but not from BioC
  fake_desc <- desc::desc("!new")
  bad3 <- make_fake_resolution(`installed::foobar` = list(
    extra = list(list(description = fake_desc))))
  expect_false(ans <- satisfy_remote_bioc(res, bad3))
  expect_match(attr(ans, "reason"), "not from BioC")

  ## BioC type, but package name does not match
  bad4 <- make_fake_resolution(`bioc::eisa2` = list())
  expect_false(ans <- satisfy_remote_bioc(res, bad4))
  expect_match(attr(ans, "reason"), "names differ")

  ## installed type, but package name does not match
  fake_desc <- desc::desc("!new")
  fake_desc$set(biocViews = "foobar")
  bad5 <- make_fake_resolution(`installed::foobar` = list(
    package = "eisa2",
    extra = list(list(description = fake_desc))))
  expect_false(ans <- satisfy_remote_bioc(res, bad5))
  expect_match(attr(ans, "reason"), "names differ")

  ## BioC type, but version is not good enough
  bad6 <- make_fake_resolution(`bioc::eisa` = list(version = "0.0.1"))
  expect_false(ans <- satisfy_remote_bioc(res, bad6))
  expect_match(attr(ans, "reason"), "Insufficient version")

  ## Same version, BioC
  ok1 <- make_fake_resolution(`bioc::eisa` = list())
  expect_true(satisfy_remote_bioc(res, ok1))

  ## Newer version, BioC
  ok2 <- make_fake_resolution(`bioc::eisa` = list(version = "2.0.0"))
  expect_true(satisfy_remote_bioc(res, ok2))

  ## Same version, installed
  fake_desc <- desc::desc("!new")
  fake_desc$set(biocViews ="BioC")
  ok3 <- make_fake_resolution(`installed::foobar` = list(
    package = "eisa",
    extra = list(list(description = fake_desc))))
  expect_true(satisfy_remote_bioc(res, ok3))

  ## Newer version, installed
  fake_desc <- desc::desc("!new")
  fake_desc$set(biocViews = "foobar")
  ok4 <- make_fake_resolution(`installed::foobar` = list(
    package = "eisa",
    version = "2.0.0",
    extra = list(list(description = fake_desc))))
  expect_true(satisfy_remote_bioc(res, ok4))
})
