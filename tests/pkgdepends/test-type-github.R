
context("type github")

test_that("resolve_remote", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  refs <- c(
    "r-lib/crayon",
    "github::r-lib/crayon",
    "crayon=r-lib/crayon",
    "crayon=github::r-lib/crayon",
    "wesm/feather/R",
    "r-lib/crayon@b5221ab0246050",
    "r-lib/crayon#61",
    "r-lib/testthat@*release"
  )

  r <- remotes()$new(
    refs, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_s3_class(res, "remotes_resolution")
  expect_equal(sort(res$ref), sort(refs))
  expect_true(all(res$type == "github"))
  expect_true(all(res$direct))
  expect_true(all(res$status == "OK"))

  ord <- match(refs, vcapply(res$metadata, "[[", "RemoteRef"))

  expect_true(all(vcapply(res$metadata, "[[", "RemoteType") == "github"))
  expect_equal(vcapply(res$metadata, "[[", "RemoteRef")[ord], refs)
  expect_equal(
    vcapply(res$metadata, "[[", "RemoteSha")[ord][6],
    "b5221ab0246050dc687dc8b9964d5c44c947b265")
  expect_equal(
    vcapply(res$metadata, "[[", "RemoteUsername")[ord],
    c("r-lib", "r-lib", "r-lib", "r-lib", "wesm", "r-lib", "r-lib", "r-lib"))
  expect_equal(
    vcapply(res$metadata, "[[", "RemoteRepo")[ord],
    c("crayon", "crayon", "crayon", "crayon", "feather", "crayon",
      "crayon", "testthat"))
  expect_equal(
    vcapply(res$metadata, "[", "RemoteSubdir")[ord],
    c(NA, NA, NA, NA, "R", NA, NA, NA))
  expect_true(all(vcapply(res$metadata, "[[", "RemoteHost") == "api.github.com"))

  expect_equal(vcapply(res$metadata, "[[", "GithubRepo"),
               vcapply(res$metadata, "[[", "RemoteRepo"))
  expect_equal(vcapply(res$metadata, "[[", "GithubUsername"),
               vcapply(res$metadata, "[[", "RemoteUsername"))
  expect_equal(vcapply(res$metadata, "[[", "GithubRef"),
               vcapply(res$metadata, "[[", "RemoteRef"))
  expect_equal(vcapply(res$metadata, "[[", "GithubSHA1"),
               vcapply(res$metadata, "[[", "RemoteSha"))
  expect_equal(vcapply(res$metadata, "[", "GithubSubdir"),
               vcapply(res$metadata, "[", "RemoteSubdir"))
})

test_that("failed resolution", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  nonrepo <- paste0(basename(tempfile()), "/", basename(tempfile()))
  r <- remotes()$new(
    nonrepo, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_true(all(res$status == "FAILED"))

  ## Existing repo, no R package there

  r <- remotes()$new(
    "github::r-lib/crayon/R", config = list(cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE),
    expect_error(r$resolve(), NA))
  res <- r$get_resolution()

  expect_true(all(res$status == "FAILED"))
})

test_that("download_remote", {

  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  ref <- "github::r-lib/crayon@b5221ab0246050dc687dc8b9964d5c44c947b265"
  r <- remotes()$new(
    ref, config = list(dependencies = FALSE, cache_dir = tmp))
  withr::with_options(
    c(pkg.show_progress = FALSE), {
      expect_error(r$resolve(), NA)
      expect_error(r$download_resolution(), NA)
    })
  dl <- r$get_resolution_download()

  expect_s3_class(dl, "remotes_downloads")
  expect_true(dl$ref == ref)
  expect_true(dl$type == "github")
  expect_true(dl$direct)
  expect_true(dl$status == "OK")
  expect_true(dl$package == "crayon")
  expect_true(dl$download_status %in% c("Got", "Had"))
})

test_that("satisfies_remote", {

  res <- make_fake_resolution(`github::r-lib/crayon` = list(
    extra = list(list(sha = "badcafe")),
    package = "crayon",
    version = "1.0.0"))

  ## Different package name
  bad1 <- make_fake_resolution(`github::r-lib/crayon` = list(
    package = "crayon2"))
  expect_false(ans <- satisfy_remote_github(res, bad1))
  expect_match(attr(ans, "reason"), "names differ")

  ## Installed ref without sha
  fake_desc <- desc::desc("!new")
  bad2 <- make_fake_resolution(`installed::foobar` = list(
    extra = list(list(description = fake_desc)),
    package = "crayon"))
  expect_false(ans <- satisfy_remote_github(res, bad2))
  expect_match(attr(ans, "reason"), "Installed package sha mismatch")

  ## Installed ref with different sha
  fake_desc <- desc::desc("!new")
  fake_desc$set("RemoteSha" = "notsobad")
  bad3 <- make_fake_resolution(`installed::foobar` = list(
    extra = list(list(description = fake_desc)),
    package = "crayon"))
  expect_false(ans <- satisfy_remote_github(res, bad3))
  expect_match(attr(ans, "reason"), "Installed package sha mismatch")

  ## Other package, different sha
  bad4 <- make_fake_resolution(`local::bar` = list(
    package = "crayon",
    extra = list(list(sha = "notsobad"))))
  expect_false(ans <- satisfy_remote_github(res, bad4))
  expect_match(attr(ans, "reason"), "Candidate package sha mismatch")

  ## Corrent sha, GitHub
  ok1 <- make_fake_resolution(`installed::foo` = list(
    extra =  list(list(remotesha = "badcafe")),
    package = "crayon",
    version = "1.0.0"))
  expect_true(satisfy_remote_github(res, ok1))

  ## Corrent sha, another type
  ok2 <- make_fake_resolution(`local::bar` = list(
    package = "crayon",
    extra = list(list(sha = "badcafe"))))
  expect_true(ans <- satisfy_remote_github(res, ok2))
})
