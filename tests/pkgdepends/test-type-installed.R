
context("installed ref type")

test_that("resolve", {

  conf <- remotes_default_config()

  tt <- dirname(dirname(attr(packageDescription("testthat"), "file")))
  cache <- list(
    package = NULL,
    metadata = pkgcache::get_cranlike_metadata_cache(),
    installed = make_installed_cache(dirname(tt)))

  ref <- paste0("installed::", tt)
  res <- asNamespace("pkgcache")$synchronise(
    resolve_remote_installed(parse_remotes(ref)[[1]], TRUE, conf, cache,
                             dependencies = "Imports")
  )

  unun <- function(x) {
    attr(x, "unknown_deps") <- NULL
    x
  }

  expect_equal(
    unun(as.list(res[c("ref", "type", "direct", "status", "package", "version")])),
    list(ref = ref, type = "installed", direct = TRUE, status = "OK",
         package = "testthat",
         version = as.character(packageVersion("testthat")))
  )

  expect_true("crayon" %in% attr(res, "unknown_deps"))

  expect_false(is.null(res$extra[[1]]$repotype))
})

test_that("download", {
  skip_if_offline()
  skip_on_cran()

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp2 <- tempfile())
  on.exit(unlink(tmp2, recursive = TRUE), add = TRUE)

  tt <- dirname(dirname(attr(packageDescription("testthat"), "file")))
  ref <- paste0("installed::", tt)
  r <- remotes()$new(
    ref, library = dirname(tt),
    config = list(dependencies = FALSE, cache_dir = tmp))
  expect_error(r$resolve(), NA)
  expect_error(r$download_resolution(), NA)
  dl <- r$get_resolution_download()
  expect_equal(dl$download_status, "Had")
})

test_that("satisfy", {
  ## Always TRUE, independently of arguments
  expect_true(satisfy_remote_installed())
})
