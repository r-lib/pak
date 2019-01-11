
context("tar")

test_that("is_gzip, is_bzip2, is_xz, iz_zip", {

  cases <- list(
    list("is_gzip",  "xxx.gz",  3),
    list("is_bzip2", "xxx.bz2", 3),
    list("is_xz",    "xxx.xz",  6),
    list("is_zip",   "xxx.zip", 4)
  )

  lapply(cases, function(case) {
    fun <- get(case[[1]])
    arch <- system.file(package = .packageName, "tools", case[[2]])
    expect_true(fun(arch))

    buf <- readBin(arch, what = "raw", n = case[[3]])
    expect_true(fun(buf))
    expect_false(fun(utils::head(buf, -1)))

    others <- setdiff(c("is_gzip", "is_bzip2", "is_xz", "is_zip"), case[[1]])
    for (ofun in others) {
      expect_false(get(ofun)(arch))
      expect_false(get(ofun)(buf))
    }
  })
})

test_that("detect_package_archive_type", {

  cases <- list(
    list("gzip",    "xxx.gz"),
    list("bzip2",   "xxx.bz2"),
    list("xz",      "xxx.xz"),
    list("zip",     "xxx.zip"),
    list("unknown", "xxx")
  )

  lapply(cases, function(case) {
    arch <- system.file(package = .packageName, "tools", case[[2]])
    expect_equal(detect_package_archive_type(arch), case[[1]])
  })
})

test_that("get_untar_decompress_arg", {
  cases <- list(
    list("-z", "xxx.gz"),
    list("-j", "xxx.bz2"),
    list("-J", "xxx.xz"),
    list(character(), "xxx")
  )

  lapply(cases, function(case) {
    arch <- system.file(package = .packageName, "tools", case[[2]])
    expect_identical(get_untar_decompress_arg(arch), case[[1]])
  })

  zip <- system.file(package = .packageName, "tools", "xxx.zip")
  expect_error(get_untar_decompress_arg(zip), "zip file")
})

test_that("eup_get_args", {

  opts <- list(
    tarfile = system.file(package = .packageName, "tools", "pkg_1.0.0.tgz"),
    files = NULL,
    exdir = "exdir",
    restore_times = TRUE,
    tar = "tar"
  )

  expect_equal(
    eup_get_args(opts),
    c("-x", "-f", opts$tarfile, "-C", opts$exdir, "-z")
  )

  ## No need to ungzip
  opts$tarfile <- system.file(package = .packageName, "tools", "xxx")
  expect_equal(
    eup_get_args(opts),
    c("-x", "-f", opts$tarfile, "-C", opts$exdir)
  )

  ## Files are specified
  opts$files <- c("this", "that")
  expect_equal(
    eup_get_args(opts),
    c("-x", "-f", opts$tarfile, "-C", opts$exdir, opts$files)
  )

  ## Do not restore times
  opts$restore_times <- FALSE
  expect_equal(
    eup_get_args(opts),
    c("-x", "-f", opts$tarfile, "-C", opts$exdir, "-m", opts$files)
  )
})

test_that("external_untar_process", {

  if (need_internal_tar()) skip("external R does not work")

  tarfile <- system.file(package = .packageName, "tools", "pkg_1.0.0.tgz")
  mkdirp(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  px <- external_untar_process()$new(tarfile, exdir = tmp)
  px$wait(5000)
  px$kill()

  expect_equal(px$get_exit_status(), 0)
  expect_true(file.exists(file.path(tmp, "pkg", "DESCRIPTION")))
})

test_that("r_untar_process", {

  tarfile <- system.file(package = .packageName, "tools", "pkg_1.0.0.tgz")
  mkdirp(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  px <- r_untar_process()$new(tarfile, exdir = tmp)
  px$wait(5000)
  px$kill()

  expect_equal(px$get_exit_status(), 0)
  expect_true(file.exists(file.path(tmp, "pkg", "DESCRIPTION")))
})

test_that("make_untar_process", {

  tarfile <- system.file(package = .packageName, "tools", "pkg_1.0.0.tgz")
  mkdirp(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  px <- make_untar_process(tarfile, exdir = tmp)
  px$wait(5000)
  px$kill()

  expect_equal(px$get_exit_status(), 0)
  expect_true(file.exists(file.path(tmp, "pkg", "DESCRIPTION")))
})

test_that("make_untar_process, internal tar", {

  mockery::stub(make_untar_process, "need_internal_tar", TRUE)

  tarfile <- system.file(package = .packageName, "tools", "pkg_1.0.0.tgz")
  mkdirp(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  px <- make_untar_process(tarfile, exdir = tmp)
  px$wait(5000)
  px$kill()

  expect_equal(px$get_exit_status(), 0)
  expect_true(file.exists(file.path(tmp, "pkg", "DESCRIPTION")))
})
