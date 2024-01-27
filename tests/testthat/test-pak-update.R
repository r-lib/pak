test_that("detect_platform", {
  macos <- list(
    platform = "aarch64-apple-darwin20",
    arch = "aarch64",
    os = "darwin20",
    system = "aarch64, darwin20",
    status = "",
    major = "4",
    minor = "3.2",
    year = "2023",
    month = "10",
    ay = "31",
    `svn rev` = "85441",
    language = "R",
    version.string = "R version 4.3.2 (2023-10-31)",
    nickname = "Eye Holes"
  )
  mockery::stub(detect_platform, "R.Version", macos)
  mockery::stub(detect_platform, "getRversion", package_version("4.3.2"))
  expect_snapshot(detect_platform())

  linux <- list(
    platform = "x86_64-pc-linux-gnu",
    arch = "x86_64",
    os = "linux-gnu",
    system = "x86_64, linux-gnu",
    status = "",
    major = "4",
    minor = "3.2",
    year = "2023",
    month = "10",
    day = "31",
    `svn rev` = "85441",
    language = "R",
    version.string = "R version 4.3.2 (2023-10-31)",
    nickname = "Eye Holes"
  )
  mockery::stub(detect_platform, "R.Version", linux)
  mockery::stub(detect_platform, "getRversion", package_version("4.3.2"))
  expect_snapshot(detect_platform())
})

test_that("pak_stream", {
  expect_equal(pak_stream("foo"), "foo")
  mockery::stub(pak_stream, "utils::packageVersion", "0.7.1")
  expect_equal(pak_stream(), "stable")
  mockery::stub(pak_stream, "utils::packageVersion", "0.7.1.9999")
  expect_equal(pak_stream(), "rc")
  mockery::stub(pak_stream, "utils::packageVersion", "0.7.1.9000")
  expect_equal(pak_stream(), "devel")
  mockery::stub(pak_stream, "utils::packageVersion", "0.7.1.9001")
  expect_equal(pak_stream(), "devel")
})

test_that("pak_repo", {
  expect_snapshot({
    pak_repo("devel")
    pak_repo("rc")
    pak_repo("stable")
  })
})

test_that("pak_repo_metadata", {
  meta_path <- paste0("file:///", normalizePath(test_path("fixtures")), "/")
  expect_snapshot(pak_repo_metadata(meta_path))
})

test_that("pak_update", {
  mockery::stub(
    pak_update,
    "pak_repo",
    paste0("file:///", normalizePath(test_path("fixtures")), "/")
  )
  macos <- list(os = "darwin20", arch = "aarch64", rver = "4.3")
  s390x <- list(os = "linux", arch = "s390x", rver = "4.3")
  mockery::stub(pak_update, "detect_platform", s390x)
  mockery::stub(pak_update, "is_load_all", FALSE)
  expect_snapshot(error = TRUE, pak_update())

  # no need to update, load_all() warning
  mockery::stub(pak_update, "detect_platform", macos)
  mockery::stub(pak_update, "check_mac_cran_r", TRUE)
  mockery::stub(pak_update, "should_update_to", FALSE)
  mockery::stub(pak_update, "is_load_all", TRUE)
  transform_lib <- function(x) {
    sub(.libPaths()[1], "<lib-path>", x, fixed = TRUE)
  }
  expect_snapshot(pak_update(), transform = transform_lib)

  # same, w/o load_all() warning
  mockery::stub(pak_update, "is_load_all", FALSE)
  expect_snapshot(pak_update(), transform = transform_lib)

  mockery::stub(pak_update, "should_update_to", TRUE)
})

test_that("pak_update_unsupported_platform", {
  meta_path <- paste0("file:///", normalizePath(test_path("fixtures")), "/")
  meta <- pak_repo_metadata(meta_path)
  me <- list(os = "Linux", arch = "s390x", rver = "4.3")
  expect_snapshot(
    pak_update_unsupported_platform("devel", me, meta),
    error = TRUE
  )
})

test_that("check_mac_cran_r", {
  expect_silent(check_mac_cran_r(list(os = "linux")))
  mockery::stub(check_mac_cran_r, "platform_pkgtype", "source")
  me <- list(os = "darwin20")
  expect_snapshot(
    error = TRUE,
    check_mac_cran_r(me)
  )
})

test_that("platform_pkgtype", {
  expect_equal(platform_pkgtype(), .Platform$pkgType)
})

test_that("should_update_to", {
  mockery::stub(
    should_update_to,
    "utils::packageDescription",
    list(
      Version = utils::packageVersion("pak"),
      Built = "R 4.3.2; aarch64-apple-darwin20; 2024-01-25 11:40:41 UTC; unix"
    )
  )
  expect_true(should_update_to(list(Version = "1000.0.0")))
  expect_true(should_update_to(list(
    Version = utils::packageVersion("pak"),
    Built = "R 4.3.2; aarch64-apple-darwin20; 2124-01-25 11:40:41 UTC; unix"
  )))
  expect_false(should_update_to(list(
    Version = utils::packageVersion("pak"),
    Built = "R 4.3.2; aarch64-apple-darwin20; 2000-01-25 11:40:41 UTC; unix"
  )))

  mockery::stub(
    should_update_to,
    "R.Version",
    list(platform = "s390x-pc-linux-gnu")
  )
  expect_snapshot(expect_true(should_update_to()))
})

test_that("get_built_date", {
  expect_equal(get_built_date(NULL), NA_character_)
  expect_equal(
    get_built_date("R 4.3.2; aarch64-apple-darwin20; 2024-01-25 11:40:41 UTC; unix"),
    "2024-01-25 11:40:41 UTC"
  )
})
