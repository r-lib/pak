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

})

test_that("pak_update_unsupported_platform", {

})

test_that("check_mac_cran_r", {

})

test_that("should_update_to", {

})

test_that("get_built_date", {

})
