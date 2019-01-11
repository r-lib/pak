
context("zip")

test_that("make_unzip_process", {

  zipfile <- system.file(package = .packageName, "tools", "xxx.zip")
  mkdirp(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  px <- make_unzip_process(zipfile, exdir = tmp)
  px$wait(5000)
  px$kill()

  expect_equal(px$get_exit_status(), 0)
  expect_true(file.exists(file.path(tmp, "xxx")))
})
