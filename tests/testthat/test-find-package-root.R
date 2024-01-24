test_that("find_package_root", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mkdirp(file.path(tmp, "a", "b", "c", "d"))
  lns <- "Package: this"
  writeLines(lns, file.path(tmp, "DESCRIPTION"))

  expect_equal(
    readLines(file.path(find_package_root(tmp), "DESCRIPTION")),
    lns
  )

  expect_equal(
    readLines(file.path(
      find_package_root(file.path(tmp, "a")), "DESCRIPTION"
    )),
    lns
  )

  expect_equal(
    readLines(file.path(
      find_package_root(file.path(tmp, "a", "b", "c", "d")), "DESCRIPTION"
    )),
    lns
  )

  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(file.path(tmp, "a", "b", "c"))
  expect_equal(
    readLines(file.path(find_package_root("."), "DESCRIPTION")),
    lns
  )
})

test_that("find_package_root errors", {
  expect_error(
    find_package_root(basename(tempfile())),
    "Path does not exist"
  )

  if (!file.exists("/DESCRIPTION")) {
    expect_error(
      find_package_root("/"),
      "Could not find R package"
    )
  }
})
