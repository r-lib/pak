context("verify_extracted_package")

describe("verify_extracted_package", {

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  run <- function(pkgfile) {
    unlink(tmp, recursive = TRUE)
    mkdirp(tmp)
    utils::untar(pkgfile, exdir = tmp)
    verify_extracted_package(pkgfile, tmp)
  }

  it("errors if archive doesn't contain a DESCRIPTION file", {
    f1 <- local_binary_package("test1")
    expect_error(run(f1),
      "'.*test1[.]tgz' is not a valid R package, it is an empty archive")
  })

  it("errors if archive DESCRIPTION is not in the root directory", {
    f2 <- local_binary_package("test2", "foo/DESCRIPTION" = character())
    expect_error(run(f2),
      "'.*test2[.]tgz' is not a valid binary, it does not contain 'test2/Meta/package.rds' and 'test2/DESCRIPTION'.")
  })

  it("can handle multiple DESCRIPTION files", {
    f3 <- local_binary_package("test3",
      "DESCRIPTION" = c("Package: test3", "Built: 2017-01-01"),
      "tests/testthat/DESCRIPTION" = character(),
      "Meta/package.rds" = character())
    expect_is(run(f3)$desc, "description")

    f4 <- local_binary_package("test4",
      "pkgdir/DESCRIPTION" = c("Package: test4", "Built: 2017-01-01"),
      "Meta/package.rds" = character())
    expect_error(run(f4),
      "'.*test4[.]tgz' is not a valid binary, it does not contain 'test4/DESCRIPTION'.")
  })

  it("fails if the binary does not contain package.rds", {
    f5 <- local_binary_package("test5", "DESCRIPTION" = character())
    expect_error(run(f5),
      "'.*test5[.]tgz' is not a valid binary, it does not contain 'test5/Meta/package[.]rds'")
  })

  it("fails if the DESCRIPTION file is empty", {
    f6 <- local_binary_package("test6", "DESCRIPTION" = character(), "Meta/package.rds" = character())
    expect_error(run(f6),
      "'.*test6[.]tgz' is not a valid binary, 'test6/DESCRIPTION' is empty")
  })

  it("fails if the DESCRIPTION file has no 'Built' entry", {
    f7 <- local_binary_package("test7", "DESCRIPTION" = c("Package: test7"), "Meta/package.rds" = character())
    expect_error(run(f7),
      "'.*test7[.]tgz' is not a valid binary, no 'Built' entry in 'test7/DESCRIPTION'")
  })
})

test_that("verify_extrancted_package errors", {

  pkg_dir <- file.path("fixtures", "packages")

  expect_error(
    verify_extracted_package("bad1", file.path(pkg_dir, "bad1")),
    "single directory"
  )

  expect_error(
    verify_extracted_package("bad2", file.path(pkg_dir, "bad2")),
    "invalid"
  )

  expect_error(
    verify_extracted_package("bad3", file.path(pkg_dir, "bad3")),
    "Package"
  )

  expect_error(
    verify_extracted_package("bad4", file.path(pkg_dir, "bad4")),
    "package name mismatch"
  )
})
