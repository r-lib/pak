
context("solve + describe")

test_that("failed resolution", {
  pkgs <- make_fake_resolution(
    aa = list(status = "FAILED", direct = TRUE, error = list(
      structure(list(message = "EEE"), class = c("error", "condition")))),
    bb = list()
  )
  dsc <- describe_fake_error(pkgs)
  expect_equal(dsc$ref, "aa")
  expect_equal(dsc$type, "standard")
  expect_equal(dsc$direct, TRUE)
  expect_equal(dsc$status, "FAILED")
  expect_equal(dsc$package, "aa")
  expect_equal(dsc$failure_type, "failed-res")
  expect_equal(dsc$failure_message, list("EEE"))
})

test_that("failed resolution", {

  skip_on_cran()
  skip_if_offline()

  withr::with_options(list(pkg.show_progress = FALSE), {
    r <- remotes()$new("nonexistentpackage", lib = tempfile())
    r$resolve()
  })
  sol <- r$solve()
  dsc <- describe_solution_error(r$get_resolution(), sol)
  expect_equal(dsc$ref, "nonexistentpackage")
  expect_equal(dsc$type, "standard")
  expect_equal(dsc$direct, TRUE)
  expect_equal(dsc$status, "FAILED")
  expect_equal(dsc$failure_type, "failed-res")
  expect_equal(dsc$failure_message, list("Cannot find standard package"))
})

test_that("failed resolution of a dependency", {
  pkgs <- make_fake_resolution(
    aa = list(direct = TRUE, deps = list(make_fake_deps(Imports = "bb, xx"))),
    bb = list(deps = list(make_fake_deps(Imports = "cc"))),
    cc = list(status = "FAILED", error = list(structure(list(message = "EEE"),
      class = c("error", "condition")))),
    dd = list(direct = TRUE, deps = list(make_fake_deps(Imports = "ee"))),
    ee = list(),
    xx = list()
  )
  dsc <- describe_fake_error(pkgs)
  expect_equal(dsc$ref, c("aa", "bb", "cc"))
  expect_equal(dsc$type, rep("standard", 3))
  expect_equal(dsc$direct, c(TRUE, FALSE, FALSE))
  expect_equal(dsc$status, c("OK", "OK", "FAILED"))
  expect_equal(dsc$package, c("aa", "bb", "cc"))
  expect_equal(dsc$failure_type, c("dep-failed", "dep-failed", "failed-res"))
  expect_equal(
    dsc$failure_message,
    list("Cannot install dependency bb",
         "Cannot install dependency cc",
         "EEE"))
})

test_that("conflicting direct refs", {
  pkgs <- make_fake_resolution(
    `cran::aa` = list(direct = TRUE),
    `aa/aa` = list(direct = TRUE)
  )
  dsc <- describe_fake_error(pkgs)
  expect_equal(dsc$ref, c("cran::aa", "aa/aa"))
  expect_equal(dsc$type, c("cran", "github"))
  expect_equal(dsc$direct, c(TRUE, TRUE))
  expect_equal(dsc$status, c("OK", "OK"))
  expect_equal(dsc$package, c("aa", "aa"))
  expect_equal(dsc$failure_type, c("satisfy-direct", "satisfy-direct"))
  expect_equal(
    dsc$failure_message,
    list("Conflicts aa/aa", "Conflicts cran::aa"))
})

test_that("dependency conflicts direct ref", {
  pkgs <- make_fake_resolution(
    `cran::aa` = list(direct = TRUE),
    `aa/aa` =  list(list(extra = list(sha = "badcafe"))),
    bb = list(direct = TRUE, deps = list(make_fake_deps(Imports = "cc"))),
    cc = list(deps = list(make_fake_deps(Imports = "aa", Remotes = "aa/aa")))
  )
  dsc <- describe_fake_error(pkgs)
  expect_equal(dsc$ref, c("aa/aa", "bb", "cc"))
  expect_equal(dsc$type, c("github", "standard", "standard"))
  expect_equal(dsc$direct, c(FALSE, TRUE, FALSE))
  expect_equal(dsc$status, c("OK", "OK", "OK"))
  expect_equal(dsc$package, c("aa", "bb", "cc"))
  expect_equal(dsc$failure_type, c("satisfy-direct", "dep-failed",
                                   "dep-failed"))
  expect_equal(
    dsc$failure_message,
    list("Conflicts cran::aa",
         "Cannot install dependency cc",
         "Cannot install dependency aa/aa"))
})

test_that("conflicting dependencies", {
  pkgs <- make_fake_resolution(
    `aa` = list(direct = TRUE,
                deps = list(make_fake_deps(Imports = "cc", Remotes = "cran::cc"))),
    `bb` = list(direct = TRUE,
                deps = list(make_fake_deps(Imports = "cc", Remotes = "cc/cc"))),
    `cran::cc` = list(),
    `cc/cc` = list(extra = list(list(sha = "badcafe")))
  )
  dsc <- describe_fake_error(pkgs)
  expect_equal(dsc$ref, c("aa", "cran::cc"))
  expect_equal(dsc$type, c("standard", "cran"))
  expect_equal(dsc$direct, c(TRUE, FALSE))
  expect_equal(dsc$status, c("OK", "OK"))
  expect_equal(dsc$package, c("aa", "cc"))
  expect_equal(dsc$failure_type, c("dep-failed", "conflict"))
  expect_equal(
    dsc$failure_message,
    list("Cannot install dependency cran::cc",
         "cran::cc conflict with cc/cc, to be installed"))
})

test_that("conflicting dependencies downstream", {
  pkgs <- make_fake_resolution(
    `a0` = list(direct = TRUE, deps = list(make_fake_deps(Imports = "aa, bb"))),
    `aa` = list(direct = TRUE,
                deps = list(make_fake_deps(Imports = "cc", Remotes = "cran::cc"))),
    `bb` = list(direct = TRUE,
                deps = list(make_fake_deps(Imports = "cc", Remotes = "cc/cc"))),
    `cran::cc` = list(),
    `cc/cc` = list(extra = list(list(sha = "badcafe")))
  )
  dsc <- describe_fake_error(pkgs)
  expect_equal(dsc$ref, c("a0", "bb", "cc/cc"))
  expect_equal(dsc$type, c("standard", "standard", "github"))
  expect_equal(dsc$direct, c(TRUE, TRUE, FALSE))
  expect_equal(dsc$status, c("OK", "OK", "OK"))
  expect_equal(dsc$package, c("a0", "bb", "cc"))
  expect_equal(dsc$failure_type, c("dep-failed", "dep-failed", "conflict"))
  expect_equal(
    dsc$failure_message,
    list("Cannot install dependency bb",
         "Cannot install dependency cc/cc",
         "cc/cc conflict with cran::cc, to be installed"))
})
