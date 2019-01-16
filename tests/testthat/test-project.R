
context("project")

test_that("proj_install from DESCRIPTION", {
  skip_if_offline()

  dsc <- desc::desc("!new")
  dsc$set(Package = "foobar")
  dir <- test_temp_dir()
  dsc$write(file = file.path(dir, "DESCRIPTION"))
  add_refs_to_description(dir, c("cli", "cran/pkgconfig@2.0.2"), FALSE)

  pkg_data$tmp <- NULL
  plan <- proj_install_make_plan(pkg = NULL, root = dir, upgrade = FALSE,
                                 ask = FALSE, start = Sys.time(), dev = FALSE)
  expect_true(plan)

  data <- pkg_data$tmp
  expect_s3_class(data$remotes, "remotes")
  expect_silent(data$remotes$get_solution())
  expect_s3_class(data$start, "POSIXct")
  expect_true(file.exists(data$root))
  expect_true(file.exists(data$lib))

  res <- proj_install_do_plan(optional = FALSE)
  expect_s3_class(res, "pkg_install_result")
  expect_true(all(
    c("assertthat", "cli", "crayon", "cran/pkgconfig@2.0.2") %in% res$ref))
  expect_true(file.exists(file.path(data$lib, "pkgconfig")))

  stat <- proj_status_internal(root = dir)
  expect_s3_class(stat, "tbl_df")
  expect_true(all(
    c("assertthat", "cli", "crayon", "pkgconfig") %in% stat$package))

  pkg_data$tmp <- NULL
  ret <- proj_remove_internal("pkgconfig", dir, ask = FALSE)
  expect_true(ret)
  proj_remove_internal_do()

  stat2 <- proj_status_internal(root = dir)
  expect_s3_class(stat2, "tbl_df")
  expect_true(all(c("assertthat", "cli", "crayon") %in% stat2$package))
  expect_false("pkgconfig" %in% stat2$package)

  dsc <- desc::desc(file.path(data$root))
  deps <- dsc$get_deps()
  expect_equal(deps$package, c("cli"))
  expect_identical(dsc$get_remotes(), character())
})

test_that("proj_install with new package", {
  skip_if_offline()

  dsc <- desc::desc("!new")
  dir <- test_temp_dir()
  dsc$write(file = file.path(dir, "DESCRIPTION"))

  pkg_data$tmp <- NULL
  plan <- proj_install_make_plan(pkg = c("cli", "cran/pkgconfig@2.0.2"),
                                 root = dir, upgrade = FALSE,
                                 ask = FALSE, start = Sys.time(), dev = FALSE)
  expect_true(plan)

  data <- pkg_data$tmp
  expect_s3_class(data$remotes, "remotes")
  expect_silent(data$remotes$get_solution())
  expect_s3_class(data$start, "POSIXct")
  expect_true(file.exists(data$root))
  expect_true(file.exists(data$lib))

  res <- proj_install_do_plan(optional = FALSE)
  expect_s3_class(res, "pkg_install_result")
  expect_true(all(
    c("assertthat", "cli", "crayon", "cran/pkgconfig@2.0.2") %in% res$ref))
  expect_true(file.exists(file.path(data$lib, "pkgconfig")))

  dsc <- desc::desc(file.path(data$root))
  deps <- dsc$get_deps()
  expect_equal(deps$package, c("cli", "pkgconfig"))
})
