
context("proj_* utilities")

test_that("proj_get_dirs", {
  dsc <- desc::desc("!new")
  dir <- test_temp_dir()
  dsc$write(file = file.path(dir, "DESCRIPTION"))
  mkdirp(file.path(dir, "one", "two", "three"))
  mkdirp(file.path(dir, "r-packages"))

  expect_equal(norm_path(proj_get_dirs(dir)$root), norm_path(dir))
  expect_equal(
    norm_path(proj_get_dirs(dir)$lib), norm_path(file.path(dir, "r-packages")))
  expect_equal(
    norm_path(proj_get_dirs(file.path(dir, "one"))$root), norm_path(dir))
  expect_equal(
    norm_path(proj_get_dirs(file.path(dir, "one", "two"))$root), norm_path(dir))
  expect_equal(
    norm_path(proj_get_dirs(file.path(dir, "one", "two", "three"))$root),
    norm_path(dir))
})

test_that("add_refs_to_description", {

  dsc <- desc::desc("!new")
  dsc$set_dep("mypackage")
  dir <- test_temp_dir()
  dsc$write(file = file.path(dir, "DESCRIPTION"))
  expect_equal(dsc$get_deps()$package, "mypackage")

  ## Already there
  add_refs_to_description(dir, "mypackage", FALSE)
  dsc2 <- desc::desc(dir)
  expect_equal(dsc2$get_deps()$package, "mypackage")
  expect_equal(dsc2$get_deps()$type, "Imports")
  expect_identical(dsc2$get_remotes(), character())

  ## Newly added
  add_refs_to_description(dir, "mypackage2", FALSE)
  dsc2 <- desc::desc(dir)
  expect_equal(sort(dsc2$get_deps()$package), c("mypackage", "mypackage2"))
  expect_equal(dsc2$get_deps()$type, c("Imports", "Imports"))
  expect_identical(dsc2$get_remotes(), character())

  ## Optional dependency
  dsc$write(file = file.path(dir, "DESCRIPTION"))
  add_refs_to_description(dir, "mypackage2", TRUE)
  dsc2 <- desc::desc(dir)
  deps <- dsc2$get_deps()
  deps <- deps[ order(deps$package), ]
  expect_equal(deps$package, c("mypackage", "mypackage2"))
  expect_equal(deps$type, c("Imports", "Suggests"))
  expect_identical(dsc2$get_remotes(), character())

  ## GH remote
  dsc$write(file = file.path(dir, "DESCRIPTION"))
  add_refs_to_description(dir, "user/mypackage2", FALSE)
  dsc2 <- desc::desc(dir)
  deps <- dsc2$get_deps()
  deps <- deps[ order(deps$package), ]
  expect_equal(deps$package, c("mypackage", "mypackage2"))
  expect_equal(deps$type, c("Imports", "Imports"))
  expect_identical(dsc2$get_remotes(), "user/mypackage2")

  ## Switching from GH to CRAN
  add_refs_to_description(dir, "mypackage2", FALSE)
  dsc2 <- desc::desc(dir)
  expect_equal(sort(dsc2$get_deps()$package), c("mypackage", "mypackage2"))
  expect_equal(dsc2$get_deps()$type, c("Imports", "Imports"))
  expect_identical(dsc2$get_remotes(), character())

  ## Switching to another GH branch
  dsc$write(file = file.path(dir, "DESCRIPTION"))
  add_refs_to_description(dir, "user/mypackage2", FALSE)
  dsc2 <- desc::desc(dir)
  deps <- dsc2$get_deps()
  deps <- deps[ order(deps$package), ]
  expect_equal(deps$package, c("mypackage", "mypackage2"))
  expect_equal(deps$type, c("Imports", "Imports"))
  expect_identical(dsc2$get_remotes(), "user/mypackage2")
  add_refs_to_description(dir, "user/mypackage2@badcafe", FALSE)
  dsc2 <- desc::desc(dir)
  deps <- dsc2$get_deps()
  deps <- deps[ order(deps$package), ]
  expect_equal(deps$package, c("mypackage", "mypackage2"))
  expect_equal(deps$type, c("Imports", "Imports"))
  expect_identical(dsc2$get_remotes(), "user/mypackage2@badcafe")


})

test_that("remove_refs_from_description", {
  dsc <- desc::desc("!new")
  dsc$set_dep("mypackage")
  dir <- test_temp_dir()
  dsc$write(file = file.path(dir, "DESCRIPTION"))
  expect_equal(dsc$get_deps()$package, "mypackage")

  ## Package is not there
  remove_refs_from_description(dir, parse_remotes("mypackage2"))
  dsc2 <- desc::desc(dir)
  expect_equal(dsc2$get_deps()$package, "mypackage")
  expect_equal(dsc2$get_deps()$type, "Imports")
  expect_identical(dsc2$get_remotes(), character())

  ## Package is there
  dsc$set_dep("mypackage2")
  dsc$write(file = file.path(dir, "DESCRIPTION"))
  remove_refs_from_description(dir, parse_remotes("mypackage2"))
  dsc2 <- desc::desc(dir)
  expect_equal(dsc2$get_deps()$package, "mypackage")
  expect_equal(dsc2$get_deps()$type, "Imports")
  expect_identical(dsc2$get_remotes(), character())

  ## Remote ref that refers to package, is removed as well
  add_refs_to_description(dir, "user/mypackage2", FALSE)
  remove_refs_from_description(dir, parse_remotes("mypackage2"))
  dsc2 <- desc::desc(dir)
  expect_equal(dsc2$get_deps()$package, "mypackage")
  expect_equal(dsc2$get_deps()$type, "Imports")
  expect_identical(dsc2$get_remotes(), character())

  ## Remote ref can be removed
  add_refs_to_description(dir, "user/mypackage2", FALSE)
  remove_refs_from_description(
    dir, parse_remotes("user/mypackage2"))
  dsc2 <- desc::desc(dir)
  expect_equal(dsc2$get_deps()$package, "mypackage")
  expect_equal(dsc2$get_deps()$type, "Imports")
  expect_identical(dsc2$get_remotes(), character())
})
