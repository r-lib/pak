
context("install_packages")

describe("install_packages", {

  it("works with source packages", {

  pkg <- "foo_0.0.0.9000.tar.gz"
  expect_error_free(pkgbuild::build("foo", quiet = TRUE))

  libpath <- create_temp_dir()

  on.exit({
    detach("package:foo", character.only = TRUE, unload = TRUE)
    remove.packages("foo", lib = libpath)
    unlink(libpath, recursive = TRUE)
    unlink(pkg)
  })

  withr::with_options(list(pkg.show_progress = FALSE), {
    plan <- make_install_plan(
      paste0("local::", pkg), lib = libpath)
    expect_error_free(
      install_package_plan(plan, lib = libpath))
  })

  expect_error_free(
    library("foo", lib.loc = libpath))
  })
})
