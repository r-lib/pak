fake_pkg <- function(lib, name) {
  dir.create(file.path(lib, name), recursive = TRUE)
  writeLines(
    c(paste("Package:", name), "Version: 1.0.0"),
    file.path(lib, name, "DESCRIPTION")
  )
}

test_that("pkg_remove() removes a package", {
  skip_on_cran()
  lib <- test_temp_dir()
  fake_pkg(lib, "pakremovea")
  fake_pkg(lib, "pakremoveb")

  pkg_remove("pakremovea", lib = lib)
  expect_equal(dir(lib), "pakremoveb")
})

test_that("pkg_remove() removes a character vector of packages", {
  skip_on_cran()
  lib <- test_temp_dir()
  fake_pkg(lib, "pakremovea")
  fake_pkg(lib, "pakremoveb")
  fake_pkg(lib, "pakremovec")

  pkg_remove(c("pakremovea", "pakremoveb"), lib = lib)
  expect_equal(dir(lib), "pakremovec")
})
