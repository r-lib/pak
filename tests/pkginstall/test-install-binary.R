context("install_binary")

test_that("install_binary", {
  skip_in_covr()
  pkg <- binary_test_package("foo_0.0.0.9000")

  libpath <- create_temp_dir()
  on.exit({
    detach("package:foo", character.only = TRUE, unload = TRUE)
    remove.packages("foo", lib = libpath)
    unlink(libpath, recursive = TRUE)
  })

  expect_error_free(
    install_binary(pkg, lib = libpath, quiet = TRUE))
  expect_error_free(
    library("foo", lib.loc = libpath))
  expect_equal(foo::foo(), NULL)
})

test_that("install_binary works for simultaneous installs", {
  skip_on_cran()
  skip_in_covr()

  pkg <- binary_test_package("foo_0.0.0.9000")
  on.exit({
    detach("package:foo", character.only = TRUE, unload = TRUE)
    remove.packages("foo", lib = libpath)
    unlink(libpath, recursive = TRUE)
  })

  libpath <- create_temp_dir()

  processes <- list()
  num <- 5

  # install and load foo here to test loaded DLLs in another process
  expect_error_free(
    install_binary(pkg, lib = libpath, quiet = TRUE))
  expect_error_free(
    library("foo", lib.loc = libpath))

  expect_equal(foo::foo(), NULL)
  processes <- replicate(num, simplify = FALSE,
    callr::r_bg(args = list(pkg, libpath),
      function(pkg, libpath) pak:::install_binary(pkg, lib = libpath))
  )

  repeat {
    Sys.sleep(.1)
    done <- all(!map_lgl(processes, function(x) x$is_alive()))
    if (done) { break }
  }

  for (i in seq_len(num)) {
    expect_identical(processes[[i]]$get_result(), file.path(libpath, "foo"))
  }
})

test_that("install_binary errors", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  cat("foobar\n", file = tmp)

  expect_error(
    install_binary(tmp, lib = tempdir(), quiet = TRUE),
    "unknown archive type"
  )
})

test_that("make_install_process error", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  cat("foobar\n", file = tmp)

  expect_error(
    make_install_process(tmp, lib = tempdir()),
    "Cannot extract"
  )
})
