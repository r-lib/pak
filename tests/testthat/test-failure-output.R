test_that("output is printed on failure", {
  skip_on_cran()
  skip_on_covr()

  # Full output in interactive sessions
  badcompile <- paste0(
    "local::",
    normalizePath(test_path("fixtures/badcompile"))
  )
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  expect_error(
    callr::r(
      stdout = tmp,
      stderr = tmp,
      function(pkg) {
        # if this is not R CMD check, use `load_all()``
        # otherwise `load_all()` does not work, because we are not in the
        # package tree in `R CMD check`.
        if (Sys.getenv("_R_CHECK_PACKAGE_NAME_") == "") pkgload::load_all()
        options(rlib_interactive = TRUE)
        pak::pkg_install(pkg)
      },
      list(pkg = badcompile)
    ),
    class = "callr_error"
  )

  lines <- readLines(tmp)
  expect_true(any(grepl("Failed to build badcompile", lines)))
  expect_true(any(grepl("Full installation output", lines)))
  expect_true(any(grepl(
    "compilation failed for package .*badcompile.*",
    lines
  )))

  expect_error(
    callr::r(
      stdout = tmp,
      stderr = tmp,
      function(pkg) {
        # see above
        if (Sys.getenv("_R_CHECK_PACKAGE_NAME_") == "") pkgload::load_all()
        options(rlib_interactive = FALSE)
        pak::pkg_install(pkg)
      },
      list(pkg = badcompile)
    ),
    class = "callr_error"
  )

  lines <- readLines(tmp)
  expect_true(any(grepl("Failed to build badcompile", lines)))
  expect_true(any(grepl("Full installation output", lines)))
  expect_true(any(grepl(
    "compilation failed for package .*badcompile.*",
    lines
  )))
})
