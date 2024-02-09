test_that("pak_sitrep", {
  skip_on_cran()
  sitrep <- capture_output(pak_sitrep())
  expect_match(sitrep, "pak version:", fixed = TRUE)
  expect_match(
    sitrep,
    asNamespace("pak")[[".__NAMESPACE__."]][["spec"]][["version"]],
    fixed = TRUE
  )
  expect_match(sitrep, "pak platform:", fixed = TRUE)
  expect_match(sitrep, "Optional packages installed:", fixed = TRUE)
  expect_match(sitrep, "Library path:", fixed = TRUE)
  expect_match(sitrep, .libPaths()[1], fixed = TRUE)

  mockery::stub(pak_sitrep, "pkg_is_installed", FALSE)
  mockery::stub(pak_sitrep, "is_load_all", TRUE)
  sitrep <- capture_output(pak_sitrep())
  expect_match(sitrep, "Optional packages missing", fixed = TRUE)
  expect_match(sitrep, "Using `load_all()` from", fixed = TRUE)

  mockery::stub(pak_sitrep, "is_load_all", FALSE)
  sitrep <- capture_output(pak_sitrep())
  expect_match(sitrep, "pak is installed at", fixed = TRUE)
})
