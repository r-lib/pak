test_that("pak has StagedInstall: no in DESCRIPTION", {
  skip_on_cran()

  dsc <- readLines(
    system.file("DESCRIPTION", package = "pak"),
    warn = FALSE
  )
  expect_true(any(grepl("^StagedInstall:\\s*no$", dsc, perl = TRUE)))
})
