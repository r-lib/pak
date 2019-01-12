
context("resolution result print")

test_that("no errors", {
  res <- read_fixture("resolution-installed.rds")
  expect_match(
    format(res),
    "RESOLUTION.*installed.*pkgconfig"
  )
})

test_that("errors", {
  skip_if_offline()
  skip_on_cran()

  withr::with_options(c(pkg.show_progress = FALSE), {
    npkg1 <- basename(tempfile())
    npkg2 <- basename(tempfile())
    r <- remotes()$new(c(npkg1, paste0("r-lib/", npkg2)), lib = tempfile())
    res <- r$resolve()
  })

  expect_output(print(res), "Errors:")
  expect_output(print(res), "Cannot find standard package")
  expect_output(print(res), "r-lib/.*:.*Not Found")
})
