
context("dependency tree")

test_that("draw_tree", {

  skip_if_offline()
  skip_on_cran()

  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)

  withr::with_options(c(pkg.show_progress = FALSE), {
    r <- remotes()$new(c("pkgconfig", "igraph"), library = lib)
    r$resolve()
    r$solve()
  })

  out <- r$draw_tree()
  expect_s3_class(out, "tree")
  expect_true(is.character(out))
})
