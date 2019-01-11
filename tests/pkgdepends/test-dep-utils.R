
context("dependency utilities")

test_that("resolve_ref_deps", {

  deps <- data.frame(
    stringsAsFactors = FALSE,
    type = c(rep("Suggests", 4), rep("Imports", 5)),
    package = c("covr", "jsonlite", "testthat", "assertthat", "curl", "R6",
      "rlang", "uuid", "bar"),
    version = c("*", "*", "*", "*", ">= 2.8.9000", "*", "*", "*", "*")
  )
  remotes <- c(Remotes = "\n    jeroen/curl,\n  foo/bar")

  obj <- resolve_ref_deps(deps, remotes)

  exp <- tibble::tibble(
    ref = c("covr", "jsonlite", "testthat", "assertthat",
            "jeroen/curl", "R6", "rlang", "uuid", "foo/bar"),
    type = c(rep("Suggests", 4), rep("Imports", 5)),
    package = c("covr", "jsonlite", "testthat", "assertthat",
                "curl", "R6", "rlang", "uuid", "bar"),
    op = c("", "", "", "", ">=", "", "", "", ""),
    version = c("", "", "", "", "2.8.9000", "", "", "", "")
  )

  expect_equal(obj, exp)
})
