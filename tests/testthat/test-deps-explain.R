test_that("pkg_deps_explain() works with a local package path", {
  skip_if_offline()
  tmp <- test_temp_dir()
  dep <- file.path(tmp, "dep")
  main <- file.path(tmp, "main")
  dir.create(dep)
  dir.create(main)
  writeLines(
    c(
      "Package: pakexplaindep",
      "Version: 1.0.0",
      "Title: Dep",
      "Description: Dep.",
      "License: MIT"
    ),
    file.path(dep, "DESCRIPTION")
  )
  writeLines(
    c(
      "Package: pakexplainmain",
      "Version: 1.0.0",
      "Title: Main",
      "Description: Main.",
      "License: MIT",
      "Imports: pakexplaindep",
      paste0("Remotes: pakexplaindep=local::", dep)
    ),
    file.path(main, "DESCRIPTION")
  )
  withr::local_dir(main)

  deps <- c("pakexplaindep", "pakexplainnone")
  expected <- pkg_deps_explain("local::.", deps)
  res <- pkg_deps_explain(".", deps)
  expect_equal(res$paths, expected$paths)
  expect_equal(
    res$paths$pakexplaindep,
    list(c("pakexplainmain", "pakexplaindep"))
  )
  expect_length(res$paths$pakexplainnone, 0)
})
