test_that("doc_config", {
  expect_snapshot(writeLines(doc_config()))

  mockery::stub(doc_config, "file.exists", FALSE)
  expect_match(doc_config(), "Cannot look up documentation")
})

test_that("include_docs", {
  expect_snapshot(
    writeLines(include_docs("pkgdepends", "docs/lib-status-return.rds"))
  )
  expect_match(
    include_docs("pkgdepends", "foo.rds"),
    "Error: cannot load pkgdepends docs"
  )
  expect_match(
    include_docs("pkgdepends", "foo.rds", top = TRUE),
    "Cannot load pkgdepends docs"
  )
})

test_that("pak_or_pkgdepends", {
  expect_equal(pak_or_pkgdepends(), "pak")
})

test_that("man_config_link", {
  expect_snapshot(
    man_config_link("configuration option")
  )
})
