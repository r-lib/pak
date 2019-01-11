
context("resolution-df")

test_that("res_make_empty_df", {
  df <- res_make_empty_df()
  expect_true(tibble::is_tibble(df))
  expect_equal(nrow(df), 0L)
})

test_that("res_df_defaults", {
  empty <- res_make_empty_df()
  def <- res_df_defaults()
  expect_true(all_named(def))
  expect_true(all(names(def) %in% names(empty)))

  def_types <- vcapply(def, class)
  empty_types <- res_df_entry_types()[names(def)]
  expect_true(all(def_types == empty_types | def_types == "call"))

  lengths <- viapply(def, length)
  expect_true(all(lengths == 1L | vcapply(def, class) == "call"))
})

test_that("res_df_entry_types", {
  types <- res_df_entry_types()
  expect_true(all_named(types))
  expect_true(is.character(types))
  expect_false(any(types == ""))
})

test_that("res_df_must_have", {
  types <- res_df_must_have()
  def <- res_df_defaults()
  expect_true(length(intersect(types, def)) == 0)
})

test_that("res_add_df_entries", {
  good <- list(
    ref = "package",
    type = "standard",
    package = "package",
    version = "1.0.0",
    sources = c("url1", "url2")
  )

  empty <- res_make_empty_df()

  df <- res_add_df_entries(empty, good)
  expect_identical(names(df), names(empty))
  expect_equal(nrow(df), 1)

  good2 <- tibble::tibble(
    ref = "package",
    type = "standard",
    package = "package",
    version = "1.0.0",
    sources = list(c("url1", "url2"))
  )

  empty <- res_make_empty_df()

  df2 <- res_add_df_entries(empty, good2)
  expect_identical(df2, df)

  good3 <- tibble::tibble(
    ref = c("package1", "package2"),
    type = c("standard", "cran"),
    package = c("package1", "package2"),
    version = c("1.0.0", "2.0.0"),
    sources = list(c("url1", "url2"), "url3")
  )

  empty <- res_make_empty_df()

  df3 <- res_add_df_entries(empty, good3)
  expect_identical(names(df3), names(empty))
  expect_equal(nrow(df3), 2)
})
