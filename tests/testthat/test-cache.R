test_that("cache_summary", {
  cs <- cache_summary()
  expect_equal(names(cs), c("cachepath", "files", "size"))
  expect_true(is_string(cs[["cachepath"]]))
  expect_true(is_count(cs[["files"]]))
  expect_true(is_count(cs[["size"]]))
})

test_that("cache_list", {

})

test_that("cache_delete", {

})

test_that("cache_clean", {

})

test_that("meta_summary", {

})

test_that("meta_list", {

})

test_that("meta_update", {

})

test_that("meta_clean", {

})
