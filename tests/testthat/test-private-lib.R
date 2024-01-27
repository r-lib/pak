test_that("private_lib_dir", {
  mockery::stub(private_lib_dir, "file.path", "foobar")
  mockery::stub(private_lib_dir, "file.exists", TRUE)
  expect_equal(private_lib_dir(), c(embedded = "foobar"))
})

test_that("private_lib_dir 2", {
  mockery::stub(private_lib_dir, "file.exists", FALSE)
  withr::local_envvar(c(PAK_PRIVATE_LIBRARY = "foobar2"))
  expect_equal(private_lib_dir(), "foobar2")
})

test_that("private_lib_dir 3", {
  mockery::stub(private_lib_dir, "file.exists", FALSE)
  withr::local_envvar(c(PAK_PRIVATE_LIBRARY = NA_character_))
  mockery::stub(private_lib_dir, "user_cache_dir", "cached-dir")
  mockery::stub(private_lib_dir, "get_minor_r_version", "4.3")
  mockery::stub(private_lib_dir, "R.Version", list(arch = "arm64"))
  expect_true(private_lib_dir() %in% c(
    "cached-dir\\lib\\4.3\\arm64",
    "cached-dir/lib/4.3/arm64"
  ))
})
