test_that("pkg_deps_explain", {
  skip_on_cran()
  local <- withr::local_tempdir()
  setup_fake_apps()

  suppressMessages(meta_list())
  expect_snapshot(pkg_deps_explain("pkg3", "pkg1"))
  expect_snapshot(pkg_deps_explain("pkg1", "pkg3"))
})
