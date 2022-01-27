
test_that("pkg_upgrade errors", {
  # Only for one package for now
  expect_error(pkg_upgrade(c("f", "b")), "length(pkg) == 1", fixed = TRUE)

  # The rest is tested in get_installed_ref_internal
})

cli::test_that_cli("pkg_upgrade", {
  mockery::stub(pkg_upgrade, "pkg_install", function(...) invisible(NULL))

  mockery::stub(pkg_upgrade, "get_installed_ref", NA_character_)
  expect_snapshot(
    pkg_upgrade("foo")
  )

  mockery::stub(pkg_upgrade, "get_installed_ref", c(CRAN = "foo"))
  expect_snapshot(
    pkg_upgrade("foo")
  )

  mockery::stub(pkg_upgrade, "get_installed_ref", c(Bioconductor = "foo"))
  expect_snapshot(
    pkg_upgrade("foo")
  )

  mockery::stub(pkg_upgrade, "get_installed_ref", "repo/foo@master")
  expect_snapshot(
    pkg_upgrade("foo")
  )
})

test_that("get_installed_ref_internal", {
  expect_error(
    get_installed_ref_internal("foo/bar"),
    "not a valid package name"
  )

  mockery::stub(get_installed_ref_internal, "find.package", function(...) {
    stop("not here")
  })
  expect_null(get_installed_ref_internal("repo"))

  mockery::stub(get_installed_ref_internal, "find.package", function(...) {
    test_path("fixtures", "get-ref-internal", "1")
  })
  expect_equal(get_installed_ref_internal("repo"), "user/repo@branch")

  mockery::stub(get_installed_ref_internal, "find.package", function(...) {
    test_path("fixtures", "get-ref-internal", "2")
  })
  expect_equal(get_installed_ref_internal("repo"), c(CRAN = "repo"))

  mockery::stub(get_installed_ref_internal, "find.package", function(...) {
    test_path("fixtures", "get-ref-internal", "3")
  })
  expect_equal(get_installed_ref_internal("repo"), c(Bioconductor = "repo"))

  mockery::stub(get_installed_ref_internal, "find.package", function(...) {
    test_path("fixtures", "get-ref-internal", "4")
  })
  expect_equal(get_installed_ref_internal("repo"), NA_character_)
})
