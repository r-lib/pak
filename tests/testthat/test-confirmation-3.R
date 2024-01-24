test_that("print_install_details, warn_for_loaded_packages on windows", {
  skip_on_cran()
  local <- withr::local_tempdir()
  setup_fake_apps()
  cache_clean()
  load_all_private()
  pkgdepends <- pkg_data[["ns"]][["pkgdepends"]]
  config <- list(library = local)
  mkdirp(local)

  sol <- pkgdepends$new_pkg_installation_proposal("pkg2", config)
  suppressMessages(sol$solve())
  expect_snapshot(
    print_install_details(sol, local, character()),
    transform = transform_bytes
  )

  mockery::stub(print_install_details, "get_os", "win")
  mockery::stub(print_install_details, "warn_for_loaded_packages", "foo")

  expect_equal(
    suppressMessages(print_install_details(sol, local, NULL)$loaded_status),
    "foo"
  )
})
