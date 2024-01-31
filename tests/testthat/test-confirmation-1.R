test_that("should_ask_confirmation", {
  expect_true(should_ask_confirmation(
    list(lib_status = c("foo", "bar", "update"))
  ))
  expect_false(should_ask_confirmation(
    list(lib_status = c("foo", "bar", "foobar"))
  ))
  expect_false(should_ask_confirmation(
    list(lib_status = character())
  ))
})

test_that("print_install_details", {
  skip_on_cran()
  local <- local_tempdir()
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

  # updates
  mkdirp(file.path(local, "pkg1"))
  writeLines(
    c("Package: pkg1", "Version: 0.0.0"),
    file.path(local, "pkg1", "DESCRIPTION")
  )
  sol <- pkgdepends$new_pkg_installation_proposal("pkg1", config)
  suppressMessages(sol$solve())
  expect_snapshot(
    print_install_details(sol, local, character()),
    transform = transform_bytes
  )
  unlink(file.path(local, "pkg1"), recursive = TRUE)

  # cached package, or packages
  dl <- local_tempdir()
  suppressMessages(pkg_download("pkg2", dl, dependencies = TRUE))
  sol <- pkgdepends$new_pkg_installation_proposal("pkg1", config)
  suppressMessages(sol$solve())
  expect_snapshot(
    print_install_details(sol, local, character()),
    transform = transform_bytes
  )
  sol <- pkgdepends$new_pkg_installation_proposal("pkg2", config)
  suppressMessages(sol$solve())
  expect_snapshot(
    print_install_details(sol, local, character()),
    transform = transform_bytes
  )

  # unknown download size
  if (Sys.getenv("PAK_EXTRA_TESTS") == "true") {
    skip("Can't run in extra tests.")
  }
  xpkgs <- dcf("Package: pkgu\nVersion: 1.0.0\n")
  xrepo <- webfakes::local_app_process(cran_app(
    xpkgs,
    options = list(no_metadata = TRUE)
  ))
  withr::local_options(repos = c(getOption("repos"), XTRA = xrepo$url()))

  sol <- pkgdepends$new_pkg_installation_proposal("pkgu", config)
  suppressMessages(sol$solve())
  expect_snapshot(
    print_install_details(sol, local, character()),
    transform = transform_bytes
  )

  # some cached, some not
  sol <- pkgdepends$new_pkg_installation_proposal(c("pkg2", "pkg3"), config)
  suppressMessages(sol$solve())
  expect_snapshot(
    print_install_details(sol, local, character()),
    transform = transform_bytes
  )

  # some cached, some not, some sizes unknown
  sol <- pkgdepends$new_pkg_installation_proposal(
    c("pkg2", "pkg3", "pkgu"),
    config
  )
  suppressMessages(sol$solve())
  expect_snapshot(
    print_install_details(sol, local, character()),
    transform = transform_bytes
  )
})

test_that("get_confirmation", {
  stub(get_confirmation, "readline", "")
  expect_silent(get_confirmation("yes"))
  stub(get_confirmation, "readline", "y")
  expect_silent(get_confirmation("yes"))
  stub(get_confirmation, "readline", "Y")
  expect_silent(get_confirmation("yes"))
  stub(get_confirmation, "readline", "yes")
  expect_silent(get_confirmation("yes"))

  stub(get_confirmation, "readline", "n")
  expect_error(get_confirmation("yes", msg = "nope"), "nope")
})

test_that("get_confirmation2", {
  stub(get_confirmation2, "readline", "")
  expect_true(get_confirmation2())
  stub(get_confirmation2, "readline", "y")
  expect_true(get_confirmation2())
  stub(get_confirmation2, "readline", "Y")
  expect_true(get_confirmation2())
  stub(get_confirmation2, "readline", "yes")
  expect_true(get_confirmation2())

  stub(get_confirmation2, "readline", "n")
  expect_false(get_confirmation2())
})

test_that("get_answer", {
  stub(get_answer, "readline", "foo")
  expect_equal(get_answer(c("foo", "bar")), "foo")

  ans <- c("foo", "bar")
  idx <- 1
  stub(get_answer, "readline", function(prompt = "") {
    cat(prompt)
    cat(ans[idx], "\n", sep = "")
    idx <<- idx + 1L
    ans[idx - 1L]
  })
  expect_snapshot(
    res <- get_answer(c("this", "bar"))
  )
  expect_equal(res, "bar")
})

test_that("offer_restart", {
  stub(
    offer_restart,
    "rstudio_detect",
    list(type = "not_rstudio")
  )
  expect_snapshot(offer_restart())

  stub(
    offer_restart,
    "rstudio_detect",
    list(type = "rstudio_console")
  )
  stub(offer_restart, "get_answer", "1")
  stub(offer_restart, "rstudioapi::restartSession", "restart")
  expect_snapshot(offer_restart())

  stub(offer_restart, "get_answer", "2")
  stub(offer_restart, "save.image", NULL)
  stub(offer_restart, "rstudioapi::restartSession", "save-restart")
  expect_snapshot(offer_restart())

  stub(offer_restart, "get_answer", "3")
  expect_snapshot(expect_equal(offer_restart(), "OK"))
})
