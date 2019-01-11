
context("solve policies")

test_that("newer version preferred", {
  pkgs <- make_fake_resolution(
    `aa` = list(version = "1-0-1", platform = "source", direct = TRUE),
    `installed::/tmp/aa` = list(version = "1.2.0")
  )
  lp <- remotes_i_create_lp_problem(pkgs, policy = "upgrade")
  sol <- remotes_i_solve_lp_problem(lp)
  expect_true(sol$objval < 1000)
  expect_equal(as.logical(sol$solution), c(FALSE, TRUE, FALSE))

  pkgs <- make_fake_resolution(
    `aa` = list(version = "1-0-1", platform = "source", direct = TRUE),
    `installed::/tmp/aa` = list(version = "1.0.0")
  )
  lp <- remotes_i_create_lp_problem(pkgs, policy = "upgrade")
  sol <- remotes_i_solve_lp_problem(lp)
  expect_true(sol$objval < 1000)
  expect_equal(as.logical(sol$solution), c(TRUE, FALSE, FALSE))
})

test_that("if newer fails, older is used", {
  pkgs <- make_fake_resolution(
    `aa` = list(version = "0.9.9", direct = TRUE),
    `cran::aa` = list(version = "1-0-1", status = "FAILED"),
    `installed::/tmp/aa` = list(version = "1.0.0")
  )
  lp <- remotes_i_create_lp_problem(pkgs, policy = "upgrade")
  sol <- remotes_i_solve_lp_problem(lp)
  expect_true(sol$objval < 1000)
  expect_equal(as.logical(sol$solution), c(FALSE, FALSE, TRUE, FALSE))

})

test_that("newer version if preferred in the dependencies", {
  pkgs <- make_fake_resolution(
    `a0` = list(direct = TRUE, deps = list(make_fake_deps(Imports = "aa"))),
    `aa` = list(version = "1-0-1", platform = "source"),
    `installed::/tmp/aa` = list(version = "1.0.0")
  )
  lp <- remotes_i_create_lp_problem(pkgs, policy = "upgrade")
  sol <- remotes_i_solve_lp_problem(lp)
  expect_true(sol$objval < 1000)
  expect_equal(as.logical(sol$solution), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("binaries are still preferred over source", {
  pkgs <- make_fake_resolution(
    `aa` = list(version = "1.0.0", direct = TRUE, platform = "macos"),
    `cran::aa` = list(version = "1.0.0", platform = "source"),
    `installed::/tmp/aa` = list(version = "0.9.9")
  )
  lp <- remotes_i_create_lp_problem(pkgs, policy = "upgrade")
  sol <- remotes_i_solve_lp_problem(lp)
  expect_true(sol$objval < 1000)
  expect_equal(as.logical(sol$solution), c(TRUE, FALSE, FALSE, FALSE))

  pkgs <- make_fake_resolution(
    `cran::aa` = list(version = "1.0.0", platform = "source"),
    `aa` = list(version = "1.0.0", direct = TRUE, platform = "macos"),
    `installed::/tmp/aa` = list(version = "0.9.9")
  )
  lp <- remotes_i_create_lp_problem(pkgs, policy = "upgrade")
  sol <- remotes_i_solve_lp_problem(lp)
  expect_true(sol$objval < 1000)
  expect_equal(as.logical(sol$solution), c(FALSE, TRUE, FALSE, FALSE))
})

test_that("installed is still preferred over binaries", {
  pkgs <- make_fake_resolution(
    `aa` = list(version = "1.0.0", direct = TRUE, platform = "macos"),
    `cran::aa` = list(version = "1.0.0", platform = "source"),
    `installed::/tmp/aa` = list(version = "1.0.0")
  )
  lp <- remotes_i_create_lp_problem(pkgs, policy = "upgrade")
  sol <- remotes_i_solve_lp_problem(lp)
  expect_true(sol$objval < 1000)
  expect_equal(as.logical(sol$solution), c(FALSE, FALSE, TRUE, FALSE))

  pkgs <- make_fake_resolution(
    `cran::aa` = list(version = "1.0.0", platform = "source"),
    `aa` = list(version = "1.0.0", direct = TRUE, platform = "macos"),
    `installed::/tmp/aa` = list(version = "1.0.0")
  )
  lp <- remotes_i_create_lp_problem(pkgs, policy = "upgrade")
  sol <- remotes_i_solve_lp_problem(lp)
  expect_true(sol$objval < 1000)
  expect_equal(as.logical(sol$solution), c(FALSE, FALSE, TRUE, FALSE))

  pkgs <- make_fake_resolution(
    `installed::/tmp/aa` = list(version = "1.0.0"),
    `cran::aa` = list(version = "1.0.0", platform = "source"),
    `aa` = list(version = "1.0.0", direct = TRUE, platform = "macos")
  )
  lp <- remotes_i_create_lp_problem(pkgs, policy = "upgrade")
  sol <- remotes_i_solve_lp_problem(lp)
  expect_true(sol$objval < 1000)
  expect_equal(as.logical(sol$solution), c(TRUE, FALSE, FALSE, FALSE))

  pkgs <- make_fake_resolution(
    `cran::aa` = list(version = "1.0.0", platform = "source"),
    `installed::/tmp/aa` = list(version = "1.0.0"),
    `aa` = list(version = "1.0.0", direct = TRUE, platform = "macos")
  )
  lp <- remotes_i_create_lp_problem(pkgs, policy = "upgrade")
  sol <- remotes_i_solve_lp_problem(lp)
  expect_true(sol$objval < 1000)
  expect_equal(as.logical(sol$solution), c(FALSE, TRUE, FALSE, FALSE))
})
