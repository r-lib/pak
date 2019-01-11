
context("solve pieces")

test_that("remotes_i_create_lp_init", {
  pkgs <- read_fixture("resolution-simple.rds")
  lp <- remotes_i_lp_init(pkgs, policy = "lazy")
  expect_equal(lp$num_candidates, 2)
  expect_equal(lp$num_direct, 1)
  expect_equal(lp$total, 3)
  expect_equal(lp$conds, list())
  expect_equal(lp$pkgs, pkgs)
  expect_equal(lp$policy, "lazy")
  expect_equal(lp$packages, "pkgconfig")
  expect_equal(lp$direct_packages, "pkgconfig")
  expect_equal(lp$indirect_packages, character())
  expect_equal(lp$ruled_out, integer())
})

test_that("remotes_i_lp_objectives lazy policy", {
  pkgs <- read_fixture("resolution-simple.rds")
  lp0 <- remotes_i_lp_init(pkgs, policy = "lazy")
  lp <- remotes_i_lp_objectives(lp0)
  expect_equal(lp0[setdiff(names(lp0), "obj")], lp[setdiff(names(lp), "obj")])
  expect_true(lp$obj[which(pkgs$platform != "source")] <
              lp$obj[which(pkgs$platform == "source")])
  expect_equal(lp$obj[3], solve_dummy_obj)
})

test_that("remotes_i_lp_objectives upgrade policy", {
  ## TODO
})

test_that("remotes_i_lp_no_multiples", {
  pkgs <- read_fixture("resolution-progress.rds")
  lp <- remotes_i_lp_init(pkgs, "lazy")
  lp <- remotes_i_lp_no_multiples(lp)
  expect_equal(
    vcapply(lp$conds, "[[", "type"),
    c(rep("exactly-once", length(lp$direct_packages)),
      rep("at-most-once", length(lp$indirect_packages)))
  )
  prvar <- which(pkgs$package == "progress")
  expect_equal(
    lp$conds[[1]],
    structure(
      list(vars = c(prvar, nrow(pkgs) + 1L), coef = c(1, 1, 1), op = "==",
           rhs = 1, type = "exactly-once", note = NULL))
  )
  prvar2 <- which(pkgs$package == "assertthat")
  expect_equal(
    lp$conds[[2]],
    structure(list(vars = prvar2, coef = c(1, 1), op = "<=", rhs = 1,
                   type = "at-most-once", note = NULL))
  )
})

test_that("remotes_i_lp_satisfy_direct", {
  pkgs <- read_fixture("resolution-gh-vs-cran.rds")
  lp <- remotes_i_lp_init(pkgs, "lazy")
  lp <- remotes_i_lp_satisfy_direct(lp)
  expect_equal(
    vcapply(lp$conds, "[[", "type"),
    rep("satisfy-refs", 4)
  )
  expect_equal(
    sort(viapply(lp$conds, "[[", "vars")),
    c(1, 2, 3, 3)
  )
  expect_equal(vcapply(lp$conds, "[[", "op"), rep("==", 4))
  expect_equal(vdapply(lp$conds, "[[", "rhs"), rep(0, 4))
})

test_that("remotes_i_lp_failures", {
  ## TODO
})

test_that("remotes_i_lp_prefer_installed", {
  ## TODO
})

test_that("remotes_i_lp_prefer_binaries", {
  ## TODO
})

test_that("remotes_i_lp_dependencies", {
  pkgs <- read_fixture("resolution-progress.rds")
  lp <- remotes_i_lp_init(pkgs, "lazy")
  lp <- remotes_i_lp_dependencies(lp)
  expect_equal(length(lp$conds), 16)
  expect_equal(
    digest::digest(lp$conds[[3]]),
    "8f4367ad78bc677389eaeece32e2f932"
  )
})
