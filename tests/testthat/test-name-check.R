test_that("pkg_name_check", {
  ret <- readRDS(test_path("fixtures/name-check.rds"))
  mockery::stub(
    pkg_name_check,
    "embedded_call",
    function(...) function(...) ret[[1]]
  )
  expect_snapshot(pkg_name_check("tools"))
  mockery::stub(
    pkg_name_check,
    "embedded_call",
    function(...) function(...) ret[[2]]
  )
  expect_snapshot(pkg_name_check("tools", "urban"))
})

test_that("format.pak_pkg_name_check, print.pak_pkg_name_check", {
  ret <- readRDS(test_path("fixtures/name-check.rds"))
  class(ret[[1]]) <- c("pak_pkg_name_check", class(ret[[1]]))
  expect_snapshot(print(ret[[1]]))
  class(ret[[2]]) <- c("pak_pkg_name_check", class(ret[[2]]))
  expect_snapshot(print(ret[[2]]))
})
