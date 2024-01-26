test_that("load_all_private", {
  pkg_data[["ns"]] <- NULL
  load_all_private()
  expect_equal(
    embedded_call("pkgdepends", "lib_status"),
    pkg_data[["ns"]][["pkgdepends"]][["lib_status"]]
  )
})

test_that("embedded_call", {
  expect_equal(
    embedded_call("pkgdepends", "lib_status"),
    pkg_data[["ns"]][["pkgdepends"]][["lib_status"]]
  )
})
