test_that("load_all_private", {
  # we need to fix a finalizer, probably in cli for this to work,
  # otherwise it crashes on older R versions
  skip("may cause crash")
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
