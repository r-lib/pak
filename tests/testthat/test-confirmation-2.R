test_that("print_sysreqs_details", {
  # no sysreqs
  expect_silent(print_sysreqs_details(list(get_sysreqs = function() NULL)))

  # sysreqs already installed
  srq <- list(miss = character(), upd = character(), inst = "libcurl")
  prop <- list(
    get_sysreqs = function() {
      srq
    },
    get_config = function() {
      list(get = function(...) TRUE)
    },
    show_sysreqs = function() {}
  )
  expect_snapshot(print_sysreqs_details(prop))

  # sysreqs will be installed
  srq$miss <- "libxml2"
  expect_snapshot(print_sysreqs_details(prop))

  # sysreqs needed, but won't be installed
  prop$get_config <- function() list(get = function(...) FALSE)
  expect_snapshot(print_sysreqs_details(prop))
})
