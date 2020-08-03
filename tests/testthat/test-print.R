
test_that("print_package_list", {
  withr::local_options(list(rlib_interactive = TRUE, cli.unicode = FALSE))
  expect_message(
    print_package_list(letters[1:5]),
    "  a, b, c, d and e"
  )
  expect_message(
    print_package_list(letters[1:5], new_version = 1:5),
    "  a (1), b (2), c (3), d (4) and e (5)",
    fixed = TRUE
  )
  expect_message(
    print_package_list(letters[1:5], new_version = 1:5, old_version = 0:4),
    "  a (0 > 1), b (1 > 2), c (2 > 3), d (3 > 4) and e (4 > 5)",
    fixed = TRUE
  )
})
