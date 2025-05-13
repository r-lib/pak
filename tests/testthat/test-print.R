cli::test_that_cli(configs = c("plain", "fancy"), "print_install_summary", {
  d <- data.frame(
    stringsAsFactors = FALSE,
    direct = c(TRUE, FALSE, FALSE, FALSE),
    type = c("deps", "standard", "cran", "cran"),
    lib_status = c("new", "update", "current", "update"),
    download_status = c("Had", "Got", "Had", "Had"),
    file_size = c(NA_integer_, 112 * 1024, NA_integer_, 1200 * 1024)
  )
  attr(d, "total_time") <- as.difftime(12.123, units = "secs")

  while (!is.null(cli::default_app())) cli::stop_app()

  expect_snapshot(print_install_summary(d))

  d$type[1] <- "local"
  expect_snapshot(print_install_summary(d))

  d$direct <- TRUE
  expect_snapshot(print_install_summary(d))

  d$lib_status <- c("new", "update", "new", "update")
  expect_snapshot(print_install_summary(d))

  d$lib_status <- c("new", "current", "new", "current")
  expect_snapshot(print_install_summary(d))
})
