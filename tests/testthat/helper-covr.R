covr <- function(filter = NULL, pre_clean = TRUE, ...) {
  # TODO: check if dependencies were installed w/ coverage support
  # Clean up trace files --------------------------------------------------
  if (pre_clean) {
    rtrace <- list.files(pattern = "^covr_trace_")
    unlink(rtrace)
    gcda <- list.files(pattern = "[.]gcda$", recursive = TRUE)
    gcov <- list.files(pattern = "[.]gcov$", recursive = TRUE)
    unlink(c(gcda, gcov))
    asNamespace("covrlabs")$reset_counters()
  }

  # Run tests -------------------------------------------------------------
  testthat::test_dir("tests/testthat", filter = filter, ...)

  # Save R coverage -------------------------------------------------------
  asNamespace("covrlabs")$save_trace()

  # Save C coverage -------------------------------------------------------
  # The rest do not have a gcov_flush hook
  # TODO: add hook to jsonlite, lpSolve, filelock, zip, curl, ps, processx.
  .Call(pkg_data$ns$cli$clic__gcov_flush)
  .Call(pkg_data$ns$pkgcache$pkgcache__gcov_flush)

  # Load and merge trace files
  message("Parsing and reading coverage")
  cov <- asNamespace("covrlabs")$parse_coverage()

  cov
}
