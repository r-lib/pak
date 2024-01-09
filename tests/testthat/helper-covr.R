covr <- function(filter = NULL, pre_clean = TRUE, quiet = TRUE, ...) {
  # TODO: check if dependencies were installed w/ coverage support
  # Clean up trace files --------------------------------------------------
  if (pre_clean) {
    rtrace <- list.files(pattern = "^covr_trace_")
    unlink(rtrace)
    gcda <- list.files(pattern = "[.]gcda$", recursive = TRUE)
    gcov <- list.files(pattern = "[.]gcov$", recursive = TRUE)
    unlink(c(gcda, gcov))
  }

  # Run tests -------------------------------------------------------------
  testthat::test_dir("tests/testthat", filter = filter, ...)

  # Save R coverage -------------------------------------------------------
  asNamespace("covr")$save_trace(".")

  # Save C coverage -------------------------------------------------------
  # The rest do not have a gcov_flush hook
  # TODO: add hook to jsonlite, lpSolve, filelock, zip, curl, ps, processx.
  .Call(pkg_data$ns$cli$clic__gcov_flush)
  .Call(pkg_data$ns$pkgcache$pkgcache__gcov_flush)

  # Load and merge trace files
  rtrace <- list.files(pattern = "^covr_trace_")
  message("Reading coverage of R code")
  rcov <- asNamespace("covr")$merge_coverage(rtrace)
  message("Reading coverage of compiled code")
  ccov <- run_gcov(".", quiet = quiet)
  cov <- c(rcov, ccov)
  fn <- lapply(cov, "[[", "functions")
  ok <- vapply(fn, is.character, logical(1))
  cov <- cov[ok]

  cov <- asNamespace("covr")$as_coverage(
    cov,
    package = list(package = "pak"),
    root = getwd()
  )
  cov
}

run_gcov <- function(path, quiet = TRUE, clean = TRUE) {
  gcov_inputs <- list.files(
    path,
    pattern = "[.]gcno$",
    recursive = TRUE,
    full.names = TRUE
  )
  run_gcov_one <- function(src) {
    cwd <- getwd()
    on.exit(setwd(cwd), add = TRUE)
    setwd(dirname(src))
    asNamespace("covr")$system_check(
      "gcov",
      args = c(basename(src), "-p"),
      quiet = quiet,
      echo = !quiet
    )
    gcov_outputs <- list.files(path, pattern = "[.]gcov$")
    unlist(
      lapply(gcov_outputs, asNamespace("covr")$parse_gcov),
      recursive = FALSE
    )
  }
  res <- asNamespace("covr")$compact(unlist(
    lapply(gcov_inputs, run_gcov_one),
    recursive = FALSE
  ))

  if (!length(res) & length(gcov_inputs)) {
    warning("parsed gcov output was empty")
  }
  res
}
