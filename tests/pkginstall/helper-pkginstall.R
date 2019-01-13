local_binary_package <- function(pkgname, ..., envir = parent.frame()) {

  # All arguments must be named
  args <- list(...)
  stopifnot(length(args) == 0 || rlang::is_named(args))

  d <- create_temp_dir()
  pkgdir <- file.path(d, pkgname)
  dir.create(pkgdir)
  nms <- names(args)
  for (i in seq_along(args)) {
    dir.create(file.path(pkgdir, dirname(nms[[i]])), showWarnings = FALSE, recursive = TRUE)
    withr::with_connection(list(con = file(file.path(pkgdir, nms[[i]]), open = "wb")), {
      writeLines(args[[i]], con, sep = "\n")
    })
  }

  filename <- file.path(d, glue::glue("{pkgname}.tgz"))
  withr::with_dir(
    dirname(filename),
    utils::tar(basename(filename), pkgname, compression = "gzip")
  )

  # We do not want to unlink files if we are calling this from the R console,
  # useful when debugging.
  is_globalenv <- identical(envir, globalenv())
  if (!is_globalenv) {
    withr::defer(unlink(d, recursive = TRUE), envir = envir)
  }
  filename
}

binary_test_package <- function(name) {

  binary <- switch(sysname(),
    windows = glue::glue("{name}.zip"),
    linux = glue::glue("{name}_R_x86_64-pc-linux-gnu.tar.gz"),
    mac = glue::glue("{name}.tgz"),
    skip(glue::glue("Cannot test on {sysname()}"))
    )
  if (!file.exists(binary)) {
    pkgbuild::build(sub("_.*$", "", name), binary = TRUE, quiet = TRUE)
  }
  binary
}

expect_error_free <- function(...) {
  testthat::expect_error(..., regexp = NA)
}

if (is_loaded("foo")) {
  unloadNamespace("foo")
}

dummy_worker_process <- R6::R6Class(
  "dummy_worker_process",
  inherit = callr::r_process,
  public = list(
    initialize = function(...) {
      super$initialize(...)
    },
    get_built_file = function() NA_character_
  )
)

make_dummy_worker_process <- function(n_iter = 10, sleep = 1, status = 0) {
  n_iter; sleep; status
  function(...) {
    dummy_worker_process$new(callr::r_process_options(
      func = function(n_iter, sleep, status) {
                                        # nocov start
        for (i in seq_len(n_iter)) {
          cat("out ", i, "\n", sep = "")
          message("err ", i)
          Sys.sleep(sleep)
        }
        status
        .GlobalEnv$.Last <- function() {
          rm(list = ".Last", envir = .GlobalEnv)
          quit(save = "no", status = status)
        }
                                        # nocov end
      },
      args = list(n_iter = n_iter, sleep = sleep, status = status)
      ))
  }
}

make_install_plan <- function(ref, lib = .libPaths()[1]) {
  r <- remotes()$new(ref, lib = lib)
  r$resolve()
  r$solve()
  r$download_solution()
  r$get_install_plan()
}

skip_in_covr <- function() {
  if (Sys.getenv("R_COVR", "") != "") skip("In covr")
}
