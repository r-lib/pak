# These functions are only used for pak development

create_dev_lib <- function() {
  if (is.null(asNamespace("pak")[[".__DEVTOOLS__"]])) {
    stop("This function only works if pak was loaded via pkgload::load_all()")
  }

  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  inst_script <- file.path(find.package("pak"), "src", "install-embedded.R")
  lib <- private_lib_dir()

  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(dirname(inst_script))

  system2(rscript, c("--vanilla", "install-embedded.R", "--load-all", lib))

  # if a test coverage session, then load dependencies now
  if (Sys.getenv("TEST_COVERAGE_PAK") == "true") {
    message("Instrumenting R code for test coverage")
    # Need to monkey patch covr
    key <- function(x) {
      paste(collapse = ":", c(get_source_filename(x, full.names = TRUE), x))
    }
    environment(key) <- asNamespace("covr")
    do.call("unlockBinding", list("key", asNamespace("covr")))
    assign("key", key, envir = asNamespace("covr"))

    load_all_private()
    asNamespace("covr")$trace_environment(asNamespace("pak"))
  }

  invisible()
}
