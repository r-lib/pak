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
}
