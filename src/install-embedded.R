lib <- file.path(Sys.getenv("R_PACKAGE_DIR"), "library")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)

opts <- paste(
  "--without-keep.source",
  "--no-html",
  "--no-help",
  "--no-data",
  "--strip",
  if (getRversion() >= "3.6") "--no-staged-install"
)

install_one <- function(pkg) {
  cat("\nCompiling", pkg, "\n")
  suppressWarnings(suppressMessages(utils::capture.output(install.packages(
    paste0("library/", pkg),
    lib = lib,
    repos = NULL,
    type = "source",
    INSTALL_opts = opts
  ))))

  invisible()
}

install_all <- function() {
  ## TODO: look up the correct order

  # No deps
  install_one("R6")
  install_one("cli")
  install_one("crayon")
  install_one("curl")
  install_one("distro")
  install_one("filelock")
  install_one("glue")
  install_one("jsonlite")
  install_one("lpSolve")
  install_one("parsedate")
  install_one("prettyunits")
  install_one("ps")
  install_one("rappdirs")
  install_one("rprojroot")
  install_one("zip")

  # ps, R6
  install_one("processx")

  # processx, R6
  install_one("callr")

  # cli, R6, rprojroot
  install_one("desc")

  # callr, cli, crayon, desc, prettyunits, processx, R6, rprojroot
  install_one("pkgbuild")

  # callr, cli, curl, filelock, jsonlite, prettyunis, processx, R6, rappdirs
  install_one("pkgcache")

  # curl, jsonlite, parsedate, prettyunits
  install_one("pkgsearch")

  # callr, cli, curl, desc, filelock, glue, jsonlite, lpSolve, pkgbuild,
  # pkgcache, prettyunits, processx, ps, R6, rprojroot, zip
  install_one("pkgdepends")
}

# print(Sys.getenv())

install_all()

