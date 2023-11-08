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

  if (!file.exists(file.path(lib, pkg))) {
    stop("FAILED")
  }

  invisible()
}

install_all <- function() {
  ## TODO: look up the correct order

  pkgs <- c(
    # no deps
    "R6", "cli", "crayon", "curl", "distro", "filelock", "glue", "jsonlite",
    "lpSolve", "parsedate", "prettyunits", "ps", "rappdirs", "rprojroot",
    "zip",
    # ps, R6
    "processx",
    # processx, R6
    "callr",
    # cli, R6, rprojroot
    "desc",
    # callr, cli, crayon, desc, prettyunits, processx, R6, rprojroot
    "pkgbuild",
    # callr, cli, curl, filelock, jsonlite, prettyunis, processx, R6, rappdirs
    "pkgcache",
    # curl, jsonlite, parsedate, prettyunits
    "pkgsearch",
    # callr, cli, curl, desc, filelock, glue, jsonlite, lpSolve, pkgbuild,
    # pkgcache, prettyunits, processx, ps, R6, rprojroot, zip
    "pkgdepends"
  )

  for (pkg in pkgs) install_one(pkg)
}

install_pkgload <- function() {
  message("Not embedding dependencies in `pkgload::load_all()`.")
}

install_embedded_main <- function() {
  if (Sys.getenv("DEVTOOLS_LOAD") == "pak") {
    install_pkgload()
  } else {
    install_all()
  }
  invisible()
}

if (is.null(sys.calls())) {
  install_embedded_main()
}
