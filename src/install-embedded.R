opts <- function() {
  paste(
    "--without-keep.source",
    "--no-html",
    "--no-help",
    "--no-data",
    "--no-docs",
    if (Sys.getenv("CROSS_COMPILING") == "yes") "--no-test-load",
    if (getRversion() >= "3.6") "--strip",
    if (getRversion() >= "3.6") "--no-staged-install"
  )
}

`%||%` <- function(l, r) if (is.null(l)) r else l

rimraf <- function(path) {
  unlink(path, force = TRUE, recursive = TRUE)
}

get_lib <- function(lib) {
  lib <- lib %||% file.path(Sys.getenv("R_PACKAGE_DIR"), "library")
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  lib
}

install_one <- function(pkg, lib = NULL) {
  lib <- get_lib(lib)

  cat("\nCompiling", pkg, "\n")
  suppressWarnings(suppressMessages(utils::capture.output(install.packages(
    paste0("library/", pkg),
    lib = lib,
    repos = NULL,
    type = "source",
    INSTALL_opts = opts()
  ))))

  if (!file.exists(file.path(lib, pkg))) {
    stop("FAILED")
  }

  invisible()
}

install_order <- function() {
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

  pkgs
}

install_all <- function(lib = NULL) {
  pkgs <- install_order()
  if (Sys.getenv("CROSS_COMPILING") == "yes") {
    lib <- get_lib(lib)
    for (pkg in pkgs) {
      install_one(pkg, lib = paste0(lib, "-", pkg))
    }
    for (pkg in pkgs) {
      file.rename(file.path(paste0(lib, "-", pkg), pkg), file.path(lib, pkg))
      unlink(file.path(paste0(lib, "-", pkg)), recursive = TRUE)
    }
  } else {
    for (pkg in pkgs) install_one(pkg, lib = lib)
  }
  file.create("DONE")
  invisible()
}

get_ver <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  desc <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc)) {
    return(NA_character_)
  }
  dsc <- read.dcf(desc)
  ver <- package_version(dsc[, "Version"])
  devver <- ver[1, 4]
  if (!is.na(devver) && devver >= 90000) {
    if ("RemoteSha" %in% colnames(dsc)) {
      sha <- dsc[, "RemoteSha"]
      return(sha)
    }
  }

  as.character(ver)
}

update_all <- function(lib = NULL) {
  lib <- get_lib(lib)
  cat("Updating dev lib at", lib, "\n")
  pkgs <- install_order()
  for (pkg in pkgs) {
    oldver <- get_ver(file.path(lib, pkg))
    newver <- get_ver(file.path("library", pkg))
    if (is.na(newver)) stop("Cannot find embedded ", pkg)
    if (is.na(oldver)) {
      message("Adding ", pkg)
      rimraf(file.path(lib, pkg)) # in case it is a broken install
      install_one(pkg, lib)
    } else if (oldver != newver) {
      message("Updating ", pkg, " ", oldver, " -> ", newver)
      rimraf(file.path(lib, pkg))
      install_one(pkg, lib)
    }
  }
}

load_all <- function() {
  args <- commandArgs(TRUE)
  if (length(args) < 2) {
    stop("Usage: install-embedded.R [ --load-all library-dir ]")
  }
  update_all(args[2])
}

parse_platforms <- function(args) {
  build <- if (grepl("^--build=", args[1])) {
    substr(args[1], 9, 1000)
  }
  target <- if (grepl("^--target=", args[2])) {
    substr(args[2], 10, 1000)
  }
  list(
    load_all = "--load-all" %in% args,
    current = R.Version()$platform,
    build = build %||% NA_character_,
    target = target %||% NA_character_
  )
}

install_embedded_main <- function() {
  unlink("DONE")
  # Parse platforms
  pl <- parse_platforms(commandArgs(TRUE))

  # From .onLoad()'s call after load_all(), create separate lib
  if (pl[["load_all"]]) {
    return(load_all())
  }

  # From load_all()'s ./configure call, do nothing
  if (Sys.getenv("DEVTOOLS_LOAD") == "pak") {
    # TODO: we could do this here, if we get rid of rappdirs and
    # write our on code to determine the user cache directory.
    cat("Delayed embedding dependencies in `pkgload::load_all()`.\n")
    file.create("DONE")
    return(invisible())
  }

  # Otherwise R CMD INSTALL, check if we are cross-compiling
  cat("Current platform:", pl$current, "\n")
  cat("Build platform: ", pl$build, "\n")
  cat("Target platform: ", pl$target, "\n")

  if (grepl("linux", pl$current) &&
    grepl("darwin", pl$target)) {
    # PPM: current is Linux, target is (some) Darwin
    Sys.setenv(CROSS_COMPILING = "yes")
  } else if (grepl("x86_64-apple-darwin", pl$current) &&
    grepl("aarch64-apple-darwin", pl$target)) {
    # Current is Darwin x86_64, target is Darwin aarch64
    Sys.setenv(CROSS_COMPILING = "yes")
    Sys.setenv(R_MAKEVARS_USER = file.path(
      getwd(),
      "Makevars-macos-arm64"
    ))
  } else if (grepl("x86_64.*linux", pl$current) &&
    grepl("aarch64.*linux, pl$target")) {
    # Current is Linux x86_64, target is Linux aarch64
    Sys.setenv(CROSS_COMPILING = "yes")
  } else {
    # Not cross compiling or cross compiling handled externally
  }

  install_all()
}

if (is.null(sys.calls())) {
  install_embedded_main()
}
