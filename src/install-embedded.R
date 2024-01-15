install_opts <- function() {
  cov <- Sys.getenv("TEST_COVERAGE_PAK") == "true"
  paste(
    if (cov) "--with-keep.source" else "--without-keep.source",
    if (cov) "--preclean",
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

# For `load_all()` the `lib` argument is passed in.

get_lib <- function(lib = NULL) {
  lib <- lib %||% file.path(Sys.getenv("R_PACKAGE_DIR"), "library")
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  lib
}

parse_built <- function(built) {
  structure(
    as.list(trimws(strsplit(built, ";", fixed = TRUE)[[1]])),
    names = c("R", "Platform", "Date", "OStype")
  )
}

deparse_built <- function(built) {
  paste(as.character(unlist(built)), collapse = "; ")
}

update_description_build <- function(path, platform) {
  source_path <- file.path(path, "DESCRIPTION")
  source_desc <- read.dcf(source_path)
  source_built <- parse_built(source_desc[, "Built"])
  if (source_built[["Platform"]] != "") {
    source_built[["Platform"]] <- platform
    source_desc[, "Built"] <- deparse_built(source_built)
    write.dcf(source_desc, source_path, keep.white = "Built")
  }

  binary_path <- file.path(path, "Meta", "package.rds")
  binary_desc <- readRDS(binary_path)
  if (binary_desc[["Built"]][["Platform"]] != "") {
    binary_desc[["Built"]][["Platform"]] <- platform
    saveRDS(binary_desc, binary_path)
  }
}

install_one <- function(pkg, lib = NULL) {
  lib <- get_lib(lib)

  if (file.exists(file.path(lib, pkg))) {
    warning("Package already installed, this should not happen.")
  }

  cat("\nCompiling", pkg, "\n")
  suppressWarnings(suppressMessages(utils::capture.output(install.packages(
    paste0("library/", pkg),
    lib = lib,
    repos = NULL,
    type = "source",
    INSTALL_opts = install_opts()
  ))))

  # Need to stop explicitly, because `install.packages()` does not error
  # on failed installation. Before calling `install_one()` we always
  # remove the previously installed package, so this check still works if
  # we are updating.

  if (!file.exists(file.path(lib, pkg))) {
    stop("FAILED")
  }

  invisible()
}

install_order <- function() {
  ## TODO: look up the correct order
  pkgs <- c(
    # no deps
    "R6", "cli", "curl", "filelock", "jsonlite", "lpSolve", "ps", "zip",
    # ps, R6
    "processx",
    # processx, R6
    "callr",
    # cli, R6
    "desc",
    # callr, cli, desc, processx, R6
    "pkgbuild",
    # callr, cli, curl, filelock, jsonlite, prettyunis, processx, R6
    "pkgcache",
    # curl, jsonlite
    "pkgsearch",
    # callr, cli, curl, desc, filelock, jsonlite, lpSolve, pkgbuild,
    # pkgcache, processx, ps, R6, zip
    "pkgdepends"
  )

  pkgs
}

# we do not need the R/ directory once the R code is bundled into a
# single RDS file. This is not called for `load_all()`, so that we
# can update single packages.

clean_up_r <- function(lib = NULL) {
  lib <- get_lib(lib)
  for (pkg in dir(lib)) {
    r_dir <- file.path(lib, pkg, "R")
    if (file.exists(r_dir)) {
      rimraf(r_dir)
    }
  }
}

# This is used for `R CMD INSTALL`, but not for `load_all()`.
# This is pretty simple, except when we are cross-compiling.
#
# Cross-compiling means that we must be able to load the dependncies
# on the building platform, and the packages for the target platform
# must be installed into a separate library. Actually, we put each
# one into its own library, to be on the safe side. After all
# dependencies are installed, we move them over to the proper place.

install_all <- function(lib = NULL) {
  pkgs <- install_order()
  if (Sys.getenv("CROSS_COMPILING") == "yes") {
    # Update 'Built' field in package itself
    update_description_build(
      Sys.getenv("R_PACKAGE_DIR"),
      Sys.getenv("R_TARGET_PLATFORM")
    )
    lib <- get_lib(lib)
    for (pkg in pkgs) {
      install_one(pkg, lib = paste0(lib, "-", pkg))
    }
    for (pkg in pkgs) {
      file.rename(file.path(paste0(lib, "-", pkg), pkg), file.path(lib, pkg))
      unlink(file.path(paste0(lib, "-", pkg)), recursive = TRUE)
      # Update 'Built' field in dependency
      update_description_build(
        file.path(lib, pkg),
        Sys.getenv("R_TARGET_PLATFORM")
      )
    }
  } else {
    for (pkg in pkgs) install_one(pkg, lib = lib)
  }
  invisible()
}

# This is used to decide if we need to update a dependency during
# `load_all()`. At this point it is only a performance optimization, and
# it could be removed, because if the vesions match, then we use
# hashing to make sure that they are the same.

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

# MD5 of a string

md5 <- function(str) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  cat(str, file = tmp)
  tools::md5sum(tmp)
}

# Hash of a directory. MD5 of each file, and then the MD5 of these.
# FIXME: hash the path as well, not just the contents, to account
# for renaming and moving files.

dir_hash <- function(path) {
  files <- sort(dir(path, full.names = TRUE, recursive = TRUE))
  files <- grep(
    "[.](s?o|gcda|gcov)$",
    files,
    value = TRUE,
    invert = TRUE
  )
  # Reinstall if turning hashing on/off
  hashes <- c(
    unname(tools::md5sum(files)),
    Sys.getenv("TEST_COVERAGE_PAK", "false")
  )
  md5(paste(hashes, collapse = " "))
}

# This is what we use in `load_all()`. It is not used during
# `R CMD INSTALL`, that case is covered by `install_all()`.
#
# It uses hashing to decide if a dependency needs to be updated in
# the private library.

update_all <- function(lib = NULL) {
  if (Sys.getenv("TEST_COVERAGE_PAK") == "true") {
    old <- Sys.getenv("R_MAKEVARS_USER", NA_character_)
    if (is.na(old)) {
      on.exit(Sys.unsetenv("R_MAKEVARS_USER"), add = TRUE)
    } else {
      on.exit(Sys.setenv("R_MAKEVARS_USER" = old), add = TRUE)
    }
    Sys.setenv(R_MAKEVARS_USER = normalizePath("Makevars-covr"))
    message("Test coverage build!")
  }
  lib <- get_lib(lib)
  hash_tmpl <- file.path(lib, "_%s.hash")
  pkgs <- install_order()
  upd <- structure(logical(length(pkgs)), names = pkgs)
  for (pkg in pkgs) {
    hash_path <- sprintf(hash_tmpl, pkg)
    new_hash <- dir_hash(file.path("library", pkg))
    if (!file.exists(hash_path)) {
      message("Adding ", pkg)
      rimraf(file.path(lib, pkg)) # in case it is a broken install
      install_one(pkg, lib)
      writeLines(new_hash, hash_path)
      upd[pkg] <- TRUE
    } else {
      old_hash <- readLines(hash_path)
      new_hash <- dir_hash(file.path("library", pkg))
      oldver <- get_ver(file.path(lib, pkg))
      newver <- get_ver(file.path("library", pkg))
      if (old_hash != new_hash) {
        message("Updating ", pkg, " ", oldver, " -> ", newver)
        rimraf(file.path(lib, pkg))
        install_one(pkg, lib)
        writeLines(new_hash, hash_path)
        upd[pkg] <- TRUE
      }
    }
  }
  upd
}

load_all <- function() {
  args <- commandArgs(TRUE)
  if (length(args) < 2) {
    stop("Usage: install-embedded.R [ --load-all library-dir ]")
  }
  upd <- update_all(args[2])
  if (any(upd)) {
    bundle_rds(args[2])
  }
  if (Sys.getenv("TEST_COVERAGE_PAK") == "true") {
    bundle_covr_rds(args[2])
  }
}

# Parse the `--build` and `--target` arguments passed to
# `./configure`. Also check if we are called from `load_all()`.

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

# Need to set the environments of functions within packages.

set_function_envs <- function(within, new) {
  old <- .libPaths()
  .libPaths(character())
  on.exit(.libPaths(old), add = TRUE)
  nms <- names(within)

  # We don't reset closures. `R6::R6Class()` must be handled specially,
  # because it is a closure, but its environment has a name.
  is_target_env <- function(x) {
    identical(x, base::.GlobalEnv) ||
      (!environmentName(x) %in% c("", "R6_capsule"))
  }

  suppressWarnings({
    for (nm in nms) {
      if (is.function(within[[nm]])) {
        if (is_target_env(environment(within[[nm]]))) {
          environment(within[[nm]]) <- new
        } else if (is_target_env(parent.env(environment(within[[nm]])))) {
          parent.env(environment(within[[nm]])) <- new
        }
      } else if ("R6ClassGenerator" %in% class(within[[nm]])) {
        within[[nm]]$parent_env <- new
        for (mth in names(within[[nm]]$public_methods)) {
          environment(within[[nm]]$public_methods[[mth]]) <- new
        }
        for (mth in names(within[[nm]]$private_methods)) {
          environment(within[[nm]]$private_methods[[mth]]) <- new
        }
      }
    }
  })

  invisible()
}

patch_env_refs <- function(pkg_env) {
  pkg_env[["::"]] <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    if (pkg %in% names(pkg_data$ns)) {
      pkg_data$ns[[pkg]][[name]]
    } else {
      # Fall back to a regular package, so we can call base packages
      getExportedValue(pkg, name)
    }
  }
  environment(pkg_env[["::"]]) <- pkg_env

  pkg_env[["asNamespace"]] <- function(ns, ...) {
    if (ns %in% names(pkg_data$ns)) {
      pkg_data$ns[[ns]]
    } else {
      base::asNamespace(ns, ...)
    }
  }
  environment(pkg_env[["asNamespace"]]) <- pkg_env

  pkg_env[["UseMethod"]] <- function(generic, object) {
    base::UseMethod(generic, object)
  }
  environment(pkg_env[["UseMethod"]]) <- pkg_env
}

# Put R code of all dependencies from their R/ directories, into a
# single RDS file.

bundle_rds <- function(lib = NULL) {
  message("Updating bundled dependencies")
  lib <- lib %||% get_lib(lib)
  ns <- new.env(parent = emptyenv())
  pkgs <- setdiff(
    dir(lib, pattern = "^[^_]"),
    c("deps.rds", "deps-covr.rds")
  )
  for (pkg in pkgs) {
    pkg_env <- new.env(parent = emptyenv())
    pkg_env[[".packageName"]] <- pkg
    ns[[pkg]] <- pkg_env
    lazyLoad(file.path(lib, pkg, "R", pkg), envir = pkg_env)
    sysdata <- file.path(lib, pkg, "R", "sysdata.rdb")
    if (file.exists(sysdata)) {
      lazyLoad(file.path(lib, pkg, "R", "sysdata"), envir = pkg_env)
    }
    set_function_envs(pkg_env, pkg_env)
    ## Sometimes a package refers to its env, this is one known instance.
    ## We could also walk the whole tree, but probably not worth it.
    if (!is.null(pkg_env$err$.internal$package_env)) {
      pkg_env$err$.internal$package_env <- pkg_env
    }

    patch_env_refs(pkg_env)

    invisible()
  }

  # pkgdepends has functions in a list, update those as well
  pds <- ns[["pkgdepends"]]
  pkgdepends_conf <- names(pds[["pkgdepends_config"]])
  for (nm in pkgdepends_conf) {
    if (is.function(pds[["pkgdepends_config"]][[nm]][["default"]])) {
      environment(pds[["pkgdepends_config"]][[nm]][["default"]]) <- pds
    }
  }
  parent.env(pds[["config"]][[".internal"]]) <- pds

  # make sure functions are byte-compiled
  compiler::compilePKGS(TRUE)
  saveRDS(ns, file.path(lib, "deps.rds"))
  compiler::compilePKGS(FALSE)
}

bundle_covr_rds <- function(lib = NULL) {
  lib <- lib %||% get_lib(lib)
  rds <- file.path(lib, "deps.rds")
  covrds <- file.path(lib, "deps-covr.rds")
  if (!file.exists(covrds) || file.mtime(covrds) < file.mtime(rds)) {
    message("Instrumenting dependency code for covr")
    ns <- readRDS(rds)
    ns <- covrlabs::serialize_to_file(
      ns,
      covrds,
      closxp_callback = covrlabs::trace_calls
    )
  } else {
    message("Instruments code bundle is current")
  }
}

# -------------------------------------------------------------------------
# Main function, called if we run as a script

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
    # TODO: we could do this here, now that we don't use rappdirs.
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
    Sys.setenv(R_TARGET_PLATFORM = pl$target)
  } else if (grepl("x86_64-apple-darwin", pl$current) &&
    grepl("aarch64-apple-darwin", pl$target)) {
    # Current is Darwin x86_64, target is Darwin aarch64
    Sys.setenv(CROSS_COMPILING = "yes")
    Sys.setenv(R_TARGET_PLATFORM = pl$target)
  } else if (grepl("x86_64.*linux", pl$current) &&
    grepl("aarch64.*linux", pl$target)) {
    # Current is Linux x86_64, target is Linux aarch64
    Sys.setenv(CROSS_COMPILING = "yes")
    Sys.setenv(R_TARGET_PLATFORM = pl$target)
  } else {
    # Not cross compiling or cross compiling handled externally
  }

  install_all()
  bundle_rds()
  clean_up_r()

  file.create("DONE")
}

if (is.null(sys.calls())) {
  install_embedded_main()
}
