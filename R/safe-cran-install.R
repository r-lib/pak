
# default_cran_mirror

safe_cran_install <- local({

  install_one <- function(pkg, lib = .libPaths()[1], INSTALL_opts = "", ...) {

    rcmd <- sprintf("loadNamespace('%s', lib.loc = '%s')", pkg, lib)
    cmd <- sprintf("R -q -e \"%s\"", rcmd)
    if (system(cmd) == 0) return()

    old <- options(
      warn = 2,
      repos = default_cran_mirror(),
      install.packages.compile.from.source = "always"
    )
    on.exit(options(old), add = TRUE)

    done <- FALSE
    # try binary first, this might be an older version, but never mind
    if (getOption("pkgType") != "source") {
      tryCatch({
        install.packages(
          pkg,
          lib = lib,
          dependencies = FALSE,
          INSTALL_opts = INSTALL_opts,
          ...
        )
        done <- TRUE
      }, error = function(err) print(err))
    }

    # try the canonical macos mirror as well, on macOS
    if (!done &&
        Sys.info()[["sysname"]] == "Darwin" &&
        getOption("pkgType") != "source") {
      options(repos = c(CRAN = "https://mac.r-project.org"))
      tryCatch({
        install.packages(
          pkg,
          lib = lib,
          dependencies = FALSE,
          INSTALL_opts = INSTALL_opts,
          ...
        )
        done <- TRUE
      }, error = function(err) print(err))
      options(repos = default_cran_mirror())
    }

    # source
    if (!done) {
      tryCatch({
        install.packages(
          pkg,
          lib = lib,
          dependencies = FALSE,
          type = "source",
          INSTALL_opts = INSTALL_opts,
          ...
        )
        done <- TRUE
      }, error = function(err) print(err))
    }

    if (!done) {
      unlink(file.path(lib, pkg), recursive = TRUE)
      stop("Failed to install ", pkg)
    }
  }

  function(pkgs, lib = .libPaths()[1], exclude = character()) {
    dir.create(lib, showWarnings = FALSE, recursive = TRUE)
    alldeps <- cran_install_order(pkgs)
    alldeps0 <- setdiff(alldeps, exclude)
    for (pkg in alldeps0) {
      install_one(
        pkg,
        lib = lib,
        INSTALL_opts = "--without-keep.source --no-html --no-help --no-data"
      )
    }
  }
})
