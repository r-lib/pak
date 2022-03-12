
pak_sitrep_data <- list(
  deps = logical(),
  failed = FALSE,
  fails = list(),
  method = NULL,
  bundledata = NULL
)

# It seems that it is not possible to share these scripts with the package,
# except for source()-ing the collated package code, which I decided not
# to do for now, as it depends on an implementation detail.

local({

  safe_cran_install <- local({

    default_cran_mirror <- function() {
      mirror <- getOption("repos")["CRAN"]
      if (is.null(mirror) || is.na(mirror) || mirror == "@CRAN@") {
        c(CRAN = "https://cran.rstudio.com")
      } else {
        c(CRAN = unname(mirror))
      }
    }

    cran_install_order <- local({

      base_packages <- function() {
        c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
          "methods", "parallel", "splines", "stats", "stats4", "tcltk",
          "tools", "utils"
          )
      }

      parse_dep_fields <- function(flds) {
        flds[is.na(flds)] <- ""
        flds <- gsub("\\s+", "", flds)
        flds <- gsub("\\([^)]+\\)", "", flds)
        notempty <- nzchar(flds)
        res <- replicate(length(flds), character())
        flds <- flds[notempty]
        flds <- strsplit(flds, ",", fixed = TRUE)

        base <- base_packages()
        flds <- lapply(flds, setdiff, y = c("R", base))

        res[notempty] <- flds
        res
      }

      extract_deps <- function(pkg, db) {
        dep_types <- c("Depends", "Imports", "LinkingTo")
        fields <- db[ db[, "Package"] %in% pkg, dep_types]
        unlist(parse_dep_fields(fields))
      }


      calculate_deps <- function(pkgs) {
        db <- available.packages(repos = default_cran_mirror())
        current <- character()
        deps <- list()
        new <- pkgs
        while (length(new) > 0) {
          deps[new] <- lapply(new, extract_deps, db = db)
          new <- setdiff(unlist(deps[new]), names(deps))
        }
        deps
      }

      topo_deps <- function(deps) {
        pkgs <- character()
        while (length(deps) > 0) {
          ndeps <- vapply(deps, length, integer(1))
          nxt <- names(deps)[ndeps == 0]
          pkgs <- c(pkgs, nxt)
          deps <- deps[! names(deps) %in% nxt]
          deps[] <- lapply(deps, setdiff, nxt)
        }
        pkgs
      }

      function(pkgs) {
        deps <- calculate_deps(pkgs)
        topo_deps(deps)
      }

    })

    install_one <- function(pkg, lib = .libPaths()[1], INSTALL_opts = "", ...) {
      old <- options(
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
        }, error = function(e) {
          err <<- e
          print(e)
        })
      }

      if (!done) {
        unlink(file.path(lib, pkg), recursive = TRUE)
        pak_sitrep_data$fails[[pkg]] <<- conditionMessage(err)
        pak_sitrep_data$failed <<- TRUE
        stop("Failure in ", pkg)
      }
    }

    function(pkgs, lib = .libPaths()[1], exclude = character(), ...) {
      dir.create(lib, showWarnings = FALSE, recursive = TRUE)
      alldeps <- cran_install_order(pkgs)
      alldeps0 <- setdiff(alldeps, exclude)
      cat("(", length(alldeps0), "): ", sep = "")

      opts <- paste(
        "--without-keep.source",
        "--no-html",
        "--no-help",
        "--no-data",
        "--strip",
        "--no-test-load",
        if (getRversion() >= "3.6") "--no-staged-install"
      )

      n <- 1
      for (pkg in alldeps0) {
        cat(".")
        if (!n %% 10) cat(n)
        n <- n + 1
        suppressWarnings(suppressMessages(utils::capture.output(install_one(
          pkg,
          lib = lib,
          INSTALL_opts = opts,
          ...
        ))))
      }
      cat(length(alldeps0), "\n", sep = "")
    }
  })

  rot1 <- function(x) {
    as.integer(charToRaw(as.character(x))) + 1L
  }

  # --------------------------------------------------
  # OS-type    check   load_all()   Remotes   mode
  # --------------------------------------------------
  # Windows    no      no           no        download
  # Windows    no      no           yes       none
  # Windows    no      yes          no        copy
  # Windows    no      yes          yes       copy
  # Windows    yes     no           no        download
  # Windows    yes     no           yes       none
  # Windows    yes     yes          no        copy
  # Windows    yes     yes          yes       copy
  # Unix       no      no           no        download
  # Unix       no      no           yes       none
  # Unix       no      yes          no        copy
  # Unix       no      yes          yes       copy
  # Unix       yes     no           no        none
  # Unix       yes     no           yes       none
  # Unix       yes     yes          no        copy
  # Unix       yes     yes          yes       copy
  # --------------------------------------------------

  bundle_method <- function() {
    pkgdir <- Sys.getenv("R_PACKAGE_DIR", "")

    # data
    ostype <- .Platform$OS.type
    load_all <- pkgdir == "" && !is.null(asNamespace("pak")$.__DEVTOOLS__)
    check <- Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_", "") != ""
    if (pkgdir != "") {
      desc <- file.path(pkgdir, "DESCRIPTION")
      dcf <- read.dcf(desc)
      remotes <- if ("Remotes" %in% colnames(dcf)) dcf[, "Remotes"]
    } else {
      dcf <- utils::packageDescription("pak")
      remotes <- dcf$Remotes
    }

    pak_sitrep_data$bundledata <<- list(
      pkgdir = rot1(Sys.getenv("R_PACKAGE_DIR", "")),
      rpkgname = rot1(Sys.getenv("R_PACKAGE_NAME", "")),
      devtools = !is.null(asNamespace("pak")$.__DEVTOOLS__),
      ghworkflow = rot1(Sys.getenv("GITHUB_WORKFLOW", "")),
      shost = rot1(Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_", "")),
      remotes = if (!is.null(remotes)) rot1(remotes)
    )

    # The table above boils down to this
    env <- tolower(Sys.getenv("PAK_BUNDLE"))
    result <- if (pkgdir == "" && !load_all) {
      "source"
    } else if (env %in% c("no", "false", "none")) {
      "none"
    } else if (env %in% c("none", "copy", "download", "double")) {
      env
    } else if (load_all) {
      "copy"
    } else if (!is.null(remotes)) {
      "none"
    } else if (ostype == "windows") {
      "download"
    } else if (check) {
      "none"
    } else {
      "download"
    }

    # Record this
    pak_sitrep_data$method <<- result

    # Avoid calling this twice in one install. I am not sure why it happens
    # at all, but it does happen. We need to avoid it differently in
    # load_all()
    if (load_all) {
      if (!is.null(asNamespace("pak")$.bundled)) return("double")
      assign(".bundled", "copy", asNamespace("pak"))
    } else {
      pid <- Sys.getpid()
      mark <- file.path(tempdir(), paste0(pid, ".log"))
      if (file.exists(mark)) return("double")
      file.create(mark)
    }

    result
  }

  bundle_deps <- function() {
    pkgdir <- Sys.getenv("R_PACKAGE_DIR", "")
    if (pkgdir != "") {
      desc <- file.path(pkgdir, "DESCRIPTION")
      dcf <- read.dcf(desc)
      deps <- dcf[, "Config/needs/dependencies"]
    } else {
      dcf <- utils::packageDescription("pak")
      deps <- dcf$`Config/needs/dependencies`
    }
    list(deps = deps, pkgdir = pkgdir)
  }

  bundle_copy <- function() {
    if (file.exists("../R/pak")) {
      sys.source("../R/pak", envir = environment())
    } else {
      for (src in dir("../R", full.names = TRUE, pattern = "[.][rR]$")) {
        sys.source(src, envir = environment())
      }
    }
    .packageName <- "pak"
    create_dev_lib()
  }

  bundle_download <- function() {
    deps <- bundle_deps()
    cat("**** building pak dependency data, this can take several minutes\n")
    lib <- Sys.getenv("PAK_LIBRARY_DIR", file.path(deps$pkgdir, "library"))
    pkgs <- trimws(strsplit(deps$deps, ",")[[1]])
    cat("**** deps data ")
    safe_cran_install(pkgs, lib = lib, quiet = TRUE)
  }

  pak_sitrep_data$platform <<- R.Version()$platform
  pak_sitrep_data$`github-repository` <<- Sys.getenv("GITHUB_REPOSITORY", "-")
  pak_sitrep_data$`github-sha` <<- Sys.getenv("GITHUB_SHA", "-")
  pak_sitrep_data$`github-ref` <<- Sys.getenv("GITHUB_REF", "-")

  mthd <- bundle_method()
  if (mthd == "none") {
    cat("**** not bundling dependency data (call pak:::create_dev_lib())\n")
  } else if (mthd == "copy") {
    bundle_copy()
  } else if (mthd == "download") {
    tryCatch(
      bundle_download(),
      error = function(err) { cat(" failed!\n") }
    )
  } else if (mthd == "double") {
    # nothing
  }

  # This is needed so we can eval(parse()) this for testing
  environment()
})
