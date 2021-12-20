
pak_sitrep_data <- list(deps = logical(), bundledata = list())

# It seems that it is not possible to share these scripts with the package,
# except for source()-ing the collated package code, which I decided not
# to do for now, as it depends on an implementation detail.

local({

  rot1 <- function(x) {
    as.integer(charToRaw(as.character(x)))
  }

  should_bundle <- function() {
    pkgdir <- Sys.getenv("R_PACKAGE_DIR", "")
    pak_sitrep_data$bundledata <<- list(
      rpkgname = rot1(Sys.getenv("R_PACKAGE_NAME", "")),
      pkgdir = rot1(Sys.getenv("R_PACKAGE_DIR", "")),
      meta = rot1(file.exists(file.path(pkgdir, "Meta"))),
      ghworkflow = rot1(Sys.getenv("GITHUB_WORKFLOW", "")),
      shost = rot1(Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_", ""))
    )

    # Do not bundle in pkgload::load_all()
    if (Sys.getenv("R_PACKAGE_NAME", "") != "pak") return(FALSE)

    # This must be set in R CMD INSTALL
    if (pkgdir == "") return(FALSE)

    # Another test for pkgload::load_all(), just in case
    if (!file.exists(file.path(pkgdir, "Meta"))) return(FALSE)

    # Do not bundle when building binary on GHA. GHA uses its own
    # bundling for now.
    if (Sys.getenv("GITHUB_WORKFLOW", "") == "Build pak binary") {
      return(FALSE)
    }

    if (Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_", "") != "") {
      return(FALSE)
    }

    TRUE
  }

  bundle <- function() {
    pkgdir <- Sys.getenv("R_PACKAGE_DIR", "")
    if (pkgdir != "") {
      desc <- file.path(pkgdir, "DESCRIPTION")
      dcf <- read.dcf(desc)
      deps <- dcf[, "Config/needs/dependencies"]
      rmts <- if ("Remotes" %in% colnames(dcf)) dcf[, "Remotes"]
    } else {
      dcf <- utils::packageDescription("pak")
      deps <- dcf$`Config/needs/dependencies`
      rmts <- dcf$Remotes
    }

    if (!is.null(rmts)) {
      cat(
        "** -----------------------------------------------------------------------------\n",
        "** Found 'Remotes', **NOT** bundling dependencies, ",
        "use `pak:::create_dev_lib()`.\n",
        "** -----------------------------------------------------------------------------\n",
        sep = ""
      )
      return()
    }

    cat("** building pak dependency data, this can take several minutes\n")

    # In case the CRAN repo is not set
    repos <- getOption("repos")
    if ("@CRAN@" %in% repos) {
      repos[repos == "@CRAN@"] <- c(CRAN = "https://cloud.r-project.org")
      o1 <- options(repos = repos)
      on.exit(options(o1), add = TRUE)
    } else if (! "CRAN" %in% names(repos)) {
      repos <- c(repos, c(CRAN = "https://cloud.r-project.org"))
      o1 <- options(repos = repos)
      on.exit(options(o1), add = TRUE)
    }

    pkgs <- bundle_parse_deps(deps)

    lib <- file.path(pkgdir, "library")
    dir.create(lib, showWarnings = FALSE, recursive = TRUE)

    Sys.setenv(R_COMPILE_AND_INSTALL_PACKAGES = "always")
    o2 <- options(install.packages.compile.from.source = "always")
    on.exit(options(o2), add = TRUE)

    Sys.setenv(R_PROFILE_USER = tempfile())

    cat("** deps data (", length(pkgs), "): ", sep = "")
    n <- 1
    for (pkg in pkgs) {
      tryCatch(
        suppressWarnings(suppressMessages(utils::capture.output(
          utils::install.packages(
            pkg, lib = lib, quiet = TRUE,
            INSTALL_opts = "--no-staged-install --no-test-load --without-keep.source --no-help --no-html --strip --no-data",
            dependencies = FALSE,
            type = .Platform$pkgType
          )
        ))),
        error = function(err) NULL
      )
      if (!file.exists(file.path(lib, pkg)) && getOption("pkgType") != "source") {
        suppressWarnings(suppressMessages(utils::capture.output(
          utils::install.packages(
            pkg, lib = lib, quiet = TRUE,
            INSTALL_opts = "--no-staged-install --no-test-load --without-keep.source --no-help --no-html --strip --no-data",
            dependencies = FALSE,
            type = "source"
            )
        )))
      }
      if (!file.exists(file.path(lib, pkg))) {
        stop("Failed to bundle deps data (", pkg, ")")
      }
      pak_sitrep_data$deps[[pkg]] <<- TRUE
      cat(".")
      if (!n %% 10) cat(n)
      n <- n + 1
    }
    cat(length(pkgs), "\n", sep = "")

    bundle_cleanup_library(lib)

    cat("** pak dependency data is embedded\n")
  }

  rimraf <- function(...) {
    x <- file.path(...)
    unlink(x, recursive = TRUE, force = TRUE)
  }

  bundle_cleanup_library <- function(lib) {
    pkgs <- dir(lib)
    rimraf(file.path(lib, pkgs, "help"))
    rimraf(file.path(lib, pkgs, "doc"))

    # Remove .so.dSYM junk files
    rimraf(dir(
      lib,
      full.names = TRUE,
      recursive = TRUE,
      pattern = "\\.so\\.dSYM$",
      include.dirs = TRUE
    ))

    # NEWS files
    rimraf(dir(
      lib,
      full.names = TRUE,
      recursive=TRUE,
      pattern = "(NEWS|NEWS.md)",
      include.dirs=TRUE
    ))

    # no need for tests
    rimraf(file.path(lib, "digest", "tinytest"))

    # logos
    rimraf(dir(
      lib,
      full.names = TRUE,
      recursive = TRUE,
      pattern = "logo.*[.](png|svg)"
    ))
  }

  bundle_re_match <- function(text, pattern, perl = TRUE, ...) {
    text <- as.character(text)
    match <- regexpr(pattern, text, perl = perl, ...)
    start <- as.vector(match)
    length <- attr(match, "match.length")
    end <- start + length - 1L
    matchstr <- substring(text, start, end)
    matchstr[start == -1] <- NA_character_

    res <- data.frame(
      stringsAsFactors = FALSE,
      .text = text,
      .match = matchstr
    )

    if (!is.null(attr(match, "capture.start"))) {
      gstart <- attr(match, "capture.start")
      glength <- attr(match, "capture.length")
      gend <- gstart + glength - 1L
      groupstr <- substring(text, gstart, gend)
      groupstr[gstart == -1] <- NA_character_
      dim(groupstr) <- dim(gstart)
      res <- cbind(groupstr, res, stringsAsFactors = FALSE)
    }

    names(res) <- c(attr(match, "capture.names"), ".text", ".match")

    res
  }

  bundle_parse_deps <- function (str) {
    strs <- unname(vapply(strsplit(str, ",")[[1]], trimws, character(1)))
    rx <- paste0(
      "(?<package>[^\\s]+)",
      "\\s*", "(?:[(](?<op>>|>=|==|<|<=)\\s*(?<version>[-0-9\\.]+)[)])?\\s*$"
    )
    mtc <- bundle_re_match(strs, rx)
    pkgs <- mtc$package

    db <- utils::available.packages()

    deps <- tools::package_dependencies(pkgs, db = db)
    repeat {
      nxt <- unlist(unique(deps))
      mss <- setdiff(nxt, names(deps))
      if (length(mss) == 0) break
      deps <- c(deps, tools::package_dependencies(mss, db = db))
    }

    base <- bundle_base_packages()
    deps <- deps[setdiff(names(deps), base)]
    deps[] <- lapply(deps, setdiff, base)

    # Now create a topological order
    pkgs <- character()
    while (length(deps) > 0) {
      nxt <- vapply(deps, length, integer(1)) == 0
      pnxt <- names(deps)[nxt]
      pkgs <- c(pkgs, pnxt)
      deps <- deps[! nxt]
      deps[] <- lapply(deps, setdiff, pnxt)
    }

    pkgs
  }

  bundle_base_packages <- function() {
    rownames(utils::installed.packages(.Library, priority="base"))
  }

  pak_sitrep_data$platform <<- R.Version()$platform
  pak_sitrep_data$`github-repository` <<- Sys.getenv("GITHUB_REPOSITORY", "-")
  pak_sitrep_data$`github-sha` <<- Sys.getenv("GITHUB_SHA", "-")
  pak_sitrep_data$`github-ref` <<- Sys.getenv("GITHUB_REF", "-")

  tryCatch(
    if (Sys.getenv("PAK_DATA_BUNDLER") != "true" && should_bundle()) {
      pak_sitrep_data$bundled <<- FALSE
      bundle()
      pak_sitrep_data$bundled <<- TRUE
    },
    error = function(err) {
      cat("Failed\n")
    }
  )

  Sys.setenv("PAK_DATA_BUNDLER" = "true")
})

pak_sitrep_data
