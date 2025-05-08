# nocov start

fake_env <- new.env(parent = emptyenv())

make_dummy_package <- function(data, path) {
  package <- data$Package
  data$Version <- data$Version %||% "1.0.0"
  mkdirp(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::local_dir(tmp)
  mkdirp(package)
  file.create(file.path(package, "NAMESPACE"))
  write.dcf(data, file.path(package, "DESCRIPTION"))
  suppressMessages(utils::capture.output(
    asNamespace("tools")$.build_packages(args = package, no.q = TRUE)
  ))
  unlink(package, recursive = TRUE)
  out <- dir()
  if (length(out) != 1) stop("Failed to build package ", package, " :(")
  mkdirp(path)
  file.copy(out, path, overwrite = TRUE)
  out
}

dummy_so <- function() {
  if (!is.null(fake_env$dummy_so)) return(fake_env$dummy_so)

  mkdirp(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::local_dir(tmp)

  writeLines(
    c(
      "#include <Rinternals.h>",
      "",
      "SEXP minus1(SEXP i) {",
      "  return Rf_ScalarInteger(REAL(i)[0] - 1.0);",
      "}"
    ),
    "init.c"
  )
  callr::rcmd("SHLIB", c("-o", "foo.so", "init.c"))
  so <- readBin("foo.so", "raw", file.size("foo.so"))

  fake_env$dummy_so <- so
  so
}

make_dummy_binary <- function(
  data,
  path,
  platform = get_platform(),
  r_version = getRversion()
) {
  # Need these files:
  # NAMESPACE  -- nded to add useDynLib() and import() as needed
  # DESCRIPTION -- need `Built` field
  # Meta/links.rds -- can be the same if no manual
  # Meta/features.rds -- .install_package_description() creates this
  # Meta/nsInfo.rds -- .install_package_namespace_info() creates this
  # Meta/package.rds -- .install_package_description() creates this
  # Meta/hsearch.rds -- can be the same if no manual
  # libs/<pkg>.so -- use the same dummy .so

  path <- normalizePath(path, mustWork = FALSE)
  mkdirp(path)

  package <- data$Package
  data$Version <- data$Version %||% "1.0.0"

  mkdirp(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::local_dir(tmp)
  mkdirp(package)
  file.create(file.path(package, "NAMESPACE"))
  write.dcf(data, file.path(package, "DESCRIPTION"))

  if (paste0("", data$NeedsCompilation) %in% c("yes", "true")) {
    # TODO: multi-arch on Windows
    writeLines(
      paste0("useDynLib(", package, ")"),
      file.path(package, "NAMESPACE")
    )
    sofile <- paste0(package, .Platform$dynlib.ext)
    sopath <- if (nzchar(.Platform$r_arch)) {
      file.path(package, "libs", .Platform$r_arch, sofile)
    } else {
      file.path(package, "libs", sofile)
    }
    mkdirp(dirname(sopath))
    writeBin(dummy_so(), sopath)
  }

  asNamespace("tools")$.install_package_description(package, package)
  asNamespace("tools")$.install_package_namespace_info(package, package)

  if (platform == "windows") {
    pkgfile <- paste0(package, "_", data$Version, ".zip")
    zip::zip(pkgfile, package)
  } else if (platform == "macos") {
    pkgfile <- paste0(package, "_", data$Version, ".tgz")
    utils::tar(pkgfile, package)
  } else {
    # Other binary package, we use .tar.gz like on PPM
    pkgfile <- paste0(package, "_", data$Version, ".tar.gz")
    utils::tar(pkgfile, package)
  }

  file.copy(pkgfile, path, overwrite = TRUE)
  pkgfile
}

standardize_dummy_packages <- function(packages) {
  packages <- packages %||%
    data.frame(
      stringsAsFactors = FALSE,
      Package = character()
    )

  if (!"Package" %in% names(packages)) {
    packages$Package <- paste0("pkg", seq_len(nrow(packages)))
  }

  if (!"Version" %in% names(packages)) {
    packages$Version <- if (nrow(packages)) "1.0.0" else character()
  } else {
    packages$Version[is.na(packages$Version)] <- "1.0.0"
  }

  packages
}

make_dummy_repo <- function(repo, packages = NULL, options = list()) {
  mkdirp(repo)

  packages <- standardize_dummy_packages(packages)

  options[["platforms"]] <- options[["platforms"]] %||% "source"

  for (plt in options[["platforms"]]) {
    options2 <- options
    options2[["platform"]] <- plt
    options2[["no_archive"]] <- options[["no_archive"]] %||% plt != "source"
    make_dummy_repo_platform(repo, packages, options2)
  }

  invisible()
}

make_dummy_repo_platform <- function(repo, packages = NULL, options = list()) {
  mkdirp(repo)

  options[["platform"]] <- options[["platform"]] %||% "source"
  options[["rversion"]] <- options[["rversion"]] %||% format(getRversion())
  packages <- standardize_dummy_packages(packages)

  if (!is.null(options$repo_prefix)) {
    repo <- file.path(repo, options$repo_prefix)
  }
  pkgdirs <- get_all_package_dirs(options[["platform"]], getRversion())
  mkdirp(pkgs_dir <- file.path(repo, pkgdirs$contriburl))

  extra <- packages
  extra$file <- character(nrow(extra))

  latest <- tapply(
    extra$Version,
    extra$Package,
    function(x) as.character(max(package_version(x))),
    simplify = TRUE
  )
  extra$archive <- latest[extra$Package] != extra$Version

  for (i in seq_len(nrow(packages))) {
    if (
      options[["platform"]] == "source" &&
        packages$Package[i] %in% options[["no_sources"]]
    )
      next
    if (
      options[["platform"]] != "source" &&
        packages$Package[i] %in% options[["no_binaries"]]
    )
      next
    if (extra$archive[i]) {
      if (isTRUE(options$no_archive)) next
      pkg_dir <- file.path(pkgs_dir, "Archive", packages$Package[i])
    } else {
      pkg_dir <- pkgs_dir
    }

    if (options[["platform"]] == "source") {
      fn <- make_dummy_package(packages[i, , drop = FALSE], pkg_dir)
    } else {
      fn <- make_dummy_binary(
        packages[i, , drop = FALSE],
        pkg_dir,
        options[["platform"]]
      )
    }
    extra$file[i] <- fn
  }

  file.create(file.path(pkgs_dir, "PACKAGES"))

  if (grepl("windows", pkgdirs$contriburl)) {
    pkg_type <- "win.binary"
  } else {
    pkg_type <- "source"
  }
  tools::write_PACKAGES(pkgs_dir, type = pkg_type)

  if (isTRUE(options$no_packages)) {
    file.remove(file.path(pkgs_dir, "PACKAGES"))
  }

  if (isTRUE(options$no_packages_gz)) {
    file.remove(file.path(pkgs_dir, "PACKAGES.gz"))
  } else if (file.exists(file.path(pkgs_dir, "PACKAGES.gz"))) {
    # if empty
  }

  if (isTRUE(options$no_packages_rds)) {
    file.remove(file.path(pkgs_dir, "PACKAGES.rds"))
  }

  if (!isTRUE(options$no_metadata)) {
    current <- extra[!extra$archive, , drop = FALSE]
    meta <- data.frame(
      stringsAsFactors = FALSE,
      file = current$file,
      size = file.size(file.path(pkgs_dir, current$file)),
      sha = cli::hash_file_sha256(file.path(pkgs_dir, current$file)),
      sysreqs = current$SystemRequirements %||% rep("NA", nrow(current)),
      built = if (nrow(current)) "NA" else character(),
      published = if (nrow(current)) format(Sys.time()) else character()
    )
    outcon <- gzcon(file(file.path(pkgs_dir, "METADATA2.gz"), "wb"))
    utils::write.csv(meta, outcon, row.names = FALSE)
    close(outcon)
  }

  if (!isTRUE(options$no_archive)) {
    archive <- extra[extra$archive, , drop = FALSE]
    adf <- list()
    adir <- file.path(pkgs_dir, "Archive")
    if (file.exists(adir)) {
      adirs <- dir(adir)
      adf <- lapply(adirs, function(d) {
        pkgs <- dir(file.path(adir, d), full.names = TRUE)
        fi <- file.info(pkgs)
        rownames(fi) <- basename(rownames(fi))
        fi
      })
      names(adf) <- adirs
    }

    mkdirp(file.path(pkgs_dir, "Meta"))
    saveRDS(adf, file.path(pkgs_dir, "Meta", "archive.rds"))
  }

  invisible()
}

cran_app <- function(
  packages = NULL,
  log = interactive(),
  basic_auth = NULL,
  options = list()
) {
  app <- webfakes::new_app()

  # Log requests by default
  if (log) app$use("logger" = webfakes::mw_log())

  if (!is.null(basic_auth)) {
    app$use("basic auth" = function(req, res) {
      exp <- paste(
        "Basic",
        base64_encode(
          paste0(basic_auth[["username"]], ":", basic_auth[["password"]])
        )
      )
      hdr <- req$get_header("Authorization") %||% ""
      if (exp != hdr) {
        res$set_header(
          "WWW-Authenticate",
          "Basic realm=\"CRAN with auth\""
        )$send_status(401L)
      } else {
        "next"
      }
    })
  }

  # Parse all kinds of bodies
  app$use("json body parser" = webfakes::mw_json())
  app$use(
    "text body parser" = webfakes::mw_text(
      type = c("text/plain", "application/json")
    )
  )
  app$use("multipart body parser" = webfakes::mw_multipart())
  app$use("URL encoded body parser" = webfakes::mw_urlencoded())

  # Add etags by default
  app$use("add etag" = webfakes::mw_etag())

  # Add date by default
  app$use("add date" = function(req, res) {
    res$set_header("Date", as.character(Sys.time()))
    "next"
  })

  app$locals$created <- FALSE
  app$locals$repo <- tempfile()
  app$locals$packages <- packages
  app$locals$options <- options

  app$use("create" = function(req, res) {
    locals <- req$app$locals
    if (!locals$created) {
      make_dummy_repo(locals$repo, locals$packages, locals$options)
      req$app$locals$created <- TRUE
    }

    reg.finalizer(
      req$app,
      function(obj) unlink(obj$locals$repo, recursive = TRUE),
      TRUE
    )

    "next"
  })
  app$use("repo" = webfakes::mw_static(app$locals$repo))

  app
}

dcf <- function(txt) {
  txt <- gsub("\n[ ]+", "\n", txt)
  as.data.frame(read.dcf(textConnection(txt)), stringsAsFactors = FALSE)
}

cran_app_pkgs <- dcf(
  "
  Package: pkg1
  Version: 1.0.0

  Package: pkg1
  Version: 0.9.0

  Package: pkg1
  Version: 0.8.0

  Package: pkg2
  Version: 1.0.0
  Depends: pkg1

  Package: pkg3
  Version: 1.0.0
  Depends: pkg2

  Package: pkg3
  Version: 0.9.9
"
)

fix_port <- function(x) {
  gsub("http://127[.]0[.]0[.]1:[0-9]+", "http://127.0.0.1:<port>", x)
}

bioc_app <- function(packages = NULL, log = interactive(), options = list()) {
  app <- webfakes::new_app()

  # Log requests by default
  if (log) app$use("logger" = webfakes::mw_log())

  # Parse all kinds of bodies
  app$use("json body parser" = webfakes::mw_json())
  app$use(
    "text body parser" = webfakes::mw_text(
      type = c("text/plain", "application/json")
    )
  )
  app$use("multipart body parser" = webfakes::mw_multipart())
  app$use("URL encoded body parser" = webfakes::mw_urlencoded())

  # Add etags by default
  app$use("add etag" = webfakes::mw_etag())

  # Add date by default
  app$use("add date" = function(req, res) {
    res$set_header("Date", as.character(Sys.time()))
    "next"
  })

  app$locals$created <- FALSE
  app$locals$repo <- tempfile()
  app$locals$packages <- packages
  app$locals$options <- options

  app$use("create" = function(req, res) {
    locals <- req$app$locals
    if (!locals$created) {
      make_bioc_repo(locals$repo, locals$packages, locals$options)
      req$app$locals$created <- TRUE
    }

    reg.finalizer(
      req$app,
      function(obj) unlink(obj$locals$repo, recursive = TRUE),
      TRUE
    )

    "next"
  })
  app$use("repo" = webfakes::mw_static(app$locals$repo))

  app
}

make_bioc_repo <- function(repo, packages, options) {
  packages <- standardize_dummy_packages(packages)

  bioc_version <- options$bioc_version %||% bioconductor$get_bioc_version()
  options$no_metadata <- options$no_metadata %||% TRUE
  options$no_archive <- options$no_archive %||% TRUE

  packages$bioc_repo <- packages$bioc_repo %||%
    if (nrow(packages)) "soft" else character()
  packages$bioc_repo[is.na(packages$bioc_repo)] <- "soft"
  bioc_repo <- packages$bioc_repo
  packages <- packages[, setdiff(names(packages), "bioc_repo"), drop = FALSE]

  # BioCsoft
  options$repo_prefix <- sprintf("packages/%s/bioc", bioc_version)
  pkg_soft <- packages[bioc_repo == "soft", , drop = FALSE]
  make_dummy_repo(repo, pkg_soft, options)

  # BioCann
  options$repo_prefix <- sprintf("packages/%s/data/annotation", bioc_version)
  pkg_ann <- packages[bioc_repo == "ann", , drop = FALSE]
  make_dummy_repo(repo, pkg_ann, options)

  # BioCexp
  options$repo_prefix <- sprintf("packages/%s/data/experiment", bioc_version)
  pkg_exp <- packages[bioc_repo == "exp", , drop = FALSE]
  make_dummy_repo(repo, pkg_ann, options)

  # BioCworkflows
  options$repo_prefix <- sprintf("packages/%s/workflows", bioc_version)
  pkg_workflows <- packages[bioc_repo == "workflows", , drop = FALSE]
  make_dummy_repo(repo, pkg_workflows, options)

  # BioCbooks
  options$repo_prefix <- sprintf("packages/%s/books", bioc_version)
  pkg_books <- packages[bioc_repo == "books", , drop = FALSE]
  make_dummy_repo(repo, pkg_books, options)

  config <- system.file("fixtures", "bioc-config.yaml", package = "pkgcache")
  if (config == "") {
    warning("Cannot find 'bioc-config.yaml' in pkgcache")
  } else {
    file.copy(config, file.path(repo, "config.yaml"))
  }

  invisible()
}

auth_proxy_app <- function(
  repo_url = NULL,
  username = "username",
  password = "token"
) {
  repo_url <- repo_url %||% "https://cloud.r-project.org"
  webfakes::new_app()$get(
    webfakes::new_regexp(""),
    function(req, res) {
      exp <- paste("Basic", base64_encode(paste0(username, ":", password)))
      hdr <- req$get_header("Authorization") %||% ""
      if (exp != hdr) {
        res$set_header(
          "WWW-Authenticate",
          "Basic realm=\"CRAN with auth\""
        )$send_status(401L)
      } else {
        res$redirect(sprintf("%s/%s", repo_url, req$path))
      }
    }
  )
}

# nocov end
