embed <- local({
  `%||%` <- function(l, r) {
    if (is.null(l)) r else l
  }

  mkdirp <- function(x) {
    dir.create(x, recursive = TRUE, showWarnings = FALSE)
  }

  rimraf <- function(x) {
    unlink(x, recursive = TRUE)
  }

  get_repos <- function() {
    c(CRAN = "https://cran.r-project.org")
  }

  base_packages <- function() {
    c(
      "base",
      "compiler",
      "datasets",
      "graphics",
      "grDevices",
      "grid",
      "methods",
      "parallel",
      "splines",
      "stats",
      "stats4",
      "tcltk",
      "tools",
      "utils"
    )
  }

  is_string <- function(x) {
    is.character(x) && length(x) == 1 && !is.na(x)
  }

  check_pak_root <- function() {
    bail <- function() {
      stop("Working directory must be the root of the pak package")
    }
    if (!file.exists("DESCRIPTION")) bail()
    dsc <- read.dcf("DESCRIPTION")
    pkg <- dsc[, "Package"]
    if (!is_string(pkg)) bail()
    if (pkg != "pak") bail()
  }

  lib_dir <- function() {
    check_pak_root()
    ld <- file.path("src", "library")
    mkdirp(ld)
    ld
  }

  get_current <- function() {
    check_pak_root()
    lib <- lib_dir()
    pkgs <- dir(lib)
    pkgs <- pkgs[!endsWith(pkgs, ".patch")]
    vers <- lapply(pkgs, function(pkg) {
      dsc <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))
      dpkg <- dsc[, "Package"]
      dver <- dsc[, "Version"]
      dsha <- if ("RemoteSha" %in% colnames(dsc)) {
        dsc[, "RemoteSha"]
      } else {
        NA_character_
      }
      if (!is_string(dpkg) || dpkg != pkg || !is_string(dver)) {
        stop("Invalid ", pkg, "DESCRIPTION")
      }
      list(version = dver, sha = dsha)
    })
    tab <- data.frame(
      stringsAsFactors = FALSE,
      package = pkgs,
      version = as.character(sapply(vers, "[[", "version")),
      sha = as.character(sapply(vers, "[[", "sha"))
    )
    tab[order(tab$package), ]
  }

  lookup_dep <- function(pkg) {
    if (grepl("/", pkg)) {
      lookup_dep_gh(pkg)
    } else {
      lookup_dep_cran(pkg)
    }
  }

  get_gh_desc <- function(slug) {
    url <- sprintf(
      "https://raw.githubusercontent.com/%s/main/DESCRIPTION",
      slug
    )
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)
    utils::download.file(url, tmp, quiet = TRUE)
    # NOTE: we do not handle recursive Remotes and such...
    dsc <- read.dcf(tmp)
    dsc
  }

  lookup_dep_gh <- function(slug) {
    dsc <- get_gh_desc(slug)
    imports <- dsc[, "Imports"]
    depsver <- trimws(strsplit(imports, ",")[[1]])
    deps <- sub("[ (].*$", "", depsver)
    unique(c(deps, unlist(lapply(deps, lookup_dep))))
  }

  lookup_dep_cran <- function(pkg) {
    tools::package_dependencies(pkg, recursive = TRUE)[[1]]
  }

  get_required <- function() {
    check_pak_root()
    lib <- lib_dir()
    dcf <- read.dcf("DESCRIPTION")
    pakdepsstr <- dcf[, "Config/needs/dependencies"]
    pakdeps <- trimws(strsplit(pakdepsstr, ",")[[1]])
    deps <- lapply(pakdeps, lookup_dep)
    allpkgs <- unique(c(pakdeps, unlist(deps)))
    sort(setdiff(allpkgs, base_packages()))
  }

  get_gh_sha <- function(slug) {
    cmd <- sprintf(
      "git ls-remote https://github.com/%s refs/heads/main",
      slug
    )
    out <- system(cmd, intern = TRUE)
    sub("\\t.*$", "", out)
  }

  lookup_gh_version <- function(slug) {
    dsc <- get_gh_desc(slug)
    sha <- get_gh_sha(slug)
    list(version = dsc[, "Version"], sha = sha)
  }

  check_update <- function(pkg = NULL) {
    check_pak_root()
    rpkg <- pkg %||% get_required()

    av <- utils::available.packages(repos = get_repos(), type = "source")
    vers <- sha <- rep(NA_character_, length(rpkg))
    wh <- match(rpkg, av[, "Package"])
    vers <- av[wh, "Version"]
    gh <- grepl("/", rpkg)
    ghver <- lapply(rpkg[gh], lookup_gh_version)
    vers[gh] <- sapply(ghver, "[[", "version")
    sha[gh] <- sapply(ghver, "[[", "sha")
    if (anyNA(vers)) {
      stop(
        "Required packages not available: ",
        paste(rpkg[is.na(vers)], collapse = ", ")
      )
    }

    crnt <- get_current()
    pkg_name <- sub("^.*/", "", rpkg)
    wh <- match(pkg_name, crnt$package)

    tab <- data.frame(
      stringsAsFactors = FALSE,
      ref = rpkg,
      package = pkg_name,
      required = unname(unlist(vers)),
      required_sha = unlist(sha),
      current = crnt$version[wh],
      current_sha = crnt$sha[wh]
    )
    rownames(tab) <- NULL

    if (is.null(pkg)) {
      xtr <- setdiff(crnt$package, pkg_name)
      if (length(xtr) > 0) {
        wh <- match(xtr, crnt$package)
        tab <- rbind(
          tab,
          data.frame(
            stringsAsFactors = FALSE,
            ref = NA_character_,
            package = xtr,
            required = NA_character_,
            required_sha = NA_character_,
            current = crnt$version[wh],
            current_sha = NA_character_
          )
        )
      }
    }

    tab$status <- ifelse(
      !is.na(tab$required) & !is.na(tab$current) & tab$required == tab$current,
      "OK",
      "UPDATE"
    )

    tab
  }

  delete_package <- function(pkg) {
    rimraf(file.path(lib_dir(), pkg))
  }

  addupdate_package <- function(
    pkg,
    ver = NULL,
    mode = c("add", "update")
  ) {
    mode <- match.arg(mode)
    stopifnot(
      is_string(pkg),
      is.null(ver) || is_string(ver)
    )

    lib <- lib_dir()
    pkg_name <- sub("^.*/", "", pkg)
    if (mode == "add") {
      if (file.exists(file.path(lib, pkg_name))) {
        stop("Package already in library: ", pkg_name)
      }
    }

    dir.create(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    if (grepl("/", pkg)) {
      url <- sprintf(
        "https://github.com/%s/archive/refs/heads/main.tar.gz",
        pkg
      )
      path1 <- file.path(tmp, paste0(pkg_name, ".tar.gz"))
      download.file(url, path1)

      path2 <- file.path(tmp, "pkgraw")
      untar(path1, exdir = path2)
      wd <- getwd()
      on.exit(setwd(wd), add = TRUE)
      setwd(path2)
      system2("R", c("CMD", "build ", dir()))
      path <- normalizePath(dir(pattern = "[.]tar[.]gz$"))
      setwd(wd)
    } else {
      path <- utils::download.packages(pkg, tmp, repos = get_repos())[, 2]
    }

    tmp1 <- file.path(tmp, "pkg")
    mkdirp(tmp1)
    untar(path, exdir = tmp1)

    if (!is.null(ver)) {
      dsc <- read.dcf(file.path(tmp1, pkg_name, "DESCRIPTION"))
      iver <- dsc[, "Version"]
      if (iver != ver) {
        stop("Could not add ", pkg, " ", ver, ", CRAN has ", iver)
      }
    }

    patch <- file.path(lib_dir(), paste0(pkg_name, ".patch"))
    if (file.exists(patch)) {
      message("Patching ", pkg_name)
      system2(
        "patch",
        c("-d", tmp1, "-p", "3", "-i", normalizePath(patch))
      )
    }

    rimraf(file.path(lib, pkg_name))
    file.rename(file.path(tmp1, pkg_name), file.path(lib, pkg_name))
    clean_package(pkg_name)
  }

  add_package <- function(pkg, ver = NULL) {
    addupdate_package(pkg, ver, "add")
  }

  update_package <- function(pkg, ver = NULL) {
    addupdate_package(pkg, ver, "update")
  }

  clean_package <- function(pkgs = NULL) {
    pkgs <- pkgs %||% get_current()
    lib <- lib_dir()
    for (pkg in pkgs) {
      rimraf(file.path(lib, pkg, "build"))
      rimraf(file.path(lib, pkg, "inst", "doc"))
      rimraf(file.path(lib, pkg, "inst", "COPYRIGHTS"))
      rimraf(file.path(lib, pkg, "man", "figures"))
      rd <- dir(file.path(lib, pkg, "man"), pattern = "[.]Rd$")
      unlink(file.path(lib, pkg, "man", rd))
      rimraf(file.path(lib, pkg, "tests"))
      rimraf(file.path(lib, pkg, ".aspell"))
      rimraf(file.path(lib, pkg, "inst", "CITATION"))
      rimraf(file.path(lib, pkg, "MD5"))
      rimraf(file.path(lib, pkg, "README.md"))
    }
  }

  update <- function(pkg = NULL) {
    sts <- check_update(pkg)
    todo <- which(sts$status == "UPDATE")
    for (idx in todo) {
      pkg <- sts[idx, ]
      if (is.na(pkg$required)) {
        message("Deleting ", pkg$package)
        delete_package(pkg$package)
      } else if (is.na(pkg$current)) {
        message("Adding ", pkg$package)
        add_package(pkg$package, pkg$required)
      } else {
        message("Updating ", pkg$package)
        update_package(pkg$package, pkg$required)
      }
    }
  }

  list(
    .envir = environment(),
    current = get_current,
    required = get_required,
    check_update = check_update,
    update = update,
    delete_package = delete_package,
    add_package = add_package,
    update_package = update_package,
    clean_package = clean_package
  )
})
