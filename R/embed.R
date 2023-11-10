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
      "base", "compiler", "datasets", "graphics", "grDevices", "grid",
      "methods", "parallel", "splines", "stats", "stats4", "tcltk",
      "tools", "utils"
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
      if (!is_string(dpkg) || dpkg != pkg || !is_string(dver)) {
        stop("Invalid ", pkg, "DESCRIPTION")
      }
      dver
    })
    tab <- data.frame(
      stringsAsFactors = FALSE,
      package = pkgs,
      version = as.character(unlist(vers))
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

  lookup_dep_gh <- function(slug) {
    "TODO":"TODO"
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

  check_update <- function(pkg = NULL) {
    check_pak_root()
    rpkg <- pkg %||% get_required()

    av <- utils::available.packages(repos = get_repos(), type = "source")
    wh <- match(rpkg, av[, "Package"])
    if (anyNA(wh)) {
      stop(
        "REquired packages not available: ",
        paste(rpkg[is.na(wh)], collapse = ", ")
      )
    }
    vers <- av[wh, "Version"]

    crnt <- get_current()
    wh <- match(rpkg, crnt$package)

    tab <- data.frame(
      stringsAsFactors = FALSE,
      package = rpkg,
      required = vers,
      current = crnt$version[wh]
    )
    rownames(tab) <- NULL

    if (is.null(pkg)) {
      xtr <- setdiff(crnt$package, rpkg)
      if (length(xtr) > 0) {
        wh <- match(xtr, crnt$package)
        tab <- rbind(
          tab,
          data.frame(
            stringsAsFactors = FALSE,
            package = xtr,
            required = NA_character_,
            current = crnt$version[wh]
          )
        )
      }
    }

    tab$status <- ifelse(
      !is.na(tab$required) & !is.na(tab$current) &
        tab$required == tab$current,
      "OK",
      "UPDATE"
    )

    tab
  }

  delete_package <- function(pkg) {
    rimraf(file.path(lib_dir(), pkg))
  }

  addupdate_package <- function(pkg, ver = NULL, mode = c("add", "update")) {
    mode <- match.arg(mode)
    stopifnot(
      is_string(pkg),
      is.null(ver) || is_string(ver)
    )

    lib <- lib_dir()
    if (mode == "add") {
      if (file.exists(file.path(lib, pkg))) {
        stop("Package already in library: ", pkg)
      }
    }

    dir.create(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    path <- utils::download.packages(pkg, tmp, repos = get_repos())
    tmp1 <- file.path(tmp, "pkg")
    untar(path[, 2], exdir = tmp1)

    if (!is.null(ver)) {
      dsc <- read.dcf(file.path(tmp1, pkg, "DESCRIPTION"))
      iver <- dsc[, "Version"]
      if (iver != ver) {
        stop("Could not add ", pkg, " ", ver, ", CRAN has ", iver)
      }
    }

    patch <- file.path(lib_dir(), paste0(pkg, ".patch"))
    if (file.exists(patch)) {
      message("Patching ", pkg)
      system2(
        "patch",
        c("-d", tmp1, "-p", "3", "-i", normalizePath(patch))
      )
    }

    rimraf(file.path(lib, pkg))
    file.rename(file.path(tmp1, pkg), file.path(lib, pkg))
    clean_package(pkg)
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
      rimraf(file.path(lib, pkg, "man", "figures"))
      rd <- dir(file.path(lib, pkg, "man"), pattern = "[.]Rd$")
      unlink(file.path(lib, pkg, "man", rd))
      rimraf(file.path(lib, pkg, "tests"))
      rimraf(file.path(lib, pkg, ".aspell"))
      rimraf(file.path(lib, pkg, "inst", "CITATION"))
      rimraf(file.path(lib, pkg, "MD5"))
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
