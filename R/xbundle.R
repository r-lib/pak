
should_bundle <- function() {
  # Do not bundle in pkgload::load_all()
  pkgdir <- Sys.getenv("R_PACKAGE_DIR", "")
  if (pkgdir == "") return(FALSE)

  if (Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_", "") != "") {
    return(FALSE)
  }

  TRUE
}

bundle_install_deps <- function() {
  pkgdir <- Sys.getenv("R_PACKAGE_DIR", "")
  if (pkgdir != "") {
    desc <- file.path(pkgdir, "DESCRIPTION")
    deps <- read.dcf(desc)[, "Config/pak/dependencies"]
  } else {
    deps <- utils::packageDescription("pak")$`Config/pak/dependencies`
  }

  pkgs <- bundle_parse_deps(deps)

  lib <- file.path(pkgdir, "library")
  dir.create(lib, showWarnings = FALSE, recursive = TRUE)

  old <- options(install.packages.compile.from.source = "always")
  on.exit(options(old), add = TRUE)
  
  for (pkg in pkgs) {
    utils::install.packages(
      pkg, lib = lib, quiet = TRUE,
      INSTALL_opts = "--no-staged-install",
      dependencies = FALSE
    )
  }

  bundle_cleanup_library(lib)
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

  # TODO: strip if we can?
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

  deps <- deps[setdiff(names(deps), base_packages())]
  deps[] <- lapply(deps, setdiff, base_packages())

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

bundle <- function() {
  bundle_install_deps()
}

if (should_bundle()) {
  bundle()
}
