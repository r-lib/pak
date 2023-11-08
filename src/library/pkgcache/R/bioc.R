#' Tools for Bioconductor versions and repositories
#'
#' \section{API:}
#'
#' ```
#' get_yaml_config(forget = FALSE)
#' set_yaml_config(text)
#'
#' get_release_version(forget = FALSE)
#' get_devel_version(forget = FALSE)
#'
#' get_version_map(forget = FALSE)
#' get_matching_bioc_version(r_version = getRversion(), forget = FALSE)
#' get_bioc_version(r_version = getRversion(), forget = FALSE)
#'
#' get_repos(bioc_version = "auto", forget = FALSE)
#' ```
#'
#' * `forget`: Whether to forget the cached version of the Bioconductor
#'   config YAML file and download it again.
#' * `text`: character vector (linewise) or scalar, the contents of the
#'   `config.yaml` file, if obtained externally, to be used as a cached
#'   version in the future.
#' * `r_version`: R version string, or `package_version` object.
#' * `bioc_version`: Bioc version string or `package_version` object,
#'   or the string `"auto"` to use the one matching the current R version.
#'
#' `get_yaml_config()` returns the raw contents of the `config.yaml` file,
#' linewise. It is typically not needed, except if one needs information
#' that cannot be surfaces via the other API functions.
#'
#' `set_yaml_config()` can be used to _set_ the contents of the
#' `config.yaml` file. This is useful, if one has already obtained it
#' externally, but wants to use the obtained file with the rest of the
#' bioc standalone code.
#'
#' `get_release_version()` returns the version of the current Bioconductor
#' release.
#'
#' `get_devel_version()` returns the version of the current development
#' version of Bioconductor.
#'
#' `get_version_map()` return the mapping between R versions and
#' Bioconductor versions. Note that this is not a one to one mapping.
#' E.g. currently R `3.6.x` maps to both Bioc `3.9` (Bioc release) and
#' `3.10` (Bioc devel); and also Bioc `3.10` maps to both R `3.6.x` and
#' R `3.7.x` (current R-devel). It returns a data frame with three columns:
#' `bioc_version`, `r_version` and `bioc_status`. The first two columns
#' contain `package_vesion` objects, the third is a factor with levels:
#' `out-of-date`, `release`, `devel`, `future`.
#'
#' `get_matching_bioc_version()` returns the matching Bioc version for an
#' R version. If the R version matches to both a released and a devel
#' version, then the released version is chosen.
#'
#' `get_bioc_version()` returns the matching Bioc version for the
#' specified R version. It does observe the `R_BIOC_VERSION` environment
#' variable, which can be used to force a Bioconductor version. If this is
#' not set, it just calls `get_matching_bioc_version()`.
#'
#' `get_repos()` returns the Bioc repositories of the specified Bioc
#' version. It defaults to the Bioc version that matches the calling R
#' version. It returns a named character vector.
#'
#' \section{NEWS:}
#' * 2019-05-30 First version in remotes.
#' * 2020-03-22 get_matching_bioc_version() is now correct if the current
#'              R version is not in the builtin mapping.
#' * 2020-11-21 Update internal map for 3.12.
#' * 2023-05-08 Add 'books' repo.
#'
#' @name bioconductor
#' @keywords internal
#' @noRd
NULL


bioconductor <- local({

  # -------------------------------------------------------------------
  # Configuration that does not change often

  config_url <- function() {
    Sys.getenv(
      "R_BIOC_CONFIG_URL",
      "https://bioconductor.org/config.yaml"
    )
  }

  builtin_map <- list(
    "2.1"  = package_version("1.6"),
    "2.2"  = package_version("1.7"),
    "2.3"  = package_version("1.8"),
    "2.4"  = package_version("1.9"),
    "2.5"  = package_version("2.0"),
    "2.6"  = package_version("2.1"),
    "2.7"  = package_version("2.2"),
    "2.8"  = package_version("2.3"),
    "2.9"  = package_version("2.4"),
    "2.10" = package_version("2.5"),
    "2.11" = package_version("2.6"),
    "2.12" = package_version("2.7"),
    "2.13" = package_version("2.8"),
    "2.14" = package_version("2.9"),
    "2.15" = package_version("2.11"),
    "3.0"  = package_version("2.13"),
    "3.1"  = package_version("3.0"),
    "3.2"  = package_version("3.2"),
    "3.3"  = package_version("3.4"),
    "3.4"  = package_version("3.6"),
    "3.5"  = package_version("3.8"),
    "3.6"  = package_version("3.10"),
    "4.0"  = package_version("3.12"),
    "4.1"  = package_version("3.14"),
    "4.2"  = package_version("3.16"),
    "4.3"  = package_version("3.17"),
    "4.4"  = package_version("3.18")
  )

  # -------------------------------------------------------------------
  # Cache

  devel_version <- NULL
  release_version <- NULL
  version_map <- NULL
  yaml_config <- NULL

  clear_cache <- function() {
    devel_version <<- NULL
    release_version <<- NULL
    version_map <<- NULL
    yaml_config <<- NULL
  }

  # -------------------------------------------------------------------
  # API

  get_yaml_config <- function(forget = FALSE) {
    if (forget || is.null(yaml_config)) {
      new <- tryCatch(read_url(config_url()), error = function(x) x)
      if (inherits(new, "error")) {
        http_url <- sub("^https", "http", config_url())
        new <- tryCatch(read_url(http_url), error = function(x) x)
      }
      if (inherits(new, "error")) stop(new)
      yaml_config <<- new
    }

    yaml_config
  }

  set_yaml_config <- function(text) {
    if (length(text) == 1) text <- strsplit(text, "\n", fixed = TRUE)[[1]]
    yaml_config <<- text
  }

  get_release_version <- function(forget = FALSE) {
    if (forget || is.null(release_version)) {
      yaml <- get_yaml_config(forget)
      pattern <- "^release_version: \"(.*)\""
      release_version <<- package_version(
        sub(pattern, "\\1", grep(pattern, yaml, value=TRUE))
      )
    }
    release_version
  }

  get_devel_version <- function(forget = FALSE) {
    if (forget || is.null(devel_version)) {
      yaml <- get_yaml_config(forget)
      pattern <- "^devel_version: \"(.*)\""
      devel_version <<- package_version(
        sub(pattern, "\\1", grep(pattern, yaml, value=TRUE))
      )
    }
    devel_version
  }

  get_version_map <- function(forget = FALSE) {
    if (forget || is.null(version_map)) {
      txt <- get_yaml_config(forget)
      grps <- grep("^[^[:blank:]]", txt)
      start <- match(grep("r_ver_for_bioc_ver", txt), grps)
      map <- txt[seq(grps[start] + 1, grps[start + 1] - 1)]
      map <- trimws(gsub("\"", "", sub(" #.*", "", map)))
      pattern <- "(.*): (.*)"
      bioc <- package_version(sub(pattern, "\\1", map))
      r <- package_version(sub(pattern, "\\2", map))
      status <- rep("out-of-date", length(bioc))
      release <- get_release_version()
      devel <- get_devel_version()
      status[bioc == release] <- "release"
      status[bioc == devel] <- "devel"

      # append final version for 'devel' R
      bioc <- c(
        bioc, max(bioc)
      )
      r <- c(r, package_version(paste(unlist(max(r)) + 0:1, collapse = ".")))
      status <- c(status, "future")

      version_map <<- rbind(
        .VERSION_MAP_SENTINEL,
        data.frame(
          bioc_version = bioc, r_version = r,
          bioc_status = factor(
            status,
            levels = c("out-of-date", "release", "devel", "future")
          )
        )
      )
    }
    version_map
  }

  get_matching_bioc_version <- function(r_version = getRversion(),
                                        forget = FALSE) {

    minor <- as.character(get_minor_r_version(r_version))
    if (minor %in% names(builtin_map)) return(builtin_map[[minor]])

    # If we are not in the map, then we need to look this up in
    # YAML data. It is possible that the current R version matches multiple
    # Bioc versions. Then we choose the latest released version. If none
    # of them were released (e.g. they are 'devel' and 'future'), then
    # we'll use the 'devel' version.

    map <- get_version_map(forget = forget)
    mine <- which(package_version(minor) == map$r_version)
    if (length(mine) == 0) {
      mine <- NA
    } else if (length(mine) > 1) {
      if ("release" %in% map$bioc_status[mine]) {
        mine <- mine["release" == map$bioc_status[mine]]
      } else if ("devel" %in% map$bioc_status[mine]) {
        mine <- mine["devel" == map$bioc_status[mine]]
      } else {
        mine <- rev(mine)[1]
      }
    }
    if (!is.na(mine)) return(map$bioc_version[mine])

    # If it is not even in the YAML, then it must be some very old
    # or very new version. If old, we fail. If new, we assume bioc-devel.
    if (package_version(minor) < "2.1") {
      stop("R version too old, cannot run Bioconductor")
    }

    get_devel_version()
  }

  get_bioc_version <- function(r_version = getRversion(),
                               forget = FALSE) {
    if (nzchar(v <- Sys.getenv("R_BIOC_VERSION", ""))) {
      return(package_version(v))
    }
    get_matching_bioc_version(r_version, forget = forget)
  }

  get_repos <- function(bioc_version = "auto", forget = FALSE) {
    if (identical(bioc_version, "auto")) {
      bioc_version <- get_bioc_version(getRversion(), forget)
    } else {
      bioc_version <- package_version(bioc_version)
    }
    mirror <- Sys.getenv("R_BIOC_MIRROR", "https://bioconductor.org")
    mirror <- getOption("BioC_mirror", mirror)
    repos <- c(
      BioCsoft      = "{mirror}/packages/{bv}/bioc",
      BioCann       = "{mirror}/packages/{bv}/data/annotation",
      BioCexp       = "{mirror}/packages/{bv}/data/experiment",
      BioCworkflows =
        if (bioc_version >= "3.7") "{mirror}/packages/{bv}/workflows",
      BioCextra     =
        if (bioc_version <= "3.5") "{mirror}/packages/{bv}/extra",
      BioCbooks     =
        if (bioc_version >= "3.12") "{mirror}/packages/{bv}/books"
    )

    ## It seems that if a repo is not available yet for bioc-devel,
    ## they redirect to the bioc-release version, so we do not need to
    ## parse devel_repos from the config.yaml file

    sub("{mirror}", mirror, fixed = TRUE,
        sub("{bv}", bioc_version, repos, fixed = TRUE))
  }

  # -------------------------------------------------------------------
  # Internals

  read_url <- function(url) {
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)
    suppressWarnings(download.file(url, tmp, quiet = TRUE))
    if (!file.exists(tmp) || file.info(tmp)$size == 0) {
      stop("Failed to download `", url, "`")
    }
    readLines(tmp, warn = FALSE)
  }

  .VERSION_SENTINEL <- local({
    version <- package_version(character())
    class(version) <- c("unknown_version", class(version))
    version
  })

  .VERSION_MAP_SENTINEL <- data.frame(
    bioc_version = .VERSION_SENTINEL,
    r_version = .VERSION_SENTINEL,
    bioc_status = factor(
      factor(),
      levels = c("out-of-date", "release", "devel", "future")
    )
  )

  get_minor_r_version <- function (x) {
    package_version(x)[,1:2]
  }

  # -------------------------------------------------------------------

  structure(
    list(
      .internal = environment(),
      get_yaml_config = get_yaml_config,
      set_yaml_config = set_yaml_config,
      get_release_version = get_release_version,
      get_devel_version = get_devel_version,
      get_version_map = get_version_map,
      get_matching_bioc_version = get_matching_bioc_version,
      get_bioc_version = get_bioc_version,
      get_repos = get_repos
    ),
    class = c("standalone_bioc", "standalone"))
})
