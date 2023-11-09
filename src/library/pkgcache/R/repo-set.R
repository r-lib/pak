
#' Query and set the list of CRAN-like repositories
#'
#' pkgcache uses the `repos` option, see [options()]. It also automatically
#' uses the current Bioconductor repositories, see [bioc_version()].
#' These functions help to query and manipulate the `repos` option.
#'
#' @details
#' `repo_get()` queries the repositories pkgcache uses. It uses the
#' `repos` option (see [options]), and also the default Bioconductor
#' repository.
#'
#' @param bioc Whether to add Bioconductor repositories, even if they
#' are not configured in the `repos` option.
#' @param r_version R version(s) to use for the Bioconductor repositories,
#' if `bioc` is `TRUE`.
#' @param cran_mirror The CRAN mirror to use, see
#'   [default_cran_mirror()].
#'
#' @return
#' `repo_get()` returns a data frame with columns:
#' * `name`: repository name. Names are informational only.
#' * `url`: repository URL.
#' * `type`: repository type. This is also informational, currently it
#'   can be `cran` for CRAN, `bioc` for a Bioconductor repository, and
#'   `cranlike`: for other repositories.
#' * `r_version`: R version that is supposed to be used with this
#'   repository. This is only set for Bioconductor repositories. It is `*`
#'   for others. This is also informational, and not used when retrieving
#'   the package metadata.
#' * `bioc_version`: Bioconductor version. Only set for Bioconductor
#'   repositories, and it is `NA` for others.
#'
#' @export
#' @family repository functions
#'
#' @examples
#' repo_get()

repo_get <- function(r_version = getRversion(), bioc = TRUE,
                     cran_mirror = default_cran_mirror()) {
  cmc__get_repos(
    getOption("repos"),
    bioc = bioc,
    cran_mirror = cran_mirror,
    as.character(r_version)
  )
}

#' @rdname repo_get
#' @param spec A single repository specification, a possibly named
#'   character scalar. See details below.
#' @details
#'   `repo_resolve()` resolves a single repository specification to a
#'   repository URL.
#' @return
#'   `repo_resolve()` returns a named character vector, with the URL(s) of
#'   the repository.
#'
#' @export
#' @examplesIf pkgcache:::run_examples()
#' repo_resolve("PPM@2020-01-21")
#' #' repo_resolve("PPM@dplyr-1.0.0")
#' #' repo_resolve("PPM@R-4.0.0")

repo_resolve <- function(spec) {
  repo_sugar(spec, names(spec))
}

#' @rdname repo_get
#' @param ...  Repository specifications. See details below.
#' @param .list List or character vector of repository specifications,
#'   see details below.
#' @details
#'   `repo_add()` adds a new repository to the `repos` option. (To remove
#'   a repository, call `option()` directly, with the subset that you want
#'   to keep.)
#' @return
#'   `repo_add()` returns the same data frame as `repo_get()`, invisibly.
#' @export

repo_add <- function(..., .list = NULL) {
  repo_add_internal(..., .list = .list)
  invisible(repo_get())
}

repo_add_internal <- function(..., .list = NULL) {
  new <- c(list(...), .list)

  if (length(new) == 0) return(invisible(repo_get()))

  toadd <- unlist(mapply(
    repo_sugar,
    new,
    names(new) %||% rep("", length(new)),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))

  names(toadd) <- make.unique(names(toadd), sep = "_")

  current <- getOption("repos")
  upd <- modify_vec(current, toadd)

  options(repos = upd)
}

#' @rdname repo_get
#' @param repos A list or character vector of repository specifications.
#' @param expr R expression to evaluate.
#' @details
#'   `with_repo()` temporarily adds the repositories in `repos`,
#'   evaluates `expr`, and then resets the configured repositories.
#' @return
#'   `with_repo()` returns the value of `expr`.
#' @export
#' @examplesIf pkgcache:::run_examples()
#' with_repo(c(CRAN = "PPM@dplyr-1.0.0"), repo_get())
#' with_repo(c(CRAN = "PPM@dplyr-1.0.0"), meta_cache_list(package = "dplyr"))
#'
#' with_repo(c(CRAN = "MRAN@2018-06-30"), summary(repo_status()))

with_repo <- function(repos, expr) {
  old <- repo_add_internal(.list = repos)
  on.exit(options(old))
  expr
}

# ## PPM
#
# PPM@latest
# PPM@2021-02-04T14:25:00Z
# PPM@2021-02-04
# PPM@dplyr@1.0.0
# PPM@dplyr-1.0.0
# PPM@R@4.0.1
# PPM@R-4.0.1
#
# ## MRAN
#
# MRAN@2021-02-04T14:25:00Z
# MRAN@2021-02-04
# MRAN@dplyr@1.0.0
# MRAN@dplyr@1.0.0
# MRAN@R-4.0.1
# MRAN@R-4.0.1
#
# URLs
#
# https://cloud.r-project.org
#
# Paths (don't yet work with pkgcache)
#
# /Users/gaborcsardi/CRAN

repo_sugar <- function(x, nm) {
  psd <- parse_url(x)

  # URL
  if (!is.na(psd$protocol)) {
    repo_sugar_url(x, nm)

  } else if (grepl("^MRAN@", x)) {
    repo_sugar_mran(x, nm)

  } else if (grepl("^PPM@", x) || grepl("^RSPM@", x)) {
    repo_sugar_ppm(x, nm)

  } else {
    repo_sugar_path(x, nm)
  }
}

repo_sugar_url <- function(x, nm) {
  if (is.null(nm) || nm == "") nm <- "EXTRA"
  structure(x, names = nm)
}

repo_sugar_path <- function(x, nm) {
  if (is.null(nm) || nm == "") nm <- "LOCAL"
  structure(x, names = nm)
}

repo_sugar_mran <- function(x, nm) {
  if (is.null(nm) || nm == "") nm <- "CRAN"
  date <- parse_spec(sub("^MRAN@", "", x))
  if (date < "2017-10-10") {
    stop("PPM snapshots go back to 2017-10-10 only")
  }

  mran <- Sys.getenv(
    "PKGCACHE_MRAN_URL",
    "https://packagemanager.posit.co/cran"
  )
  structure(paste0(mran, "/", date), names = nm)
}

repo_sugar_ppm <- function(x, nm) {
  if (is.null(nm) || nm == "") nm <- "CRAN"
  x <- sub("^PPM@", "", x)
  x <- sub("^RSPM@", "", x)
  date <- parse_spec(x)

  # do we potentially have binaries?
  current <- current_r_platform_data()
  current_rver <- get_minor_r_version(getRversion())
  binaries <-
    ! tolower(Sys.getenv("PKGCACHE_PPM_BINARIES")) %in% c("no", "false", "0", "off") &&
    current$cpu == "x86_64" &&
    grepl("linux", current$os)

  # if we may have binaries, then get the distro data as well
  synchronise(when_all(
    async_get_ppm_versions(date = if (as.character(date) == "latest") NULL else date),
    if (binaries) {
      async_get_ppm_status(
        distribution = current$distribution,
        release = current$release,
        r_version = current_rver
      )
    } else {
      async_constant()
    }
  ))

  # do we really have binaries? check in PPM status
  distros <- pkgenv$ppm_distros
  rvers <- pkgenv$ppm_r_versions
  mch <- which(
    distros$distribution == current$distribution &
    distros$release == current$release
  )
  binaries <- binaries &&
    length(mch) == 1 &&
    distros$binaries[mch] &&
    current_rver %in% rvers

  # search for date
  if (as.character(date) == "latest") {
    ver <- "latest"
  } else {
    vers <- pkgenv$ppm_versions
    ppm_dates <- names(vers)
    if (date < ppm_dates[1]) {
      stop("PPM snapshots go back to ", as.Date(ppm_dates[1]), " only")
    }
    sel <- which(date <= ppm_dates)[1]
    if (is.na(sel)) {
      stop("Cannot find matching PPM snapshot for ", date)
    }
    ver <- vers[[sel]]
  }

  # create repo URL
  ppm <- ppm_repo_url()

  if (binaries) {
    structure(
      paste0(ppm, "/", "__linux__/", distros$binary_url[mch], "/", ver),
      names = nm
    )
  } else {
    structure(paste0(ppm, "/", ver), names = nm)
  }
}

parse_spec <- function(x) {
  if (x == "latest") {
    x
  } else if (grepl("^R[-@]", x)) {
    parse_spec_r(sub("^R[-@]", "", x))
  } else if (!is.na(at <- parse_iso_8601(x))) {
    parse_spec_date(at)
  } else {
    parse_spec_pkg(x)
  }
}

parse_spec_r <- function(x) {
  if (is.null(pkgenv$r_versions) ||
      package_version(x) > last(pkgenv$r_versions)$version) {
    tryCatch(
      pkgenv$r_versions <- get_r_versions(),
      error = function(err) warning("Failed to update list of R versions")
    )
  }

  vers <- vcapply(pkgenv$r_versions, "[[", "version")
  dates <- parse_iso_8601(vcapply(pkgenv$r_versions, "[[", "date"))

  if (! x %in% vers) {
    stop("Unknown R version: '", x, "'")
  }

  next_day(dates[match(x, vers)])
}

get_r_versions <- function() {
  url <- Sys.getenv(
    "PKGCACHE_R_VERSIONS_URL",
    "https://api.r-hub.io/rversions/r-versions"
  )
  res <- curl::curl_fetch_memory(url)
  if (res$status_code >= 300) {
    stop("Failed to download R versions from '", url, "'")
  }
  jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE)
}

parse_spec_date <- function(at) {
  as.Date(at)
}

parse_spec_pkg <- function(x) {
  pkg <- sub("[-@].*$", "", x)
  ver <- sub("^.*[-@]", "", x)
  if (pkg == "" || ver == "") {
    stop("Invalid package version: '", x, "'")
  }

  if (is.null(pkgenv$pkg_versions[[pkg]]) ||
      package_version(ver) > last(names(pkgenv$pkg_versions[[pkg]]))) {
    pkgenv$pkg_versions[[pkg]] <- get_pkg_versions(pkg)
  }

  vers <- pkgenv$pkg_versions[[pkg]]
  if (! ver %in% names(vers)) {
    stop("Unknown '", pkg, "' version: '", ver, "'")
  }

  next_day(parse_iso_8601(vers[[ver]]))
}

get_pkg_versions <- function(pkg) {
  crandb <- Sys.getenv(
    "PKGCACHE_CRANDB_URL",
    "https://crandb.r-pkg.org/%s/all"
  )
  url <- sprintf(crandb, pkg)
  res <- curl::curl_fetch_memory(url)
  if (res$status_code == 404) {
    stop("Cannot find package versions for '", pkg, "'. Is it a CRAN package?")
  }
  if (res$status_code >= 300) {
    stop("Failed to download package versions for '", pkg, "' from '", url, "'")
  }

  jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE)$timeline
}

next_day <- function(x) {
  as.Date(x) + 1
}

#' @name repo_get
#' @rdname repo_get
#' @details
#' # Repository specifications
#'
#' The format of a repository specification is a named or unnamed
#' character scalar. If the name is missing, pkgcache adds a name
#' automatically. The repository named `CRAN` is the main CRAN repository,
#' but otherwise names are informational.
#'
#' Currently supported repository specifications:
#' - URL pointing to the root of the CRAN-like repository. Example:
#'   ```
#'   https://cloud.r-project.org
#'   ```
#' - `PPM@latest`, PPM (Posit Package Manager, formerly RStudio Package
#'   Manager), the latest snapshot.
#' - `PPM@<date>`, PPM (Posit Package Manager, formerly RStudio Package
#'   Manager) snapshot, at the specified date.
#' - `PPM@<package>-<version>` PPM snapshot, for the day after the
#'   release of `<version>` of `<package>`.
#' - `PPM@R-<version>` PPM snapshot, for the day after R `<version>`
#'   was released.
#'
#' Still works for dates starting from 2017-10-10, but now deprecated,
#' because MRAN is discontinued:
#' - `MRAN@<date>`, MRAN (Microsoft R Application Network) snapshot, at
#'   the specified date.
#' - `MRAN@<package>-<version>` MRAN snapshot, for the
#'   day after the release of `<version>` of `<package>`.
#' - `MRAN@R-<version>` MRAN snapshot, for the day
#'   after R `<version>` was released.
#'
#' Notes:
#' * See more about PPM at <https://packagemanager.posit.co/client/#/>.
#' * The `RSPM@` prefix is still supported and treated the same way as
#'   `PPM@`.
#' * The MRAN service is now retired, see
#'   <https://techcommunity.microsoft.com/t5/azure-sql-blog/microsoft-r-application-network-retirement/ba-p/3707161>
#'   for details.
#' * `MRAN@...` repository specifications now resolve to PPM, but note that
#'   PPM snapshots are only available from 2017-10-10. See more about this
#'   at <https://posit.co/blog/migrating-from-mran-to-posit-package-manager/>.
#' * All dates (or times) can be specified in the ISO 8601 format.
#' * If PPM does not have a snapshot available for a date, the next
#'   available date is used.
#' * Dates that are before the first, or after the last PPM snapshot
#'   will trigger an error.
#' * Unknown R or package versions will trigger an error.
#'
NULL
