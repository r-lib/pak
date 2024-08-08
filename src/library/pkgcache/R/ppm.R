
#' Returns the current Posit Package Manager (PPM) repository URL
#'
#' @details
#' This URL has the form `{base}/{repo}`, e.g.
#' `https://packagemanager.posit.co/all`.
#'
#' To configure a hosted PPM instance, set the `PKGCACHE_PPM_URL`
#' environment variable to the base URL (e.g.
#' `https://packagemanager.posit.co`).
#'
#' To use [repo_add()] with PPM snapshots, you may also set the
#' `PKGCACHE_PPM_REPO` environment variable to the name of the default
#' repository.
#'
#' On Linux, instead of setting these environment variables, you can also
#' add a PPM repository to the `repos` option, see [base::options()].
#' If the environment variables are not set, then `ppm_repo_url()` will
#' try to extract the PPM base URL and repository name from this option.
#'
#' If the `PKGCACHE_PPM_URL` environment variable is not set, and the
#' `repos` option does not contain a PPM URL (on Linux), then pkgcache
#' uses the public PPM instance at `https://packagemanager.posit.co`, with
#' the `cran` repository.
#'
#' @return String scalar, the repository URL of the configured PPM
#'   instance. If no PPM instance is configured, then the URL of the Posit
#'   Public Package Manager instance. It includes the repository name, e.g.
#'   `https://packagemanager.posit.co/all`.
#' @export
#' @seealso The 'pkgcache and Posit Package Manager on Linux'
#'   article at <`r pkgdown_url()`>.
#' @seealso [repo_resolve()] and [repo_add()] to find and configure PPM
#'   snapshots.
#' @family PPM functions
#' @examples
#' ppm_repo_url()

ppm_repo_url <- function() {
  # New env vars first
  ppm_env <- Sys.getenv("PKGCACHE_PPM_URL")
  if (ppm_env != "") {
    return(paste0(
      ppm_env,
      "/",
      Sys.getenv("PKGCACHE_PPM_REPO", "cran")
    ))
  }

  # Old env var
  rspm_env <- Sys.getenv("PKGCACHE_RSPM_URL")
  if (rspm_env != "") {
    return(rspm_env)
  }

  # If getOption("repos") has a PPM URL, then use that
  repos <- as.character(getOption("repos"))
  ppm <- is_ppm_linux_repo_url(repos)

  if (any(ppm)) {
    mch <- re_match(repos[ppm][1], re_ppm_linux())
    return(paste0(mch$base, mch$repo))
  }

  # Otherwise default
  "https://packagemanager.posit.co/cran"
}

get_ppm_base_url <- function() {
  dirname(ppm_repo_url())
}

#' List all available Posit Package Manager (PPM) snapshots
#'
#' @details
#' The repository URL of a snapshot has the following form on Windows:
#' ```
#' {base}/{repo}/{id}
#' ```
#' where `{base}` is the base URL for PPM (see [ppm_repo_url()]) and
#' `{id}` is either the date or id of the snapshot, or `latest` for
#' the latest snapshot. E.g. these are equivalent:
#' ```
#' https://packagemanager.posit.co/cran/5
#' https://packagemanager.posit.co/cran/2017-10-10
#' ```
#'
#' On a Linux distribution that has PPM support, the repository URL that
#' contains the binary packages looks like this:
#' ```
#' {base}/{repo}/__linux__/{binary_url}/{id}
#' ```
#' where `{id}` is as before, and `{binary_url}` is a code name for a release
#' of a supported Linux distribution. See the `binary_url` column of the
#' result of [ppm_platforms()] for these code names.
#'
#' @return Data frame with two columns:
#' - `date`: the time the snapshot was taken, a `POSIXct` vector,
#' - `id`:  integer id of the snapshot, this can be used in the repository
#'   URL.
#'
#' @seealso The 'pkgcache and Posit Package Manager on Linux'
#'   article at <`r pkgdown_url()`>.
#' @family PPM functions
#' @export
#' @examplesIf !pkgcache:::is_rcmd_check()
#' ppm_snapshots()

ppm_snapshots <- function() {
  last <- as.Date(format_iso_8601(Sys.time())) - 1
  dts <- seq(as.Date("2017-10-10"), last, by = 1)
  data_frame(
    date = dts,
    id = as.character(dts)
  )
}

#' List all platforms supported by Posit Package Manager (PPM)
#'
#' @return Data frame with columns:
#' - `name`: platform name, this is essentially an identifier,
#' - `os`: operating system, `linux`, `windows` or `macOS` currently,
#' - `binary_url`: the URL segment of the binary repository URL of this
#'   platform, see [ppm_snapshots()].
#' - `distribution`: for Linux platforms the name of the distribution,
#' - `release`: for Linux platforms, the name of the release,
#' - `binaries`: whether PPM builds binaries for this platform.
#'
#' @seealso The 'pkgcache and Posit Package Manager on Linux'
#'   article at <`r pkgdown_url()`>.
#' @family PPM functions
#' @export
#' @examplesIf !pkgcache:::is_rcmd_check()
#' ppm_platforms()

ppm_platforms <- function() {
  plt <- synchronise(async_get_ppm_status(forget = TRUE))$distros
  plt$binary_url[plt$binary_url == ""] <- NA_character_
  as_data_frame(plt)
}

async_get_ppm_status <- function(forget = FALSE, distribution = NULL,
                                 release = NULL, r_version = NULL) {
  tmp2 <- tempfile()

  # is this a known distro?
  known <- if (is.null(distribution)) {
    TRUE
  } else if (is.null(release)) {
    distribution %in% pkgenv$ppm_distros_cached$distribution
  } else {
    mch <- which(
      distribution == pkgenv$ppm_distros_cached$distribution &
      release == pkgenv$ppm_distros_cached$release
    )
    !is.na(mch)
  }

  rver_known <- if (is.null(r_version)) {
    TRUE
  } else {
    r_version <- get_minor_r_version(r_version)
    r_version %in% pkgenv$ppm_r_versions_cached
  }

  # can we used the cached values? Only if
  # * not a forced update, and
  # * distro is known, or we already updated.
  # * r_Version is known, or we already updated
  updated <- !is.null(pkgenv$ppm_distros)
  cached <- !forget && (known || updated) && (rver_known || updated)
  def <- if (isTRUE(cached)) {
    pkgenv$ppm_distros <- pkgenv$ppm_distros_cached
    pkgenv$ppm_r_versions <- pkgenv$ppm_r_versions_cached
    async_constant()
  } else {
    url <- Sys.getenv(
      "PKGCACHE_PPM_STATUS_URL",
      paste0(get_ppm_base_url(), "/__api__/status")
    )
    download_file(url, tmp2)$
      then(function(res) {
        stat <- jsonlite::fromJSON(tmp2, simplifyVector = FALSE)
        dst <- data.frame(
          stringsAsFactors = FALSE,
          name = vcapply(stat$distros, "[[", "name"),
          os = vcapply(stat$distros, "[[", "os"),
          binary_url = vcapply(stat$distros, "[[", "binaryURL"),
          distribution = vcapply(stat$distros, "[[", "distribution"),
          release = vcapply(stat$distros, "[[", "release"),
          binaries = vlapply(stat$distros, "[[", "binaries")
        )
        pkgenv$ppm_distros <- dst
        pkgenv$ppm_distros_cached <- dst

        rvers <- unlist(stat$r_versions)
        pkgenv$ppm_r_versions <- rvers
        pkgenv$ppm_r_versions_cached <- rvers
      })$
      catch(error = function(err) {
        warning("Failed to download PPM status")
      })
  }

  def$
    finally(function() unlink(tmp2))$
    then(function() {
      list(
        distros = pkgenv$ppm_distros,
        r_versions = pkgenv$ppm_r_versions
      )
    })
}

#' Does PPM build binary packages for the current platform?
#'
#' @return `TRUE` or `FALSE`.
#'
#' @export
#' @seealso The 'pkgcache and Posit Package Manager on Linux'
#'   article at <`r pkgdown_url()`>.
#' @family PPM functions
#' @examplesIf !pkgcache:::is_rcmd_check()
#' current_r_platform()
#' ppm_has_binaries()

ppm_has_binaries <- function() {
  current <- current_r_platform_data()

  binaries <-
    (! tolower(Sys.getenv("PKGCACHE_PPM_BINARIES")) %in% c("no", "false", "0", "off")) &&
    current$cpu == "x86_64" &&
    (current$os == "mingw32" || grepl("linux", current$os))

  if (!binaries) return(FALSE)

  current_rver <- get_minor_r_version(getRversion())
  synchronise(async_get_ppm_status(
    distribution = current$distribution,
    release = current$release,
    r_version = current_rver
  ))
  distros <- pkgenv$ppm_distros
  rver <- pkgenv$ppm_r_versions

  if (current$os == "mingw32") {
    binaries <- binaries &&
      "windows" %in% distros$os &&
      all(distros$binaries[distros$os == "windows"]) &&
      current_rver %in% rver

  } else {
    mch <- which(
      distros$distribution == current$distribution &
      distros$release == current$release
    )
    binaries <- binaries &&
      length(mch) == 1 &&
      distros$binaries[mch] &&
      current_rver %in% rver
  }

  binaries
}

#' List all R versions supported by Posit Package Manager (PPM)
#'
#' @return Data frame with columns:
#' - `r_version`: minor R versions, i.e. version numbers containing the
#'   first two components of R versions supported by this PPM instance.
#'
#' @seealso The 'pkgcache and Posit Package Manager on Linux'
#'   article at <`r pkgdown_url()`>.
#' @family PPM functions
#' @export
#' @examplesIf !pkgcache:::is_rcmd_check()
#' ppm_r_versions()

ppm_r_versions <- function() {
  plt <- synchronise(async_get_ppm_status(forget = TRUE))$r_versions
  data_frame(r_version = plt)
}
