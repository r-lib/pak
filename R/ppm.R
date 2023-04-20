
#' Does PPM build binary packages for the current platform?
#'
#' @return `TRUE` or `FALSE`.
#'
#' @export
#' @seealso The 'pkgcache and Posit Package Manager on Linux'
#'   article at <`r pkgcache:::pkgdown_url()`>.
#' @family PPM functions
#' @examplesIf FALSE
#' system_r_platform()
#' ppm_has_binaries()

ppm_has_binaries <- function() {
  remote(
    function(...) asNamespace("pak")$ppm_has_binaries_internal(...),
    list()
  )
}

ppm_has_binaries_internal <- function() {
  pkgcache::ppm_has_binaries()
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
#'   article at <`r pkgcache:::pkgdown_url()`>.
#' @family PPM functions
#' @export
#' @examplesIf FALSE
#' ppm_platforms()

ppm_platforms <- function() {
  load_extra("pillar")
  remote(
    function(...) asNamespace("pak")$ppm_platforms_internal(...),
    list()
  )
}

ppm_platforms_internal <- function() {
  pkgcache::ppm_platforms()
}

#' List all R versions supported by Posit Package Manager (PPM)
#'
#' @return Data frame with columns:
#' - `r_version`: minor R versions, i.e. version numbers containing the
#'   first two components of R versions supported by this PPM instance.
#'
#' @seealso The 'pkgcache and Posit Package Manager on Linux'
#'   article at <`r pkgcache:::pkgdown_url()`>.
#' @family PPM functions
#' @export
#' @examplesIf FALSE
#' ppm_r_versions()

ppm_r_versions <- function() {
  load_extra("pillar")
  remote(
    function(...) asNamespace("pak")$ppm_r_versions_internal(...),
    list()
  )
}

ppm_r_versions_internal <- function() {
  pkgcache::ppm_r_versions()
}

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
#' In the environment variables are not set, then `ppm_repo_url()` will
#' try extract the PPM base URL and repository name from this option.
#'
#' If the `PKGCACHE_PPM_URL` environment variable is not set, and the
#' `repos` option does not contain a PPM URL (on Linux), then pak
#' uses the public PPM instance at `https://packagemanager.posit.co`, with
#' the `cran` repository.
#'
#' @return String scalar, the repository URL of the configured PPM
#'   instance. If no PPM instance is configured, then the URL of the Posit
#'   Public Package Manager instance. It includes the repository name, e.g.
#'   `https://packagemanager.posit.co/all`.
#' @export
#' @seealso The 'pkgcache and Posit Package Manager on Linux'
#'   article at <`r pkgcache:::pkgdown_url()`>.
#' @seealso [repo_resolve()] and [repo_add()] to find and configure PPM
#'   snapshots.
#' @family PPM functions
#' @examplesIf FALSE
#' ppm_repo_url()

ppm_repo_url <- function() {
  remote(
    function(...) asNamespace("pak")$ppm_repo_url_internal(...),
    list()
  )
}

ppm_repo_url_internal <- function() {
  pkgcache::ppm_repo_url()
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
#'   article at <`r pkgcache:::pkgdown_url()`>.
#' @family PPM functions
#' @export
#' @examplesIf FALSE
#' ppm_snapshots()

ppm_snapshots <- function() {
  remote(
    function(...) asNamespace("pak")$ppm_snapshots_internal(...),
    list()
  )
}

ppm_snapshots_internal <- function() {
  pkgcache::ppm_snapshots()
}
