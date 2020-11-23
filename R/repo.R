
#' Show the status of CRAN-like repositories
#'
#' It checks the status of the configured or supplied repositories.
#'
#' `repo_ping()` is similar to `repo_status()` but also prints a short
#' summary of the data, and it returns its result invisibly.
#'
#' @param platforms Platforms to use, default is the current platform,
#'   plus source packages.
#' @param r_version R version(s) to use, the default is the current
#'   R version, via [getRversion()].
#' @param bioc Whether to add the Bioconductor repositories. If you
#'   already configured them via `options(repos)`, then you can
#'   set this to `FALSE`.
#' @param cran_mirror The CRAN mirror to use.
#' @return A tibble that has a row for every repository, on every
#' queried platform and R version. It has these columns:
#' * `name`: the name of the repository. This comes from the names
#'   of the configured repositories in `options("repos")`, or
#'   added by pkgcache. It is typically `CRAN` for CRAN, and the
#'   current Bioconductor repositories are `BioCsoft`, `BioCann`,
#'   `BioCexp`, `BioCworkflows`.
#' * `url`: base URL of the repository.
#' * `bioc_version`: Bioconductor version, or `NA` for
#'   non-Bioconductor repositories.
#' * `platform`: platform, possible values are `source`, `macos` and
#'   `windows` currently.
#' * `path`: the path to the packages within the base URL, for a
#'   given platform and R version.
#' * `r_version`: R version, one of the specified R versions.
#' * `ok`: Logical flag, whether the repository contains a metadata
#'   file for the given platform and R version.
#' * `ping`: HTTP response time of the repository in seconds. If
#'   the `ok` column is `FALSE`, then this columns in `NA`.
#' * `error`: the error object if the HTTP query failed for this
#'   repository, platform and R version.
#'
#' @export

repo_status <- function(platforms = NULL, r_version = getRversion(),
                        bioc = TRUE, cran_mirror = NULL) {
  load_extra("tibble")
  remote(
    function(...) asNamespace("pak")$repo_status_internal(...),
    list(platforms, r_version, bioc, cran_mirror)
  )
}

repo_status_internal <- function(platforms = NULL, r_version = getRversion(),
                                 bioc = TRUE, cran_mirror = NULL) {

  platforms <- platforms %||% pkgcache::default_platforms()
  cran_mirror <- cran_mirror %||% pkgcache::default_cran_mirror()

  tab <- pkgcache::repo_status(
    platforms = platforms,
    r_version = r_version,
    bioc = bioc,
    cran_mirror = cran_mirror
  )

  class(tab) <- setdiff(class(tab), "pkgcache_repo_status")
  tab
}

#' @export
#' @rdname repo_status

repo_ping <- function(platforms = NULL, r_version = getRversion(),
                      bioc = TRUE, cran_mirror = NULL) {
  ret <- remote(
    function(...) asNamespace("pak")$repo_ping_internal(...),
    list(platforms, r_version, bioc, cran_mirror)
  )

  cat(ret$fmt, sep = "\n")
  invisible(ret$data)
}

repo_ping_internal <- function(platforms = NULL, r_version = getRversion(),
                               bioc = TRUE, cran_mirror = NULL) {

  platforms <- platforms %||% pkgcache::default_platforms()
  cran_mirror <- cran_mirror %||% pkgcache::default_cran_mirror()

  tab <- pkgcache::repo_status(
    platforms = platforms,
    r_version = r_version,
    bioc = bioc,
    cran_mirror = cran_mirror
  )

  fmt <- utils::capture.output(summary(tab))
  class(tab) <- setdiff(class(tab), "pkgcache_repo_status")
  list(fmt = fmt, data = tab)
}
