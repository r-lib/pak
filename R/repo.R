
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
#' @return A data frame that has a row for every repository, on every
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
#' @family repository functions
#' @export
#' @section Examples:
#' ```{asciicast repo-status}
#' repo_status()
#' ```
#'
#' ```{asciicast repo-status-2}
#' repo_status(
#'   platforms = c("windows", "macos"),
#'   r_version = c("4.0", "4.1")
#' )
#' ```
#'
#' ```{asciicast repo-status-3, asciicast_cols = 77, asciicast_width = 77, R.options = list(cli.width = 77, width = 77)}
#' repo_ping()
#' ```

repo_status <- function(platforms = NULL, r_version = getRversion(),
                        bioc = TRUE, cran_mirror = NULL) {
  load_extra("pillar")
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

#' Query the currently configured CRAN-like repositories
#'
#' pak uses the `repos` option, see [options()]. It also automatically
#' adds a CRAN mirror if none is set up, and the correct version of the
#' Bioconductor repositories. See the `cran_mirror` and `bioc` arguments.
#'
#' `repo_get()` returns the table of the currently configured repositories.
#'
#' @param r_version R version to use to determine the correct Bioconductor
#' version, if `bioc = TRUE`.
#' @param bioc Whether to automatically add the Bioconductor repositories
#' to the result.
#' @param cran_mirror CRAN mirror to use. Leave it at `NULL` to use the
#' mirror in `getOption("repos")` or an automatically selected one.
#'
#' @family repository functions
#' @export
#' @section Examples:
#' ```{asciicast repo-get}
#' repo_get()
#' ```

repo_get <- function(r_version = getRversion(),
                     bioc = TRUE, cran_mirror = NULL) {
  load_extra("pillar")
  remote(
    function(...) asNamespace("pak")$repo_get_internal(...),
    list(r_version, bioc, cran_mirror)
  )
}

repo_get_internal <- function(r_version = getRversion(), bioc = TRUE,
                              cran_mirror = NULL) {
  cran_mirror = cran_mirror %||% pkgcache::default_cran_mirror()
  pkgcache::repo_get(r_version, bioc, cran_mirror)
}

#' Add a new CRAN-like repository
#'
#' Add a new repository to the list of repositories that pak uses to
#' look for packages.
#'
#' `repo_add()` adds new repositories. It resolves the specified
#' repositories using `repo_resolve()` and then modifies the `repos`
#' global option.
#'
#' `repo_add()` only has an effect in the current R session. If you
#' want to keep your configuration between R sessions, then set the
#' `repos` option to the desired value in your user or project `.Rprofile`
#' file.
#'
#' @param ... Repository specifications, possibly named character vectors.
#' See details below.
#' @param .list List or character vector of repository specifications.
#' This argument is easier to use programmatically than `...`. See
#' details below.
#'
#' @details
#' # Repository specifications
#'
#' The format of a repository specification is a named or unnamed
#' character scalar. If the name is missing, pak adds a name
#' automatically. The repository named `CRAN` is the main CRAN repository,
#' but otherwise names are informational.
#'
#' Currently supported repository specifications:
#' - URL pointing to the root of the CRAN-like repository. Example:
#'   ```
#'   https://cloud.r-project.org
#'   ```
#' - `RSPM@<date>`, RSPM (RStudio Package Manager) snapshot, at the
#'   specified date.
#' - `RSPM@<package>-<version>` RSPM snapshot, for the day after the
#'   release of `<version>` of `<package>`.
#' - `RSPM@R-<version>` RSPM snapshot, for the day after R `<version>`
#'   was released.
#' - `MRAN@<date>`, MRAN (Microsoft R Application Network) snapshot, at
#'   the specified date.
#' - `MRAN@<package>-<version>` MRAN snapshot, for the
#'   day after the release of `<version>` of `<package>`.
#' - `MRAN@R-<version>` MRAN snapshot, for the day
#'   after R `<version>` was released.
#'
#'
#' Notes:
#' * See more about RSPM at `https://packagemanager.rstudio.com/client/#/`.
#' * See more about MRAN snapshots at
#'   <https://mran.microsoft.com/timemachine>.
#' * All dates (or times) can be specified in the ISO 8601 format.
#' * If RSPM does not have a snapshot available for a date, the next
#'   available date is used.
#' * Dates that are before the first, or after the last RSPM snapshot
#'   will trigger an error.
#' * Dates before the first, or after the last MRAN snapshot will trigger
#'   an error.
#' * Unknown R or package versions will trigger an error.
#'
#' @family repository functions
#' @export
#' @section Exaples:
#' ```{asciicast repo-add}
#' repo_add(RSPMdplyr100 = "RSPM@dplyr-1.0.0")
#' repo_get()
#' ```
#'
#' ```{asciicast repo-add-2}
#' repo_resolve("MRAN@2020-01-21")
#' ```
#' ```{asciicast repo-add-3}
#' repo_resolve("RSPM@2020-01-21")
#' ```
#' ```{asciicast repo-add-4}
#' repo_resolve("MRAN@dplyr-1.0.0")
#' ```
#' ```{asciicast repo-add-5}
#' repo_resolve("RSPM@dplyr-1.0.0")
#' ```
#' ```{asciicast repo-add-6}
#' repo_resolve("MRAN@R-4.0.0")
#' ```
#' ```{asciicast repo-add-7}
#' repo_resolve("RSPM@R-4.0.0")
#' ```

repo_add <- function(..., .list = NULL) {
  new <- c(list(...), .list)
  ret <- remote(
    function(...) asNamespace("pak")$repo_add_internal(...),
    list(.list = new)
  )
  options(repos = ret$option)
  invisible(ret$tab)
}

repo_add_internal <- function(.list) {
  tab <- pkgcache::repo_add(.list = .list)
  list(option = getOption("repos"), tab = tab)
}

#' @param spec Repository specification, a possibly named character
#' scalar.
#' @return `repo_resolve()` returns a named character scalar, the URL
#' of the repository.
#' @rdname repo_add
#' @export

repo_resolve <- function(spec) {
  remote(function(spec) pkgcache::repo_resolve(spec), list(spec))
}
