#' Show the status of CRAN-like repositories
#'
#' It checks the status of the configured or supplied repositories.
#'
#' `repo_ping()` is similar to `repo_status()` but also prints a short
#' summary of the data, and it returns its result invisibly.
#'
#' @param platforms Platforms to use, default is the current platform,
#'   plus source packages, via the [`pkg.platforms`][pak-config] option.
#' @param r_version R version(s) to use, the default is the current
#'   R version, via [getRversion()].
#' @param bioc Whether to add the Bioconductor repositories. If you
#'   already configured them via `options(repos)`, then you can
#'   set this to `FALSE`. Defaults to the [`pkg.use_bioconductor`][pak-config]
#'   option.
#' @param cran_mirror The CRAN mirror to use. Defaults to the
#'   [`pkg.cran_mirror`][pak-config] option.
#' @return A data frame that has a row for every repository, on every
#' queried platform and R version. It has these columns:
#' * `name`: the name of the repository. This comes from the names
#'   of the configured repositories in `options("repos")`, or
#'   added by pak. It is typically `CRAN` for CRAN, and the
#'   current Bioconductor repositories are `BioCsoft`, `BioCann`,
#'   `BioCexp`, `BioCworkflows`.
#' * `url`: base URL of the repository.
#' * `bioc_version`: Bioconductor version, or `NA` for
#'   non-Bioconductor repositories.
#' * `username`: Included if at least one repository is authenticated.
#'   `NA_character_` for repositories without authentication. See
#'   [repo_auth()].
#' * `has_password`: `TRUE` is the function could retrieve the password
#'   for the authenticated repository. It is `NA` for repositories without
#'   authentication. This column is included only if at least one
#'   repository has authentication. See [repo_auth()].
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

repo_status <- function(
  platforms = NULL,
  r_version = getRversion(),
  bioc = NULL,
  cran_mirror = NULL
) {
  load_extra("pillar")
  remote(
    function(...) asNamespace("pak")$repo_status_internal(...),
    list(platforms, r_version, bioc, cran_mirror)
  )
}

repo_status_internal <- function(
  platforms = NULL,
  r_version = getRversion(),
  bioc = NULL,
  cran_mirror = NULL
) {
  config <- pkgdepends::current_config()
  platforms <- platforms %||% config$get("platforms")
  cran_mirror <- cran_mirror %||% config$get("cran_mirror")
  bioc <- bioc %||% config$get("use_bioconductor")

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

repo_ping <- function(
  platforms = NULL,
  r_version = getRversion(),
  bioc = NULL,
  cran_mirror = NULL
) {
  ret <- remote(
    function(...) asNamespace("pak")$repo_ping_internal(...),
    list(platforms, r_version, bioc, cran_mirror)
  )

  cat(ret$fmt, sep = "\n")
  invisible(ret$data)
}

repo_ping_internal <- function(
  platforms = NULL,
  r_version = getRversion(),
  bioc = NULL,
  cran_mirror = NULL
) {
  config <- pkgdepends::current_config()
  platforms <- platforms %||% config$get("platforms")
  cran_mirror <- cran_mirror %||% config$get("cran_mirror")
  bioc <- bioc %||% config$get("use_bioconductor")

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
#' * `username`: user name, for authenticated repositories.
#' * `has_password`: whether `repo_get()` could find the password for
#'   this repository. Call [repo_auth()] for more information if the
#'   credential lookup failed.
#'
#' @family repository functions
#' @export
#' @section Examples:
#' ```{asciicast repo-get}
#' repo_get()
#' ```

repo_get <- function(
  r_version = getRversion(),
  bioc = NULL,
  cran_mirror = NULL
) {
  load_extra("pillar")
  remote(
    function(...) asNamespace("pak")$repo_get_internal(...),
    list(r_version, bioc, cran_mirror)
  )
}

repo_get_internal <- function(
  r_version = getRversion(),
  bioc = NULL,
  cran_mirror = NULL
) {
  config <- pkgdepends::current_config()
  cran_mirror <- cran_mirror %||% config$get("cran_mirror")
  bioc <- bioc %||% config$get("use_bioconductor")
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
#' @param username User name to set, for authenticated repositories, see
#'   [repo_auth()].
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
#'   `https://techcommunity.microsoft.com/blog/azuresqlblog/microsoft-r-application-network-retirement/3707161`
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
#' @family repository functions
#' @export
#' @section Examples:
#' ```{asciicast repo-add}
#' repo_add(PPMdplyr100 = "PPM@dplyr-1.0.0")
#' repo_get()
#' ```
#'
#' ```{asciicast repo-add-3}
#' repo_resolve("PPM@2020-01-21")
#' ```
#' ```{asciicast repo-add-5}
#' repo_resolve("PPM@dplyr-1.0.0")
#' ```
#' ```{asciicast repo-add-7}
#' repo_resolve("PPM@R-4.0.0")
#' ```

repo_add <- function(..., .list = NULL, username = NULL) {
  new <- c(list(...), .list)
  ret <- suppressMessages(remote(
    function(...) asNamespace("pak")$repo_add_internal(...),
    list(.list = new, username = username)
  ))
  options(repos = ret$option)
  invisible(ret$tab)
}

repo_add_internal <- function(.list, username) {
  tab <- pkgcache::repo_add(.list = .list, username = username)
  list(option = getOption("repos"), tab = tab)
}

#' @param spec Repository specification, a possibly named character
#' scalar.
#' @param username User name to set, for authenticated repositories, see
#'   [repo_auth()].
#' @return `repo_resolve()` returns a named character scalar, the URL
#' of the repository.
#' @rdname repo_add
#' @export

repo_resolve <- function(spec, username = NULL) {
  remote(
    function(spec, username) pkgcache::repo_resolve(spec, username),
    list(spec, username)
  )
}

#' Authenticated repositories
#'
#' pak supports HTTP basic authentication when interacting with
#' CRAN-like repositories. To use authentication, include a username
#' in the repo URL:
#' ```
#' https://<username>@<repo-host>/<repo-path>
#' ```
#'
#' pak will look up the password for this url and username from the
#' the user's `.netrc` file and from the system credential store using
#' the keyring package.
#'
#' ## `.netrc`` files
#'
#' First pak searches in the `.netrc` file. If the `NETRC` environment
#' variable is set, pak uses its value to determine the location of the
#' `netrc` file.
#'
#' Otherwise pak looks for the `netrc` file in current user's home
#' directory, at `~/.netrc`. On Windows it also looks for `~/_netrc` if the
#' file starting with a dot does not exist.
#'
#' If you create a `netrc` file, make sure that is only readable by you.
#' E.g. on Unix run
#' ```sh
#' chmod 600 ~/.netrc
#' ```
#'
#' `netrc` files are simple text files that can store passwords for multiple
#' hosts. They may contain three types of tokens:
#'
#' ### `machine <hostname>`
#'
#' A host name, without the protocol. Subsequent `login` and `password`
#' tokens belong to this host, until another `machine` token is found, or
#' the end of file.
#'
#' ### `login <username>`
#'
#' User name. It must be preceded by a `machine` token.
#'
#' ### `password <password>`
#'
#' Password. It must be preceded by a `machine` and a `login` token.
#'
#' Whitespace is ignored in `netrc` files. You may include multiple tokens
#' on the same line, or have one token per line. Here is an example:
#'
#' ```
#' machine myhost.mydomain.com login myuser password secret
#' machine myhost2.mydomain.com
#' login myuser
#' password secret
#' login anotheruser
#' password stillsecret
#' ```
#'
#' If you need to include whitespace in a password, put the password in double
#' quotes.
#'
#'
#' ## The system credential store
#'
#' pak currently supports the following keyring
#' backends:
#'
#' * Windows credential store,
#' * macOS Keychain,
#' * Linux Secret Service via libsecret, if built with libsecret support,
#' * environment variables.
#'
#' For the URL above it tries the following keyring
#' keys, in this order:
#' ```
#' https://<username>@repo-host/<repo-path>
#' https://repo-host/<repo-path>
#' https://<username>@repo-host
#' https://repo-host
#' ```
#'
#' To add an authenticated repository use [repo_add()] with the `username`
#' argument. Alternatively, you can set the `repos` option directly using
#' [base::options()] and including the username in the repository URL.
#'
#' `repo_auth()` lists authentication information for all configured
#' repositories.
#'
#' @inheritParams repo_get
#' @param check_credentials Whether to check that credentials are
#'   available for authenticated repositories.
#' @return Data frame with columns:
#'   - all columns from the output of [repo_get()],
#'   - `auth_domains`: authentication domains. pak tries to find the
#'     credentials for these domains, until the search is successful or all
#'     domains fail.
#'   - `auth_domain`: if the credential lookup is successful, then this is
#'     the authentication domain that was used to get the credentials.
#'   - `auth_source`: where the credentials were found. E.g.
#'     `keyring:<backend>` means it was in the default macos keyring.
#'   - `auth_error`: for failed credential searches this is the description
#'     of why the search failed. E.g. maybe the keyring package is not
#'     installed, or pak found no credentials for any of the
#'     authentication domains.
#'
#' @seealso [Authenticated repositories].
#' @family authenticated repositories
#' @export

repo_auth <- function(
  r_version = getRversion(),
  bioc = NULL,
  cran_mirror = NULL,
  check_credentials = TRUE
) {
  load_extra("pillar")
  remote(
    function(...) asNamespace("pak")$repo_auth_internal(...),
    list(r_version, bioc, cran_mirror, check_credentials)
  )
}

repo_auth_internal <- function(
  r_version = getRversion(),
  bioc = NULL,
  cran_mirror = NULL,
  check_credentials = TRUE
) {
  config <- pkgdepends::current_config()
  cran_mirror <- cran_mirror %||% config$get("cran_mirror")
  bioc <- bioc %||% config$get("use_bioconductor")
  pkgcache::repo_auth(r_version, bioc, cran_mirror, check_credentials)
}
