#' Posit Package Manager single sign-on (SSO) authentication
#'
#' @details
#' ## Set up SSO authentication:
#' - Set the `PACKAGEMANAGER_ADDRESS` environment variable to the URL of
#'   your RStudio Package Manager instance. For example, add this line to
#'   your `.Renviron` file:
#'   ```
#'   PACKAGEMANAGER_ADDRESS=https://<ppm-url>
#'   ```
#'   Alternatively, you can also set it in your shell profile on Unix,
#'   or in the System or User environment variables on Windows.
#' - Set `options(repos)` to include a repository from your Package Manager
#'   instance. Include `__token__` as the username in the URL. For example:
#'   ```
#'   options(repos = c(
#'     PPM = "https://__token__@<ppm-url>/<repo-path>",
#'     getOption("repos")
#'   ))
#'   ```
#'   You probably want to add this to your `.Rprofile` file, so that it is
#'   set in every R session.
#' - Call [repo_get()] to trigger authentication and caching of the token.
#'   You should be prompted to log in via your browser, and the obtained
#'   token will be cached for future use. Call `ppm_sso_status()` to check
#'   the status of your authentication, including the path of the cached
#'   token and its expiration time.
#' - Alternatively, you can call `ppm_sso_login()` directly to trigger
#'   the login process directly.
#'
#' `ppm_sso_login()` initiates the SSO login process. You should be
#' prompted to log in via your browser, and the obtained token will be
#' cached for future use.
#'
#' @return `ppm_sso_login()` returns the obtained token invisibly.
#'
#' @seealso [Authenticated repositories],
#'   <https://docs.posit.co/rspm/admin/authentication/>
#' @export
#' @examplesIf FALSE
#' Sys.setenv(PACKAGEMANAGER_ADDRESS = "https://<ppm-url>")
#' options(repos = c(
#'   PPM = "https://__token__@<ppm-url>/<repo-path>",
#'   getOption("repos")
#' ))
#' ppm_sso_login()
#' ppm_sso_status()
#' ppm_sso_status(connect = TRUE)
#' ppm_sso_logout()

ppm_sso_login <- function() {
  res <- remote(
    function() {
      asNamespace("pkgcache")$ppm_sso_login()
    },
    list()
  )
  invisible(res)
}

#' @rdname ppm_sso_login
#' @details
#' `ppm_sso_logout()` removes the cached token, effectively logging you
#' out. If there is no cached token, it does nothing.
#' @return `ppm_sso_logout()` does not return anything.
#' @export

ppm_sso_logout <- function() {
  res <- remote(
    function() {
      asNamespace("pkgcache")$ppm_sso_logout()
    },
    list()
  )
  invisible(res)
}

#' @rdname ppm_sso_login
#' @param connect If `TRUE`, also checks if the token is valid by making a test
#'   request to the Package Manager instance. This requires an active internet
#'   connection and may take a few seconds. If `FALSE`, only checks if a
#'   token is cached and not expired.
#' @details
#' `ppm_sso_status()` checks the status of your authentication, including
#' the path of the cached token and its expiration time.
#' @return `ppm_sso_status()` returns a list with the following components:
#' - `ppm_url`: The URL of the Package Manager instance.
#' - `token_file`: The path of the cached token file.
#' - `token`: The cached token (partially masked for display) or `NA` if
#'   no token is found locally.
#' - `valid`: `TRUE` if the token is valid (only if `connect = TRUE`),
#'   `FALSE` if invalid, or `NA` if not checked.
#' - `issuer`: The issuer of the token, or `NA` if not available.
#' - `subject`: The subject of the token, or `NA` if not available.
#' - `audience`: The audience of the token, or `NA` if not available.
#' - `issued_at`: The issue time of the token as a POSIXct object, or `NA`
#'   if not available.
#' - `expires_at`: The expiration time of the token as a POSIXct object,
#'   or `NA`  if not available.
#' - `expired`: `TRUE` if the token is expired, `FALSE` if not expired,
#'   or `NA` if expiration time is not available.
#' - `expires_in`: The time until expiration as a difftime object, or
#'   `NA` if expiration time is not available or the token is already
#'   expired.
#' @export
ppm_sso_status <- function(connect = FALSE) {
  remote(
    function(connect) {
      ret <- asNamespace("pkgcache")$ppm_sso_status(connect)
      asNamespace("pak")$pak_preformat(ret)
    },
    list(connect)
  )
}
