#' Authenticated repositories
#'
#' @name Authenticated repositories
#' @rdname repo-auth
#' @description
#' pak supports HTTP basic authentication when interacting with CRAN-like
#' repositories.
#'
#' ```{r child = "man/chunks/auth.Rmd"}
#' ```
NULL

#' Query or set repository password in the system credential store
#'
#' Use pak's internal copy of the keyring package to query or set a
#' repository password in the system credential store.
#'
#' `repo_auth_key_get()` retrieves a password from the default keyring. It
#' errors if it cannot find the credentials for `url`.
#'
#' `repo_auth_key_set()` adds or updates a password in the system
#' crednetial store.
#'
#' @param url Repository URL. It may contain a username, in which case
#'   `username` may be `NULL`.
#' @param username User name, if it is not included in `url`.
#' @param password Password to set.
#' @return `repo_auth_key_get()` returns a single string, the repository
#'   password.
#'
#' @export

repo_auth_key_get <- function(url, username = NULL) {
  remote(
    function(...) asNamespace("pak")$repo_auth_key_get_internal(...),
    list(url, username)
  )
}

repo_auth_key_get_internal <- function(url, username = NULL) {
  if (is.null(username)) {
    parsed_url <- parse_url(url)
    username <- parsed_url$username
    if (length(username) == 0 || nchar(username) == 0) {
      stop("Cannot get repo key for URL ", url, ", username is missing")
    }
  }
  keyring::key_get(url, username)
}

#' @export
#' @rdname repo_auth_key_get

repo_auth_key_set <- function(url, password, username = NULL) {
  remote(
    function(...) asNamespace("pak")$repo_auth_key_set_internal(...),
    list(url, password, username)
  )
  invisible(NULL)
}

repo_auth_key_set_internal <- function(url, password, username = NULL) {
  if (is.null(username)) {
    parsed_url <- parse_url(url)
    username <- parsed_url$username
    if (length(username) == 0 || nchar(username) == 0) {
      stop("Cannot set repo key for URL ", url, ", username is missing")
    }
  }
  keyring::key_set_with_value(url, username, password)
  invisible(NULL)
}

#' CRAN proxy with authentication, for testing
#'
#' It needs the webfakes package.
#'
#' @param repo_url URL of the original CRAN repository.
#' @param username User name.
#' @param password Password.
#' @return A webfakes app.
#'
#' @noRd
#' @examples
#' # Run the proxy in the current R session, use `NULL` for a random port.
#' # Use http://username@127.0.0.1:3000 as the repo URL
#' auth_proxy_app()$listen(3000)
#'
#' # Run the proxy in a subprocess, on a random port, query its URL,
#' # and use it. This also needs the callr package.
#' repo <- webfakes::new_app_process(auth_proxy_app())
#' repo$url()
#'
#' # opt out from Bioconductor packages
#' options(pkg.use_bioconductor = FALSE)
#'
#' # Add credetnails to the default keyring, password is "token"
#' repo_auth_key_set(repo$url(), username = "username", "token")
#'
#' # Set repos
#' repo_add(CRAN = repo$url(), username = "username")
#' repo_get()
#'
#' # repo status
#' repo_status()
#' repo_ping()
#'
#' # repo status with auth info
#' repo_auth()
#'
#' # get list pf packages
#' meta_update()
#' meta_list()

auth_proxy_app <- function(repo_url = NULL, username = "username",
                           password = "token") {
  repo_url <- repo_url %||% "https://cloud.r-project.org"
  webfakes::new_app()$get(
    webfakes::new_regexp(""), function(req, res) {
      # base64_encode will be exported in the next version of webfakes
      exp <- paste("Basic", asNamespace("webfakes")$base64_encode(paste0(username, ":", password)))
      hdr <- req$get_header("Authorization") %||% ""
      if (exp != hdr) {
        res$
          set_header("WWW-Authenticate", "Basic realm=\"CRAN with auth\"")$
          send_status(401L)
      } else {
        res$
          redirect(sprintf("%s/%s", repo_url, req$path))
      }
    }
  )
}
