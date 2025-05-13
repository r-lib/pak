#' Authenticated repositories
#'
#' @name Authenticated repositories
#' @rdname repo-auth
#' @family authenticated repositories
#' @description
#' pak supports HTTP basic authentication when interacting with CRAN-like
#' repositories.
#'
#' ```{r child = "man/chunks/auth.Rmd"}
#' ```
NULL

#' Query or set repository password in the system credential store
#'
#' Use pak's keyring functions to query or set a repository password in
#' the system credential store.
#'
#' `repo_auth_key_get()` retrieves a password from the default keyring. It
#' errors if it cannot find the credentials for `url`.
#'
#' `repo_auth_key_set()` adds or updates a password in the system
#' credential store.
#'
#' `repo_auth_unlock()` unlocks the default keyring, if it is locked.
#' You might need this if the keyring is locked. If you are using
#' ecrypted files to store the keys, then you typically need to call this
#' function in each session. You typically don't need to do that if you
#' are using the native Windows, macOS or Linux (Secret Service) backends.
#'
#' @param url Repository URL. It may contain a username, in which case
#'   `username` may be `NULL`.
#' @param username User name, if it is not included in `url`.
#' @param password Password (key) to set.
#' @param keyring_password Password to unlock the keyring.
#' @return `repo_auth_key_get()` returns a single string, the repository
#'   password.
#'
#' @family authenticated repositories
#' @export

repo_auth_key_get <- function(url, username = NULL) {
  remote(
    function(...) asNamespace("pak")$repo_auth_key_get_internal(...),
    list(url, username)
  )
}

backend_is_available <- function(backend, ...) {
  backend$is_available(...)
}

check_keyring_backend <- function(backend) {
  if (backend$name == "file") {
    dk <- backend$keyring_default()
    kl <- backend$keyring_list()
    if (!dk %in% kl$keyring) {
      # default keyring does not exist
      stop(
        "The default keyring (", dk, ") does not exist, ",
        "call `repo_auth_unlock()` to create it."
      )
    }
    if (kl$locked[match(dk, kl$keyring)]) {
      # default keyring is locked
      stop(
        "The default keyring (", dk, ") is locked. ",
        "Call `repo_auth_unlock() to unlock it."
      )
    }
  }

  if (backend$name == "secret service") {
    if (Sys.info()[["sysname"]] != "Linux") {
      stop("The 'secret service' keyring backend only works on Linux.")
    }
    ret <- tryCatch(
      backend_is_available(backend, report_error = TRUE),
      error = function(e) conditionMessage(e)
    )
    if (!isTRUE(ret)) {
      if (ret == "keyring build has no libsecret support") {
        stop(
          "This pak build does not support the secret service keyring backend."
        )
      } else {
        stop(ret)
      }
    }
  }
}

repo_auth_key_get_internal <- function(url, username = NULL) {
  if (is.null(username)) {
    parsed_url <- parse_url(url)
    username <- parsed_url$username
    if (length(username) == 0 || nchar(username) == 0) {
      stop("Cannot get repo key for URL ", url, ", username is missing")
    }
  }
  kb <- keyring::default_backend()
  cli::cli_alert_info("Using keyring backend: {kb$name}.")
  check_keyring_backend(kb)
  kb$get(url, username)
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
  kb <- keyring::default_backend()
  cli::cli_alert_info("Using keyring backend: {kb$name}.")
  check_keyring_backend(kb)
  kb$set_with_value(url, username, password)
  cli::cli_alert_success("Set keyring password for {.url {url}}.")
  invisible(NULL)
}

#' @export
#' @rdname repo_auth_key_get

repo_auth_unlock <- function(keyring_password) {
  remote(
    function(...) asNamespace("pak")$repo_auth_unlock_internal(...),
    list(keyring_password)
  )
  invisible(NULL)
}

repo_auth_unlock_internal <- function(password) {
  kb <- keyring::default_backend()
  cli::cli_alert_info("Using keyring backend: {kb$name}.")

  if (kb$name == "file") {
    dk <- kb$keyring_default()
    kl <- kb$keyring_list()
    if (!dk %in% kl$keyring) {
      # default keyring does not exist
      cli::cli_alert_info("Creating default keyring.")
      kb$keyring_create(password = password)
      cli::cli_alert_success("Created default keyring: {dk}.")
    }
    if (kb$keyring_is_locked()) {
      # default keyring is locked
      kb$keyring_unlock(password = password)
    }
  } else if (kb$name != "env" && kb$keyring_is_locked()) {
    cli::cli_alert_info("Unlocking keyring with password.")
    kb$keyring_unlock(password = password)
    if (kb$keyring_is_locked()) {
      cli::cli_alert_danger("Failed to unlock keyring with password")
    } else {
      cli::cli_alert_success("Unlocked keyring with password.")
    }
  } else {
    cli::cli_alert_success("Keyring is already unlocked.")
  }
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
