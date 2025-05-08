#' Authenticated repositories
#'
#' `repo_auth()` lists authentication information for all configured
#' repositories.
#'
#' pkgcache supports HTTP basic authentication when interacting with
#' CRAN-like repositories. To use authentication, include a username
#' in the repo URL:
#' ```
#' https://<username>@<repo-host>/<repo-path>
#' ```
#'
#' pkgcache tries to obtain the passwords for the authenticated CRAN-like
#' repos from two sources, in this order:
#'
#' 1. A "netrc" file. If the `NETRC` environment variable is set, it
#'    should point to a file in "netrc" format. Otherwise pkgcache uses
#'    the `~/.netrc` file in the current user's home directory, if it
#'    exists. On Windows it also tries the `~/_netrc` file.
#' 2. The system's credential store, via the keyring package.
#'
#' See the [documentation of libcurl](
#'   https://curl.se/libcurl/c/CURLOPT_NETRC.html) for details about the
#' format of the netrc file.
#'
#' When looking for the password in the system credential store, pkgcache
#' looks at the following keys, in this order:
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
#' @inheritParams repo_get
#' @param check_credentials Whether to check that credentials are
#'   available for the authenticated repositories.
#' @return Data frame with columns:
#'   - all columns from the output of [repo_get()],
#'   - `auth_domains`: authentication domains. pkgcache tries to find the
#'     credentials for these domains, until the search is successful or all
#'     domains fail. This column is a list column of character vectors.
#'   - `auth_domain`: if the credential lookup is successful, then this is
#'     the authentication domain that was used to get the credentials.
#'   - `auth_source`: where the credentials were found. E.g.
#'     `keyring:macos` means it was in the default macos keyring.
#'   - `auth_error`: for failed credential searches this is the description
#'     of why the search failed. E.g. maybe the keyring package is not
#'     installed, or pkgcache found no credentials for any of the
#'     authentication domains.
#'
#' @export

repo_auth <- function(
  r_version = getRversion(),
  bioc = TRUE,
  cran_mirror = default_cran_mirror(),
  check_credentials = TRUE
) {
  res <- cmc__get_repos(
    getOption("repos"),
    bioc = bioc,
    cran_mirror = cran_mirror,
    as.character(r_version),
    auth = FALSE
  )

  key <- random_key()
  on.exit(clear_auth_cache(key), add = TRUE)
  start_auth_cache(key)

  res$username <- rep(NA_character_, nrow(res))
  res$has_password <- rep(NA, nrow(res))
  res$auth_domains <- I(replicate(nrow(res), NULL))
  res$auth_domain <- rep(NA_character_, nrow(res))
  res$auth_source <- rep(NA_character_, nrow(res))
  res$auth_error <- rep(NA_character_, nrow(res))
  for (w in seq_len(nrow(res))) {
    url <- res$url[w]
    if (check_credentials) {
      cred <- repo_auth_headers(url, warn = FALSE)
      if (is.null(cred)) next
      res$username[w] <- cred$username
      res$has_password[w] <- cred$found
      res$auth_domains[w] <- list(cred$auth_domains)
      if (cred$found) {
        res$auth_source[w] <- cred$source
        res$auth_domain[w] <- cred$auth_domain
      } else {
        res$auth_error[w] <- cred$error
      }
    } else {
      parsed_url <- parse_url_basic_auth(url)
      if (
        length(parsed_url$username) == 0 ||
          nchar(parsed_url$username) == 0
      ) {
        next
      }
      res$username[w] <- parsed_url$username
      res$auth_domains[w] <- list(unique(unlist(
        parsed_url[c("repouserurl", "repourl", "hostuserurl", "hosturl")]
      )))
    }
  }

  res
}

#' Retrieve credentials for CRAN-like repos
#'
#' Returns a set of HTTP headers for the given URL if (1) it belongs to a
#' package repository; and (2) has credentials stored in the keyring.
#'
#' @param url Repo URL or download URL. For authentication it should include
#'   a username.
#' @param use_cache Whether to allow retrieving the credenticals from the
#'   credential cache.
#' @param set_cache Whether to save the retrieved credentials in the
#'   credential cache.
#' @param warn Whether to warn if the function cannot find credentials.
#' @return
#'   - `NULL` if the `url`` does not have authentication, e.g. if it does
#'     not include a (non-empty) username. It is also `NULL` if the `url`
#'     already has a password.
#'   - If `url` has authentication (but no password), then a list
#'     with entries:
#'     * `found`: `TRUE` if the function found the credentials, `FALSE`
#'       otherwise.
#'     * `headers`: character vector, the headers to add to the HTTP
#'       request.
#'     * `auth_domains`: all possible authentication domains.
#'     * `auth_domain`: the domain that was used to retrieve the
#'       credentials. This can be full path to the repository, with or
#'       without the username, or the hostname URL, with or without the
#'       username.
#'     * `username`: user name, from the URL.
#'     * `source`: if the function found the credentials, then it is a
#'       short description about where the credencials were found.
#'     * `error`: if the function did not find the credentials, then it is
#'       a short description about why their retrieval failed.
#' @noRd

repo_auth_headers <- function(
  url,
  use_cache = TRUE,
  set_cache = TRUE,
  warn = TRUE
) {
  # shortcut to speed up the common case of no credentials
  if (!grepl("@", url)) {
    return(NULL)
  }

  parsed_url <- parse_url_basic_auth(url)
  if (length(parsed_url$password) > 0 && nchar(parsed_url$password) != 0) {
    # The URL already contains a password. This is pretty poor practice, maybe
    # we should issue a warning pointing users to the keyring package instead.
    return(NULL)
  }
  if (length(parsed_url$username) == 0 || nchar(parsed_url$username) == 0) {
    # No username to key the lookup in the keyring with.
    return(NULL)
  }

  # Try URLs in this order:
  # - repo URL with username
  # - repo URL w/o username
  # - host URL with username
  # - host URL w/o username
  # We try each with and without a keyring username
  urls <- unique(unlist(
    parsed_url[c("repouserurl", "repourl", "hostuserurl", "hosturl")]
  ))

  if (use_cache) {
    for (u in urls) {
      if (u %in% names(pkgenv$credentials)) {
        creds <- pkgenv$credentials[[u]]
        creds$source <- paste0(creds$source, ":cached")
        return(creds)
      }
    }
  }

  res <- list(
    found = FALSE,
    headers = character(),
    auth_domains = urls,
    auth_domain = NA_character_,
    username = parsed_url$username,
    source = NULL,
    error = NULL
  )

  pwd <- repo_auth_netrc(parsed_url$host, parsed_url$username)
  if (!is.null(pwd)) {
    res$auth_domain <- parsed_url$host
    res$source <- paste0(".netrc")
  }

  if (is.null(pwd) && !requireNamespace("keyring", quietly = TRUE)) {
    res$found <- FALSE
    res$error <- "keyring not installed"
    if (warn) {
      cli::cli_alert_warning(
        "Cannot find credentials for URL {.url {url}}, the keyring package
         is not installed."
      )
    }
    return(res)
  }

  if (is.null(pwd)) {
    kb <- keyring::default_backend()
    for (u in urls) {
      pwd <- try_catch_null(kb$get(u, parsed_url$username)) %||%
        try_catch_null(kb$get(u))
      if (!is.null(pwd)) {
        res$auth_domain <- u
        res$source <- paste0("keyring:", kb$name)
        break
      }
    }
  }

  if (!is.null(pwd)) {
    res$found <- TRUE
    auth <- paste(parsed_url$username, pwd, sep = ":")
    res$headers <- c("Authorization" = paste("Basic", base64_encode(auth)))
  } else {
    if (warn) {
      cli::cli_alert_warning(
        "Cannot find credentials for URL {.url {url}}, credential lookup
         failed. Keyring backend: {.val {kb$name}}."
      )
    }
    res$error <- paste0("keyring lookup failed (", kb$name, " backend)")
  }

  if (set_cache) {
    # we also cache negative results, to avoid many lookups and warnings
    key <- if (res$found) res$auth_domain else urls[1]
    pkgenv$credentials[[key]] <- res
    if (res$found) {
      cli::cli_alert_success(
        wrap = TRUE,
        "Found credentials for repo {.url {parsed_url$repouserurl}}
         ({res$source})."
      )
    } else {
      cli::cli_alert_danger(
        wrap = TRUE,
        "Did not find credentials for repo {.url {parsed_url$repouserurl}},
         {res$error}."
      )
    }
  }

  res
}

clear_auth_cache <- function(key = NULL) {
  if (
    is.null(key) ||
      identical(pkgenv$credentials[[".exit_handler"]], key)
  ) {
    rm(
      list = ls(pkgenv$credentials, all.names = TRUE),
      envir = pkgenv$credentials
    )
  }
}

start_auth_cache <- function(key) {
  if (!".exit_handler" %in% names(pkgenv$credentials)) {
    assign(".exit_handler", key, envir = pkgenv$credentials)
  }
}

base64_encode <- function(x) {
  if (!is.raw(x)) {
    x <- charToRaw(x)
  }
  processx::base64_encode(x)
}

parse_url_basic_auth <- function(url) {
  psd <- parse_url(url)
  if (is.na(psd$host)) {
    return(NULL)
  }
  userat <- if (nchar(psd$username)) paste0(psd$username, "@") else ""
  repo <- c(
    paste0(psd$protocol, "://", psd$host, psd$path),
    paste0(psd$protocol, "://", userat, psd$host, psd$path)
  )
  repo <- sub("(/(src|bin)/)(.*)$", "", repo)
  # Lop off any /__linux__/ subdirectories, too.
  repo <- sub("^(.*)/__linux__/[^/]+(/.*)$", "\\1\\2", repo, perl = TRUE)
  list(
    host = psd$host,
    hosturl = paste0(psd$protocol, "://", psd$host),
    hostuserurl = paste0(psd$protocol, "://", userat, psd$host),
    repourl = repo[1],
    repouserurl = repo[2],
    username = psd$username,
    password = psd$password
  )
}

add_auth_status <- function(repos) {
  maybe_has_auth <- grepl("^https?://[^/]*@", repos$url)
  if (!any(maybe_has_auth)) return(repos)

  key <- random_key()
  on.exit(clear_auth_cache(key), add = TRUE)
  start_auth_cache(key)

  repos$username <- rep(NA_character_, nrow(repos))
  repos$has_password <- rep(NA, nrow(repos))
  for (w in which(maybe_has_auth)) {
    url <- repos$url[w]
    creds <- repo_auth_headers(url, warn = FALSE)
    if (is.null(creds)) next
    repos$username[w] <- creds$username
    repos$has_password[w] <- creds$found
  }

  repos
}

repo_auth_netrc <- function(host, username) {
  netrc_path <- Sys.getenv("PKG_NETRC_PATH", Sys.getenv("NETRC"))
  if (netrc_path == "") {
    netrc_path <- path.expand("~/.netrc")
    if (!file.exists(netrc_path) && .Platform[["OS.type"]] == "windows") {
      netrc_path <- path.expand("~/_netrc")
    }
  }
  if (!file.exists(netrc_path)) return(NULL)

  # netrc files do not allow port numbers
  host <- sub(":[0-9]+$", "", host)

  lines <- readLines(netrc_path, warn = FALSE)
  # mark potential end of macros with NA
  lines[lines == ""] <- NA_character_
  tokens <- scan(text = lines, what = "", quiet = TRUE)

  idx <- 1L
  err <- FALSE
  machine <- login <- NA_character_
  skip_na <- function() {
    while (idx <= length(tokens) && is.na(tokens[[idx]])) {
      idx <<- idx + 1L
    }
  }

  skip_na()
  while (!err && idx <= length(tokens)) {
    switch(
      tokens[idx],
      "machine" = {
        # next token is a host name
        idx <- idx + 1L
        skip_na()
        if (idx <= length(tokens)) {
          machine <- tokens[idx]
          idx <- idx + 1L
          skip_na()
        } else {
          cli::cli_alert_warning(
            "Invalid netrc file at {.path {netrc_path}}: ended while
             parsing a {.code machine} token."
          )
        }
      },
      "default" = {
        machine <- host
        idx <- idx + 1L
        skip_na()
      },
      "login" = {
        # next token is username
        idx <- idx + 1L
        skip_na()
        if (idx <= length(tokens)) {
          if (is.na(machine)) {
            cli::cli_alert_warning(
              "Invalid netrc file at {.path {netrc_path}}: found a
               {.code login} token without a {.code machine} token."
            )
            break
          }
          login <- tokens[idx]
          idx <- idx + 1L
          skip_na()
        } else {
          cli::cli_alert_warning(
            "Invalid netrc file at {.path {netrc_path}}: ended while
             parsing {.code login} token."
          )
        }
      },
      "password" = {
        # next item is password
        idx <- idx + 1L
        skip_na()
        if (idx <= length(tokens)) {
          if (is.na(machine) || is.na(login)) {
            cli::cli_alert_warning(
              "Invalid netrc file at {.path {netrc_path}}: {.code password}
               token must come after {.code machine} (or {.code default})
               and {.code login}."
            )
            break
          }
          if (machine == host && login == username) {
            # bingo
            return(tokens[idx])
          }
          idx <- idx + 1L
          skip_na()
        } else {
          cli::cli_alert_warning(
            "Invalid netrc file at {.path {netrc_path}}: ended while
             parsing {.code password} token."
          )
        }
      },
      "macdef" = {
        # macro, we don't use this, skip until it is over
        idx <- idx + 1L
        while (idx <= length(tokens) && !is.na(tokens[idx])) {
          idx <- idx + 1L
        }
        skip_na()
      },
      {
        cli::cli_alert_warning(
          "Invalid netrc file at {.path {netrc_path}}: unknown token:
          {.code {tokens[idx]}}."
        )
        break
      }
    )
  }

  NULL
}
