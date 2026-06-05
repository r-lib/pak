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
#'   token will be cached for future use. Call [ppm_sso_status()] to check
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
#' @seealso <https://docs.posit.co/rspm/admin/authentication/>
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
  ppm_url <- Sys.getenv("PACKAGEMANAGER_ADDRESS", NA_character_)

  identity_token <- ppm_sso_get_identity_token_from_file() %||%
    ppm_sso_device_flow(ppm_url)
  ppm_token <- ppm_sso_identity_to_ppm_token(ppm_url, identity_token)
  ppm_sso_write_token_to_file(ppm_url, ppm_token)

  invisible(ppm_token)
}

#' @rdname ppm_sso_login
#' @details
#' `ppm_sso_logout()` removes the cached token, effectively logging you
#' out. If there is no cached token, it does nothing.
#' @return `ppm_sso_logout()` does not return anything.
#' @export

ppm_sso_logout <- function() {
  ppm_url <- Sys.getenv("PACKAGEMANAGER_ADDRESS", NA_character_)

  # remove from cache if there
  try_catch_null(suppressWarnings(rm(
    list = ppm_url,
    envir = pkgenv$ppm_sso_cache,
    inherits = FALSE
  )))
  parsed <- parse_url(ppm_url)
  try_catch_null(suppressWarnings(rm(
    list = parsed$host,
    envir = pkgenv$credentials,
    inherits = FALSE
  )))

  token_file_path <- ppm_sso_token_path()
  if (!file.exists(token_file_path)) {
    return(invisible())
  }
  tokens <- try_catch_null({
    tokens <- suppressWarnings(tstoml::ts_read_toml(token_file_path))
    urls <- tsitter::ts_tree_unserialize(
      tsitter::ts_tree_select(tokens, list("connections", TRUE, "address"))
    )
    idx <- which(urls == ppm_url)[1]
    tokens
  })

  if (is.na(idx)) {
    return(invisible())
  }

  tokens <- tsitter::ts_tree_delete(
    tsitter::ts_tree_select(tokens, list("connections", idx))
  )

  tsitter::ts_tree_write(tokens, token_file_path)

  invisible()
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
  ppm_url <- Sys.getenv("PACKAGEMANAGER_ADDRESS", NA_character_)
  ppm_sso_check_url(ppm_url)
  token <- ppm_sso_get_cached_token(ppm_url, alive = TRUE) %||%
    ppm_sso_get_existing_token(ppm_url, valid = FALSE)

  jwt <- token %&&% jwt_split(token)
  iat <- .POSIXct(jwt$payload$iat %||% NA_real_)
  exp <- .POSIXct(jwt$payload$exp %||% NA_real_)
  now <- Sys.time()
  auth <- if (connect) {
    token %&&%
      try_catch_null(ppm_sso_can_authenticate(ppm_url, token)) %||%
      FALSE
  } else {
    NA
  }

  structure(
    list(
      ppm_url = ppm_url,
      token_file = ppm_sso_token_path(),
      token = token %||% NA_character_,
      valid = auth,
      issuer = jwt$payload$iss %||% NA_character_,
      subject = jwt$payload$sub %||% NA_character_,
      audience = jwt$payload$aud %||% NA_character_,
      issued_at = iat,
      expires_at = exp,
      expired = exp < now,
      expires_in = if (!is.na(exp) && now < exp) {
        exp - now
      } else {
        as.difftime(NA_real_, units = "secs")
      }
    ),
    class = "ppm_sso_status"
  )
}

jwt_split <- function(jwt) {
  input <- strsplit(jwt, ".", fixed = TRUE)[[1]]
  stopifnot(length(input) %in% c(2, 3))
  header <- jsonlite::fromJSON(rawToChar(ppm_sso_base64url_decode(input[1])))
  if (length(header$typ)) {
    stopifnot(toupper(header$typ) == "JWT")
  }
  if (is.na(input[3])) {
    input[3] = ""
  }
  sig <- ppm_sso_base64url_decode(input[3])
  payload <- jsonlite::fromJSON(rawToChar(ppm_sso_base64url_decode(input[2])))
  data <- charToRaw(paste(input[1:2], collapse = "."))
  if (!grepl("^none|EdDSA|[HRE]S(256|384|512)$", header$alg)) {
    stop("Invalid algorithm: ", header$alg)
  }
  if (grepl(".S\\d\\d\\d", header$alg)) {
    type <- match.arg(substring(header$alg, 1, 1), c("HMAC", "RSA", "ECDSA"))
    keysize <- as.numeric(substring(header$alg, 3))
  } else {
    type <- header$alg
    keysize = NULL
  }
  list(
    type = type,
    keysize = keysize,
    data = data,
    sig = sig,
    payload = payload,
    header = header
  )
}

#' @export

print.ppm_sso_status <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

format.ppm_sso_status <- function(x, ...) {
  token <- if (!is.na(x$token)) {
    paste0(
      substr(x$token, 1, 3),
      "...",
      substr(x$token, nchar(x$token) - 3, nchar(x$token))
    )
  } else {
    NA_character_
  }
  key <- function(x) {
    cli::col_cyan(x)
  }
  url <- function(x) {
    if (!is.na(x) && startsWith(x, "http")) {
      cli::style_hyperlink(x, x)
    } else {
      x
    }
  }
  tick <- function(x, invert = FALSE) {
    txt <- if (isTRUE(x)) {
      "yes"
    } else if (isFALSE(x)) {
      "no"
    } else {
      "?"
    }
    if (invert) {
      x <- !x
    }
    if (isTRUE(x)) {
      cli::col_green(txt)
    } else if (isFALSE(x)) {
      cli::col_magenta(txt)
    } else {
      txt
    }
  }
  ein <- if (is.na(x$expires_in)) "-" else format_time$pretty_dt(x$expires_in)
  c(
    cli::rule("PPM SSO Status"),
    paste(key("PPM URL:    "), url(x$ppm_url)),
    paste(key("Token file: "), x$token_file),
    paste(key("Token:      "), token),
    paste(key("Valid:      "), tick(x$valid)),
    paste(key("Issuer:     "), url(x$issuer)),
    paste(key("Subject:    "), x$subject),
    paste(key("Audience:   "), x$audience),
    paste(key("Issued at:  "), x$issued_at),
    paste(key("Expires at: "), x$expires_at),
    paste(key("Expired:    "), tick(x$expired, invert = TRUE)),
    paste(key("Expires in: "), ein),
    NULL
  )
}


ppm_sso_check_url <- function(ppm_url) {
  if (is.na(ppm_url)) {
    stop(
      "Please set the PACKAGEMANAGER_ADDRESS environment variable to ",
      "the URL of your RStudio Package Manager instance."
    )
  }

  if (is.na(parse_url(ppm_url)$host)) {
    stop(
      "The PACKAGEMANAGER_ADDRESS environment variable must be a valid URL, ",
      "but got: ",
      ppm_url
    )
  }
}

ppm_sso_auth <- function(repo) {
  ppm_url <- Sys.getenv("PACKAGEMANAGER_ADDRESS", NA_character_)
  parsed <- tryCatch(
    parse_url(repo),
    error = function(e) {
      stop("Failed to parse repository URL: ", repo)
    }
  )
  repo_host <- paste0(parsed$protocol, "://", parsed$host)
  if (repo_host != ppm_url) {
    stop(
      "The repository URL (",
      repo_host,
      ") does not match the configured ",
      "Package Manager URL (",
      ppm_url,
      ")."
    )
  }

  token <- ppm_sso_get_cached_token(ppm_url, alive = TRUE) %||%
    ppm_sso_get_existing_token(ppm_url, valid = TRUE) %||%
    ppm_sso_login()

  pkgenv$ppm_sso_cache[[ppm_url]] <- token

  token
}

ppm_sso_get_cached_token <- function(ppm_url, alive = TRUE) {
  token <- pkgenv$ppm_sso_cache[[ppm_url]]

  # no token in cache
  if (is.null(token)) {
    return(NULL)
  }

  # no need to test if token is live
  if (!alive) {
    return(token)
  }

  # no expiration date
  jwt <- jwt_split(token)
  exp <- jwt$payload$exp
  if (is.null(exp)) {
    return(token)
  }

  # check if token is still valid
  if (.POSIXct(exp) > Sys.time()) {
    return(token)
  }

  # not valid any more, remove from cache
  pkgenv$ppm_sso_cache[[ppm_url]] <- NULL

  NULL
}

ppm_sso_post_form <- function(url, payload) {
  payload <- payload[!vapply(payload, is.null, logical(1))]
  body <- paste(
    paste0(
      curl::curl_escape(names(payload)),
      "=",
      curl::curl_escape(unlist(payload, use.names = FALSE))
    ),
    collapse = "&"
  )
  h <- curl::new_handle()
  curl::handle_setheaders(
    h,
    "Content-Type" = "application/x-www-form-urlencoded"
  )
  curl::handle_setopt(h, post = TRUE, postfields = body)
  resp <- curl::curl_fetch_memory(url, handle = h)
  list(
    status = resp$status_code,
    body = tryCatch(
      jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE),
      error = function(e) {
        resp$content
      }
    )
  )
}

ppm_sso_token_path <- function() {
  Sys.getenv(
    "PACKAGEMANAGER_SSO_TOKEN_FILE",
    file.path(
      path.expand("~"),
      ".ppm",
      "tokens.toml"
    )
  )
}

ppm_sso_get_existing_token <- function(ppm_url, valid = TRUE) {
  path <- ppm_sso_token_path()
  try_catch_null({
    ts_tokens <- suppressWarnings(tstoml::ts_read_toml(path))
    for (conn in ts_tokens[[list("connections", TRUE)]]) {
      if (identical(conn$address, ppm_url)) {
        if (valid && !ppm_sso_can_authenticate(ppm_url, conn$token)) {
          return(NULL)
        }
        return(conn$token)
      }
    }
  })
}

ppm_sso_get_identity_token_from_file <- function() {
  token_file <- Sys.getenv("PACKAGEMANAGER_IDENTITY_TOKEN_FILE", unset = NA)
  if (is.na(token_file)) {
    return(NULL)
  }
  try_catch_null({
    trimws(readLines(token_file, n = 1, warn = FALSE))
  })
}

ppm_sso_device_flow_init <- function(ppm_url) {
  verifier <- ppm_sso_new_pkce_verifier()
  challenge <- ppm_sso_new_pkce_challenge(verifier)

  # 1. Initiate Device Auth
  init_url <- paste0(ppm_url, "/__api__/device")
  payload <- list(
    code_challenge_method = "S256",
    code_challenge = challenge
  )
  init_resp <- ppm_sso_post_form(init_url, payload)
  if (init_resp$status >= 400) {
    stop(
      "Failed to initiate device authorization (HTTP ",
      init_resp$status,
      ")."
    )
  }
  init_resp_body <- init_resp$body

  display_uri <- init_resp_body$verification_uri_complete %||%
    init_resp_body$verification_uri
  if (is.null(display_uri)) {
    stop("No verification URI found in device auth response.")
  }

  list(
    verifier = verifier,
    display_uri = display_uri,
    user_code = init_resp_body$user_code,
    device_code = init_resp_body$device_code,
    expires_in = init_resp_body$expires_in,
    interval = init_resp_body$interval
  )
}

ppm_sso_device_flow_message <- function(ppm_url, init_result) {
  cli::cli_rule("PPM SSO Login")
  cli::cli_text("Login at {.url {init_result$display_uri}}")
  cli::cli_text(
    "and enter code {.emph {cli::col_magenta(init_result$user_code)}}
     when prompted."
  )
  if (is_interactive()) {
    readline("Press ENTER to open in browser...")
    utils::browseURL(init_result$display_uri)
  } else if (isTRUE(getOption("pak.is_worker"))) {
    # called from pak, make the UI slightly nicer.
    # unfortunately we cannot interact with the user here
    utils::browseURL(init_result$display_uri)
  }
}

ppm_sso_device_flow <- function(ppm_url) {
  init_result <- ppm_sso_device_flow_init(ppm_url)
  ppm_sso_device_flow_message(ppm_url, init_result)
  token <- ppm_sso_device_flow_complete(ppm_url, init_result)
  if (is.null(token)) {
    stop("Failed to complete device authorization or obtain identity token.")
  }
  token
}

ppm_sso_can_authenticate <- function(ppm_url, token) {
  h <- curl::new_handle()
  curl::handle_setheaders(h, "Authorization" = paste("Bearer", token))
  resp <- curl::curl_fetch_memory(ppm_url, handle = h)
  status <- resp$status_code
  status < 500 && status != 401 && status != 403
}

ppm_sso_identity_to_ppm_token <- function(ppm_url, identity_token) {
  url <- paste0(ppm_url, "/__api__/token")
  payload <- list(
    grant_type = "urn:ietf:params:oauth:grant-type:token-exchange",
    subject_token = identity_token,
    subject_token_type = "urn:ietf:params:oauth:token-type:id_token"
  )

  resp <- ppm_sso_post_form(url, payload)
  if (resp$status >= 400) {
    stop(
      "Failed to exchange identity token for PPM token (HTTP ",
      resp$status,
      ")."
    )
  }

  token_data <- resp$body
  if (is.null(token_data$access_token)) {
    stop("Failed to exchange identity token for PPM token.")
  }

  token_data$access_token
}

ppm_sso_write_token_to_file <- function(ppm_url, token) {
  # this is more difficult than it should be because TOML is unable
  # to represent an empty array of tables
  token_file_path <- ppm_sso_token_path()
  mkdirp(dirname(token_file_path))
  new_conn <- list(
    address = ppm_url,
    token = token,
    auth_type = "sso"
  )

  tokens <- try_catch_null({
    tokens <- suppressWarnings(tstoml::ts_read_toml(token_file_path))
    urls <- tsitter::ts_tree_unserialize(
      tsitter::ts_tree_select(tokens, list("connections", TRUE, "address"))
    )
    idx <- which(urls == ppm_url)[1]
    tokens
  })

  if (is.null(tokens)) {
    tokens <- tstoml::ts_parse_toml("")
    tokens <- tsitter::ts_tree_insert(
      tokens,
      key = "connections",
      list(new_conn)
    )
  } else if (!is.na(idx)) {
    tokens <- tsitter::ts_tree_update(
      tsitter::ts_tree_select(tokens, list("connections", idx, "token")),
      new_conn$token
    )
  } else if (length(urls) == 0) {
    tokens <- tsitter::ts_tree_insert(
      tokens,
      key = "connections",
      list(new_conn)
    )
  } else {
    tokens <- tsitter::ts_tree_insert(
      tsitter::ts_tree_select(tokens, "connections"),
      list(new_conn)
    )
  }

  bytes <- as.raw(tokens)
  file.create(token_file_path)
  Sys.chmod(token_file_path, "600")
  writeBin(bytes, token_file_path)
}

ppm_sso_base64url_decode <- function(x) {
  # Add padding if missing
  padding_needed <- (4 - nchar(x) %% 4) %% 4
  x <- paste0(x, strrep("=", padding_needed))
  # Replace URL-safe characters
  x <- gsub("-", "+", gsub("_", "/", x))
  processx::base64_decode(x)
}

ppm_sso_base64url_encode <- function(x) {
  encoded <- processx::base64_encode(x)
  # Make it URL-safe
  gsub("\\+", "-", gsub("\\/", "_", gsub("=+$", "", encoded)))
}

ppm_sso_hex_to_raw <- function(s) {
  n <- nchar(s)
  as.raw(strtoi(substring(s, seq(1L, n, 2L), seq(2L, n, 2L)), 16L))
}

ppm_sso_sha256_raw <- function(x) {
  ppm_sso_hex_to_raw(cli::hash_sha256(x))
}

ppm_sso_new_pkce_verifier <- function() {
  ppm_sso_base64url_encode(.Call(pkgcache_rand_bytes, 32L))
}

ppm_sso_new_pkce_challenge <- function(verifier) {
  ppm_sso_base64url_encode(ppm_sso_sha256_raw(verifier))
}

ppm_sso_device_flow_complete <- function(ppm_url, init_result) {
  device_code <- init_result$device_code
  verifier <- init_result$verifier
  interval <- init_result$interval %||% 5
  expires_in <- init_result$expires_in %||% 300

  url <- paste0(ppm_url, "/__api__/device_access")
  start_time <- Sys.time()
  payload <- list(
    device_code = device_code,
    code_verifier = verifier
  )

  # PPM might not respond until the user completes auth, so show this
  oldopt <- options(cli.progress_show_after = 0)
  on.exit(options(oldopt), add = TRUE)
  cli::cli_progress_bar(
    format = "{cli::pb_spin} Waiting for browser."
  )
  cli::cli_progress_update()

  while (as.numeric(Sys.time() - start_time) < expires_in) {
    resp <- ppm_sso_post_form(url, payload)
    status <- resp$status

    if (status == 200) {
      cli::cli_progress_done()
      cli::cli_alert_success("Authorization successful.")
      return(resp$body$id_token)
    } else if (status == 400) {
      error_code <- resp$body$error
      if (error_code == "access_denied") {
        cli::cli_progress_done()
        cli::cli_alert_danger("Authorization denied by user.")
        stop("Access denied by user.")
      }
      if (error_code == "expired_token") {
        cli::cli_progress_done()
        cli::cli_alert_danger("Device authorization request expired.")
        stop("Device authorization request expired.")
      }
      # For "authorization_pending" or "slow_down", just wait and retry.
    } else {
      cli::cli_progress_done()
      cli::cli_alert_danger(
        "Device authorization failed (HTTP {status})."
      )
      stop("Device authorization failed.")
    }

    deadline <- Sys.time() + interval
    while (Sys.time() < deadline) {
      Sys.sleep(.1)
      cli::cli_progress_update()
    }
  }

  cli::cli_progress_done()
  cli::cli_alert_danger("Device authorization timed out.")
  stop("Device authorization timed out.")
}
