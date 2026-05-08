ppm_sso_data <- new.env(parent = emptyenv())
ppm_sso_data$name <- "ppm"
ppm_sso_data$viable <- FALSE

ppm_sso_init <- function(url = NULL) {
  url <- url %||% Sys.getenv("PACKAGEMANAGER_ADDRESS", NA_character_)
  if (!is_string(url)) {
    stop(
      "Please set the PACKAGEMANAGER_ADDRESS environment variable to ",
      "the URL of your RStudio Package Manager instance."
    )
  }

  parsed_url <- regmatches(
    url,
    regexec("^(?:https?://)?([^/]+)", url)
  )[[1]]
  if (length(parsed_url) < 2) {
    stop("Invalid Package Manager URL: ", url)
  }

  ppm_sso_data$ppm_url <- url
  ppm_sso_data$service_name <- parsed_url[2]
  ppm_sso_data$token_file_path <- file.path(
    path.expand("~"),
    ".ppm",
    "tokens.toml"
  )
  ppm_sso_data$viable <- TRUE
}

ppm_sso_login <- function(service = NULL) {
  service <- service %||%
    ppm_sso_data$ppm_url %||%
    Sys.getenv("PACKAGEMANAGER_ADDRESS", NA_character_)
  if (!ppm_sso_data$viable) {
    ppm_sso_init()
  }

  if (!ppm_are_requirements_valid(service)) {
    stop(
      "Package Manager SSO is not properly configured. Please ensure that ",
      "the PACKAGEMANAGER_ADDRESS environment variable is set to the URL of ",
      "your Posit Package Manager instance."
    )
  }

  existing_token <- ppm_sso_get_existing_token()
  if (!is.null(existing_token) && ppm_sso_can_authenticate(existing_token)) {
    return(existing_token)
  }

  identity_token <- ppm_sso_get_identity_token_from_file() %||%
    ppm_sso_device_flow()
  ppm_token <- ppm_sso_identity_to_ppm_token(identity_token)
  ppm_sso_write_token_to_file(ppm_token)

  ppm_token
}

ppm_are_requirements_valid <- function(service) {
  is_string(ppm_sso_data$ppm_url) && startsWith(service, ppm_sso_data$ppm_url)
}

ppm_sso_get_existing_token <- function() {
  if (!file.exists(ppm_sso_data$token_file_path)) {
    return(NULL)
  }
  tryCatch(
    {
      tokens_data <- RcppTOML::parseTOML(ppm_sso_data$token_file_path)
      for (conn in tokens_data$connection) {
        if (identical(conn$url, ppm_sso_data$ppm_url)) {
          return(conn$token)
        }
      }
    },
    error = function(e) {
      NULL
    }
  )
}

ppm_sso_can_authenticate <- function(token) {
  req <- httr2::request(ppm_sso_data$ppm_url) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_error(is_error = function(resp) FALSE) # Handle errors manually

  resp <- httr2::req_perform(req)

  status <- httr2::resp_status(resp)
  status < 500 && status != 401 && status != 403
}

ppm_sso_get_identity_token_from_file <- function() {
  token_file <- Sys.getenv("PACKAGEMANAGER_IDENTITY_TOKEN_FILE", unset = NA)
  if (is.na(token_file)) {
    return(NULL)
  }

  tryCatch(
    {
      trimws(readLines(token_file, n = 1, warn = FALSE))
    },
    error = function(e) {
      NULL
    }
  )
}

ppm_sso_device_flow <- function() {
  verifier <- ppm_sso_new_pkce_verifier()
  challenge <- ppm_sso_new_pkce_challenge(verifier)

  # 1. Initiate Device Auth
  init_url <- paste0(ppm_sso_data$ppm_url, "/__api__/device")
  payload <- list(
    code_challenge_method = "S256",
    code_challenge = challenge
  )
  init_resp_body <- httr2::request(init_url) |>
    httr2::req_body_form(!!!payload) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  display_uri <- init_resp_body$verification_uri_complete %||%
    init_resp_body$verification_uri
  if (is.null(display_uri)) {
    stop("No verification URI found in device auth response.")
  }

  message("\nPlease open the following URL in your browser:")
  message(paste("  ", display_uri))
  message("\nAnd enter the following code when prompted:")
  message(paste("  ", init_resp_body$user_code))
  message("\nWaiting for authorization...")

  try(utils::browseURL(display_uri), silent = TRUE)

  # 2. Poll for token
  token_resp_body <- ppm_sso_complete_device_auth(
    init_resp_body$device_code,
    verifier,
    init_resp_body$interval %||% 5,
    init_resp_body$expires_in %||% 300
  )

  if (is.null(token_resp_body) || is.null(token_resp_body$id_token)) {
    stop("Failed to complete device authorization or obtain identity token.")
  }

  token_resp_body$id_token
}

ppm_sso_identity_to_ppm_token <- function(identity_token) {
  url <- paste0(ppm_sso_data$ppm_url, "/__api__/token")
  payload <- list(
    grant_type = "urn:ietf:params:oauth:grant-type:token-exchange",
    subject_token = identity_token,
    subject_token_type = "urn:ietf:params:oauth:token-type:id_token"
  )

  resp <- httr2::request(url) |>
    httr2::req_body_form(!!!payload) |>
    httr2::req_perform()

  token_data <- httr2::resp_body_json(resp)
  if (is.null(token_data$access_token)) {
    stop("Failed to exchange identity token for PPM token.")
  }

  token_data$access_token
}

ppm_sso_write_token_to_file <- function(token) {
  dir.create(
    dirname(ppm_sso_data$token_file_path),
    showWarnings = FALSE,
    recursive = TRUE
  )

  new_connection <- list(
    url = ppm_sso_data$ppm_url,
    token = token,
    method = "sso"
  )

  existing_data <- if (file.exists(ppm_sso_data$token_file_path)) {
    tryCatch(
      RcppTOML::parseTOML(ppm_sso_data$token_file_path),
      error = function(e) {
        list(connection = list())
      }
    )
  } else {
    list(connection = list())
  }

  # Find and update existing entry or add a new one
  found <- FALSE
  if (
    !is.null(existing_data$connection) && length(existing_data$connection) > 0
  ) {
    for (i in seq_along(existing_data$connection)) {
      if (identical(existing_data$connection[[i]]$url, ppm_sso_data$ppm_url)) {
        existing_data$connection[[i]] <- new_connection
        found <- TRUE
        break
      }
    }
  }

  if (!found) {
    existing_data$connection <- c(
      existing_data$connection,
      list(new_connection)
    )
  }

  # Manually construct TOML output
  output_lines <- c()
  for (conn in existing_data$connection) {
    output_lines <- c(
      output_lines,
      "[[connection]]",
      paste0("url = \"", conn$url, "\""),
      paste0("token = \"", conn$token, "\""),
      paste0("method = \"", conn$method, "\""),
      ""
    )
  }
  writeLines(output_lines, ppm_sso_data$token_file_path)
}

ppm_sso_base64url_encode <- function(x) {
  encoded <- openssl::base64_encode(x)
  # Make it URL-safe
  gsub("\\+", "-", gsub("\\/", "_", gsub("=+$", "", encoded)))
}

ppm_sso_new_pkce_verifier <- function() {
  ppm_sso_base64url_encode(openssl::rand_bytes(32))
}

ppm_sso_new_pkce_challenge <- function(verifier) {
  hash <- openssl::sha256(charToRaw(verifier))
  ppm_sso_base64url_encode(hash)
}

ppm_sso_complete_device_auth = function(
  device_code,
  verifier,
  interval,
  expires_in
) {
  url <- paste0(ppm_sso_data$ppm_url, "/__api__/device_access")
  start_time <- Sys.time()
  payload <- list(
    device_code = device_code,
    code_verifier = verifier
  )

  while (as.numeric(Sys.time() - start_time) < expires_in) {
    resp <- httr2::request(url) |>
      httr2::req_body_form(!!!payload) |>
      httr2::req_error(is_error = \(resp) FALSE) |> # Handle errors manually
      httr2::req_perform()

    status <- httr2::resp_status(resp)

    if (status == 200) {
      return(httr2::resp_body_json(resp))
    } else if (status == 400) {
      error_data <- httr2::resp_body_json(resp)
      error_code <- error_data$error
      if (error_code == "access_denied") {
        stop("Access denied by user.")
      }
      if (error_code == "expired_token") {
        stop("Device authorization request expired.")
      }
      # For "authorization_pending" or "slow_down", just wait and retry.
    } else {
      httr2::resp_check_status(resp)
    }

    Sys.sleep(interval)
  }

  stop("Device authorization timed out.")
}

# nocov start

# Fake PPM server that proxies to Auth0, for testing ppm_sso_device_flow().
# Auth0 device flow does not use PKCE, so we verify the PKCE challenge
# locally and forward only the device_code to Auth0's /oauth/token.
ppm_sso_fake_app <- function(
  auth0_domain,
  client_id,
  audience = NULL,
  scope = "openid profile email"
) {
  app <- webfakes::new_app()

  app$use("logger" = webfakes::mw_log())
  app$use("urlencoded body parser" = webfakes::mw_urlencoded())
  app$use("json body parser" = webfakes::mw_json())

  app$locals$challenges <- new.env(parent = emptyenv())
  app$locals$auth0_domain <- auth0_domain
  app$locals$client_id <- client_id
  app$locals$audience <- audience
  app$locals$scope <- scope

  # Bearer-token check used by ppm_sso_can_authenticate(): any token passes.
  app$get("/", function(req, res) {
    res$set_status(200L)$send("ok")
  })

  app$post("/__api__/device", function(req, res) {
    challenge <- req$form$code_challenge
    method <- req$form$code_challenge_method %||% "S256"
    if (!identical(method, "S256")) {
      return(res$set_status(400L)$send_json(
        auto_unbox = TRUE,
        list(error = "unsupported_challenge_method")
      ))
    }

    payload <- list(
      client_id = app$locals$client_id,
      scope = app$locals$scope
    )
    if (!is.null(app$locals$audience)) {
      payload$audience <- app$locals$audience
    }

    upstream <- httr2::request(
      paste0("https://", app$locals$auth0_domain, "/oauth/device/code")
    ) |>
      httr2::req_body_form(!!!payload) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()

    body <- httr2::resp_body_json(upstream)
    if (httr2::resp_status(upstream) >= 400L) {
      return(res$set_status(httr2::resp_status(upstream))$send_json(
        auto_unbox = TRUE,
        body
      ))
    }

    assign(body$device_code, challenge, envir = app$locals$challenges)

    res$send_json(
      auto_unbox = TRUE,
      list(
        device_code = body$device_code,
        user_code = body$user_code,
        verification_uri = body$verification_uri,
        verification_uri_complete = body$verification_uri_complete,
        expires_in = body$expires_in,
        interval = body$interval %||% 5L
      )
    )
  })

  app$post("/__api__/device_access", function(req, res) {
    device_code <- req$form$device_code
    verifier <- req$form$code_verifier

    if (!exists(device_code, envir = app$locals$challenges, inherits = FALSE)) {
      return(res$set_status(400L)$send_json(
        auto_unbox = TRUE,
        list(error = "expired_token")
      ))
    }
    expected <- get(
      device_code,
      envir = app$locals$challenges,
      inherits = FALSE
    )
    actual <- ppm_sso_base64url_encode(openssl::sha256(charToRaw(verifier)))
    if (!identical(expected, actual)) {
      return(res$set_status(400L)$send_json(
        auto_unbox = TRUE,
        list(error = "invalid_grant")
      ))
    }

    upstream <- httr2::request(
      paste0("https://", app$locals$auth0_domain, "/oauth/token")
    ) |>
      httr2::req_body_form(
        grant_type = "urn:ietf:params:oauth:grant-type:device_code",
        device_code = device_code,
        client_id = app$locals$client_id
      ) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()

    body <- httr2::resp_body_json(upstream)
    if (httr2::resp_status(upstream) == 200L) {
      rm(list = device_code, envir = app$locals$challenges)
      return(res$send_json(
        auto_unbox = TRUE,
        list(id_token = body$id_token)
      ))
    }

    # Auth0 returns 403 for authorization_pending / slow_down; the PPM client
    # only treats 400 as a soft pending state, so translate the status.
    res$set_status(400L)$send_json(
      auto_unbox = TRUE,
      list(error = body$error %||% "unknown_error")
    )
  })

  # Trivial token exchange: echo subject_token back as access_token.
  app$post("/__api__/token", function(req, res) {
    if (
      !identical(
        req$form$grant_type,
        "urn:ietf:params:oauth:grant-type:token-exchange"
      )
    ) {
      return(res$set_status(400L)$send_json(
        auto_unbox = TRUE,
        list(error = "unsupported_grant_type")
      ))
    }
    res$send_json(
      auto_unbox = TRUE,
      list(
        access_token = req$form$subject_token,
        token_type = "Bearer",
        issued_token_type = "urn:ietf:params:oauth:token-type:access_token"
      )
    )
  })

  app
}

# nocov end
