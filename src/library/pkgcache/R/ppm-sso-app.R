# nocov start

# Fake PPM server that proxies to Auth0, for testing ppm_sso_device_flow().
# Auth0 device flow does not use PKCE, so we verify the PKCE challenge
# locally and forward only the device_code to Auth0's /oauth/token.
ppm_sso_auth0_app <- function(
  auth0_domain,
  client_id,
  audience = NULL,
  scope = "openid profile email",
  redirect_url = "https://packagemanager.posit.co"
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
      scope = app$locals$scope,
      audience = app$locals$audience
    )

    upstream <- ppm_sso_post_form(
      paste0("https://", app$locals$auth0_domain, "/oauth/device/code"),
      payload
    )

    if (upstream$status >= 400L) {
      return(res$set_status(upstream$status)$send_json(
        auto_unbox = TRUE,
        upstream$body
      ))
    }

    assign(upstream$body$device_code, challenge, envir = app$locals$challenges)

    res$send_json(
      auto_unbox = TRUE,
      list(
        device_code = upstream$body$device_code,
        user_code = upstream$body$user_code,
        verification_uri = upstream$body$verification_uri,
        verification_uri_complete = upstream$body$verification_uri_complete,
        expires_in = upstream$body$expires_in,
        interval = upstream$body$interval %||% 5L
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
    actual <- ppm_sso_base64url_encode(ppm_sso_sha256_raw(verifier))
    if (!identical(expected, actual)) {
      return(res$set_status(400L)$send_json(
        auto_unbox = TRUE,
        list(error = "invalid_grant")
      ))
    }

    upstream <- ppm_sso_post_form(
      paste0("https://", app$locals$auth0_domain, "/oauth/token"),
      list(
        grant_type = "urn:ietf:params:oauth:grant-type:device_code",
        device_code = device_code,
        client_id = app$locals$client_id
      )
    )

    if (upstream$status == 200L) {
      rm(list = device_code, envir = app$locals$challenges)
      return(res$send_json(
        auto_unbox = TRUE,
        list(id_token = upstream$body$id_token)
      ))
    }

    # Auth0 returns 403 for authorization_pending / slow_down; the PPM client
    # only treats 400 as a soft pending state, so translate the status.
    res$set_status(400L)$send_json(
      auto_unbox = TRUE,
      list(error = upstream$body$error %||% "unknown_error")
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

  # Everything we don't handle locally (package metadata, downloads, etc.)
  # is proxied to the real PPM via a redirect, preserving path and query.
  # 307 keeps the method and body intact for non-GET requests.
  app$all(webfakes::new_regexp("^/.*$"), function(req, res) {
    target <- paste0(redirect_url, req$path)
    if (nzchar(req$query_string)) {
      target <- paste0(target, "?", req$query_string)
    }
    res$redirect(target, 307L)
  })

  app
}

ppm_sso_app <- function(redirect_url = "https://packagemanager.posit.co") {
  app <- webfakes::new_app()

  app$use("logger" = webfakes::mw_log())
  app$use("urlencoded body parser" = webfakes::mw_urlencoded())
  app$use("json body parser" = webfakes::mw_json())

  app$locals$challenges <- new.env(parent = emptyenv())

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

    device_code <- ppm_sso_base64url_encode(.Call(pkgcache_rand_bytes, 32L))
    user_code <- "ABCD-EFGH"
    verification_uri <- "https://example.invalid/activate"

    assign(device_code, challenge, envir = app$locals$challenges)

    res$send_json(
      auto_unbox = TRUE,
      list(
        device_code = device_code,
        user_code = user_code,
        verification_uri = verification_uri,
        verification_uri_complete = paste0(
          verification_uri,
          "?user_code=",
          user_code
        ),
        expires_in = 300L,
        interval = 1L
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
    actual <- ppm_sso_base64url_encode(ppm_sso_sha256_raw(verifier))
    if (!identical(expected, actual)) {
      return(res$set_status(400L)$send_json(
        auto_unbox = TRUE,
        list(error = "invalid_grant")
      ))
    }

    rm(list = device_code, envir = app$locals$challenges)
    res$send_json(
      auto_unbox = TRUE,
      list(id_token = ppm_sso_local_make_jwt())
    )
  })

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

  # Everything we don't handle locally (package metadata, downloads, etc.)
  # is proxied to the real PPM via a redirect, preserving path and query.
  # 307 keeps the method and body intact for non-GET requests.
  app$all(webfakes::new_regexp("^/.*$"), function(req, res) {
    target <- paste0(redirect_url, req$path)
    if (nzchar(req$query_string)) {
      target <- paste0(target, "?", req$query_string)
    }
    res$redirect(target, 307L)
  })

  app
}

ppm_sso_local_make_jwt <- function(
  iss = "https://ppm-sso-local.invalid/",
  sub = "ppm-sso-local-user",
  aud = "ppm-sso-local",
  ttl = 3600L,
  now = unclass(Sys.time())
) {
  header <- list(alg = "none", typ = "JWT")
  payload <- list(
    iss = iss,
    sub = sub,
    aud = aud,
    iat = as.integer(now),
    exp = as.integer(now + ttl)
  )
  enc <- function(x) {
    ppm_sso_base64url_encode(charToRaw(
      jsonlite::toJSON(x, auto_unbox = TRUE)
    ))
  }
  paste0(enc(header), ".", enc(payload), ".")
}

# nocov end
