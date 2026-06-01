test_that("PPM SSO: login, status and download", {
  skip_on_cran()

  # cran_app() and ppm_sso_app() are pkgcache test helpers. The PPM SSO app
  # handles the login endpoints locally and redirects everything else
  # (package metadata, downloads) to the fake CRAN repo.
  pkgcache <- asNamespace("pkgcache")
  cran <- webfakes::local_app_process(
    pkgcache$cran_app(pkgcache$cran_app_pkgs),
    opts = webfakes::server_opts(num_threads = 5)
  )
  ppm <- webfakes::local_app_process(
    pkgcache$ppm_sso_app(redirect_url = sub("/$", "", cran$url())),
    opts = webfakes::server_opts(num_threads = 5)
  )
  ppm_url <- sub("/$", "", ppm$url())

  # Point pak at the fake PPM. The `__token__` user name in the repo URL
  # triggers the SSO credential lookup. Both PACKAGEMANAGER_ADDRESS and
  # `repos` are forwarded to the pak subprocess automatically.
  repo_url <- set_user_in_url(ppm_url, "__token__")
  withr::local_envvar(PACKAGEMANAGER_ADDRESS = ppm_url)
  withr::local_options(repos = c(PPM = repo_url))

  # Redirect the token cache (~/.ppm/tokens.toml) in the subprocess into a
  # temporary HOME, and stop the device flow from trying to open a browser
  # (the worker sets `pak.is_worker = TRUE`). Restart the worker afterwards
  # so these changes do not leak into other tests.
  home <- withr::local_tempdir()
  withr::defer({
    if (remote_is_alive()) pkg_data$remote$kill()
  })
  remote(
    function(home) {
      Sys.setenv(HOME = home, USERPROFILE = home)
      options(browser = function(url) invisible(NULL))
    },
    list(home = home)
  )

  # 1. Log in. Runs the device flow against the fake PPM, exchanges the
  #    identity token for a PPM token and caches it in the token file.
  token <- suppressMessages(ppm_sso_login())
  expect_type(token, "character")
  expect_true(nzchar(token))
  token_file <- file.path(home, ".ppm", "tokens.toml")
  expect_true(file.exists(token_file))

  # 2. Status reports the cached token and its decoded JWT claims.
  st <- ppm_sso_status()
  expect_equal(st$ppm_url, ppm_url)
  expect_equal(normalizePath(st$token_file), normalizePath(token_file))
  expect_equal(st$token, token)
  expect_equal(st$issuer, "https://ppm-sso-local.invalid/")
  expect_equal(st$subject, "ppm-sso-local-user")
  expect_equal(st$audience, "ppm-sso-local")
  expect_false(st$expired)
  expect_gt(as.numeric(st$expires_in, units = "secs"), 0)
  # without connect = TRUE we do not contact the server
  expect_true(is.na(st$valid))

  # with connect = TRUE the token is verified against the fake PPM, whose
  # root endpoint accepts any bearer token
  st2 <- ppm_sso_status(connect = TRUE)
  expect_true(st2$valid)

  # 3. Download a package through the authenticated PPM repo.
  dest <- withr::local_tempdir()
  dl <- suppressMessages(pkg_download(
    "pkg1",
    dest_dir = dest,
    platforms = "source"
  ))
  expect_true("pkg1" %in% dl$package)
  expect_true(all(file.exists(dl$fulltarget)))
  expect_true(all(file.size(dl$fulltarget) > 0))
})
