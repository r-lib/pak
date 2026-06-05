test_that("PPM SSO: login, status and download", {
  skip_on_cran()

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

  repo_url <- set_user_in_url(ppm_url, "__token__")
  token_file <- withr::local_tempfile()
  withr::local_envvar(PACKAGEMANAGER_ADDRESS = ppm_url)
  withr::local_options(repos = c(PPM = repo_url))
  withr::local_envvar(PACKAGEMANAGER_SSO_TOKEN_FILE = token_file)

  withr::defer({
    if (remote_is_alive()) pkg_data$remote$kill()
  })
  remote(
    function() {
      options(browser = function(url) invisible(NULL))
    }
  )

  # login
  token <- suppressMessages(ppm_sso_login())
  expect_type(token, "character")
  expect_true(nzchar(token))
  expect_true(file.exists(token_file))

  # status
  st <- ppm_sso_status()
  expect_equal(st$ppm_url, ppm_url)
  expect_equal(normalizePath(st$token_file), normalizePath(token_file))
  expect_equal(st$token, token)
  expect_equal(st$issuer, "https://ppm-sso-local.invalid/")
  expect_equal(st$subject, "ppm-sso-local-user")
  expect_equal(st$audience, "ppm-sso-local")
  expect_false(st$expired)
  expect_gt(as.numeric(st$expires_in, units = "secs"), 0)
  expect_true(is.na(st$valid))

  # status with connection
  st2 <- ppm_sso_status(connect = TRUE)
  expect_true(st2$valid)

  # download a package file
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
