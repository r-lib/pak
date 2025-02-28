test_that("basic auth", {
  # in case the pkgcache API breaks
  skip_on_cran()
  skip("not ready yet")

  withr::local_options(
    keyring_backend = "env",
    cli.dynamic = FALSE,
    cli.ansi = FALSE
  )

  fake_cran_auth <- webfakes::local_app_process(
    asNamespace("pkgcache")$cran_app(
      asNamespace("pkgcache")$cran_app_pkgs,
      basic_auth = c(username = "username", password = "token")
    ),
    opts = webfakes::server_opts(num_threads = 5)
  )
  withr::local_options(
    repos = c(CRAN = set_user_in_url(fake_cran_auth$url())),
    pkg.use_bioconductor = FALSE,
    pkg.platforms = "source"
  )

  keyring::key_set_with_value(getOption("repos")[["CRAN"]], "username", "token")

  browser()
})
