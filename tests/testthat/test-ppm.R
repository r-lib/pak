test_that("ppm_has_binaries", {
  expect_silent(ppm_has_binaries())
})

test_that("ppm_platforms", {
  withr::local_envvar(PKGCACHE_PPM_STATUS_URL = ppm$url("/ppmstatus"))
  expect_snapshot(as.data.frame(ppm_platforms()))
})

test_that("ppm_r_versions", {
  withr::local_envvar(PKGCACHE_PPM_STATUS_URL = ppm$url("/ppmstatus"))
  expect_snapshot(as.data.frame(ppm_r_versions()))
})

test_that("ppm_repo_url", {
  withr::local_envvar(
    PKGCACHE_PPM_URL = NA_character_,
    PKGCACHE_RSPM_URL = my <- "https://my.rspm/repo"
  )
  expect_equal(ppm_repo_url(), my)
})

test_that("ppm_repo_url 2", {
  withr::local_envvar(
    PKGCACHE_PPM_URL = NA_character_,
    PKGCACHE_RSPM_URL = NA_character_
  )
  withr::local_options(
    repos = c(
      RSPM = "https://packagemanager.rstudio.com/all/__linux__/jammy/latest",
      CRAN = "https://cran.rstudio.com"
    )
  )

  expect_equal(ppm_repo_url(), "https://packagemanager.rstudio.com/all")
})

test_that("ppm_snapshots", {
  withr::local_envvar(
    PKGCACHE_PPM_TRANSACTIONS_URL = ppm$url("/ppmversions")
  )
  expect_snapshot(ppm_snapshots())
})
