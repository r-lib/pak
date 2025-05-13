test_that("check_keyring_backend, file", {
  skip_on_cran()

  # default keyring file does not exist
  withr::local_options(
    keyring_backend = "file",
    keyring_keyring = basename(tempfile())
  )
  expect_snapshot(
    error = TRUE,
    check_keyring_backend(keyring::default_backend()),
    transform = transform_tempfile
  )

  # default keyring is locked
  expect_snapshot(
    repo_auth_unlock_internal(password = "secret"),
    transform = transform_tempfile
  )
  keyring::keyring_lock()
  expect_snapshot(
    error = TRUE,
    check_keyring_backend(keyring::default_backend()),
    transform = transform_tempfile
  )
})

test_that("check_keyring_backend, secret service", {
  skip_on_cran()

  # not Linux
  withr::local_options(
    keyring_backend = "secret_service"
  )
  fake(check_keyring_backend, "Sys.info", list(sysname = "Windows"))
  expect_snapshot(
    error = TRUE,
    check_keyring_backend(keyring::default_backend())
  )

  # no secret service support
  fake(check_keyring_backend, "Sys.info", list(sysname = "Linux"))
  fake(check_keyring_backend, "backend_is_available", function(...) {
    stop("keyring build has no libsecret support")
  })
  expect_snapshot(
    error = TRUE,
    check_keyring_backend(keyring::default_backend())
  )

  # ok
  fake(check_keyring_backend, "backend_is_available", TRUE)
  expect_silent(
    check_keyring_backend(keyring::default_backend())
  )
})

test_that("repo_auth_key_get_internal, repo_auth_key_set_internal", {
  skip_on_cran()

  withr::local_options(
    keyring_backend = "env"
  )

  url <- "https://username@cloud.r-project.org"
  expect_snapshot({
    repo_auth_key_set_internal(url, "secret")
  })

  expect_snapshot({
    pw <- repo_auth_key_get_internal(url)
  })
  expect_equal(pw, "secret")
})

test_that("repo_auth_unlock_internal", {
  skip_on_cran()

  withr::local_options(
    keyring_backend = "env"
  )

  # no need to unlock
  expect_snapshot({
    repo_auth_unlock_internal("pass")
  })

  # create file keyring
  withr::local_options(
    keyring_backend = "file",
    keyring_keyring = basename(tempfile())
  )
  expect_snapshot(
    {
      repo_auth_unlock_internal("pass")
    },
    transform = transform_tempfile
  )

  # it exists, but unlock it
  keyring::keyring_lock()
  expect_snapshot({
    repo_auth_unlock_internal("pass")
  })

  # unlock with bad password
  keyring::keyring_lock()
  expect_snapshot(error = TRUE, {
    repo_auth_unlock_internal("pass2")
  })
})
