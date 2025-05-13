test_that("get_os_from_platform", {
  cases <- list(
    c("x86_64-w64-mingw32", "windows"),
    c("i386-w64-mingw32", "windows"),
    c("x86_64-apple-darwin17.0", "macos"),
    c("x86_64-pc-linux-gnu", "linux"),
    c("x86_64-pc-linux-musl", "linux"),
    c("s390x-ibm-linux-gnu", "linux"),
    c("powerpc64le-unknown-linux-gnu", "linux"),
    c("aarch64-unknown-linux-gnu", "linux"),
    c("i386-pc-solaris2.10", "solaris"),
    c("x86_64-pc-solaris2.10", "solaris"),
    c("amd64-portbld-freebsd12.1", "freebsd")
  )

  for (case in cases) {
    expect_equal(get_os_from_platform(case[[1]]), case[[2]])
  }
})

test_that("get_arch_from_platform", {
  cases <- list(
    c("x86_64-w64-mingw32", "x86_64"),
    c("i386-w64-mingw32", "i386"),
    c("x86_64-apple-darwin17.0", "x86_64"),
    c("x86_64-pc-linux-gnu", "x86_64"),
    c("x86_64-pc-linux-musl", "x86_64"),
    c("s390x-ibm-linux-gnu", "s390x"),
    c("powerpc64le-unknown-linux-gnu", "powerpc64le"),
    c("aarch64-unknown-linux-gnu", "aarch64"),
    c("i386-pc-solaris2.10", "i386"),
    c("x86_64-pc-solaris2.10", "x86_64"),
    c("amd64-portbld-freebsd12.1", "amd64")
  )

  for (case in cases) {
    expect_equal(get_arch_from_platform(case[[1]]), case[[2]])
  }
})

test_that("get_libc_from_platform", {
  cases <- list(
    c("x86_64-w64-mingw32", NA_character_),
    c("i386-w64-mingw32", NA_character_),
    c("x86_64-apple-darwin17.0", NA_character_),
    c("x86_64-pc-linux-gnu", "gnu"),
    c("x86_64-pc-linux-musl", "musl"),
    c("s390x-ibm-linux-gnu", "gnu"),
    c("powerpc64le-unknown-linux-gnu", "gnu"),
    c("aarch64-unknown-linux-gnu", "gnu"),
    c("i386-pc-solaris2.10", NA_character_),
    c("x86_64-pc-solaris2.10", NA_character_),
    c("amd64-portbld-freebsd12.1", NA_character_)
  )

  for (case in cases) {
    expect_equal(get_libc_from_platform(case[[1]]), case[[2]])
  }
})

test_that("platform_match", {
  good <- list(
    # Identical is always good
    c("x86_64-w64-mingw32", "x86_64-w64-mingw32"),
    c("x86_64-apple-darwin17.0", "x86_64-apple-darwin17.0"),
    c("x86_64-pc-linux-gnu", "x86_64-pc-linux-gnu"),
    c("i386-pc-solaris2.10", "i386-pc-solaris2.10"),
    c("amd64-portbld-freebsd12.1", "amd64-portbld-freebsd12.1"),

    # Windows, archs don't matter
    c("x86_64-w64-mingw32", "i386-w64-mingw32"),

    # All matches for macOS if arch matches
    c("x86_64-apple-darwin17.0", "x86_64-apple-darwin13.4.0"),
    c("x86_64-apple-darwin13.4.0", "x86_64-apple-darwin17.0"),

    # Linux, arch must match. musl on install is OK
    c("x86_64-pc-linux-musl", "x86_64-pc-linux-gnu")
  )

  for (case in good) {
    expect_true(platform_match(case[[1]], case[[2]]))
  }

  bad <- list(
    # OS must match
    c("x86_64-pc-linux-gnu", "x86_64-pc-solaris2.10"),

    # macOS arch must match
    c("arm64-apple-darwin17.0", "x86_64-apple-darwin13.4.0"),

    # Linux arch must match
    c("aarch64-unknown-linux-gnu", "x86_64-pc-linux-gnu"),
    # libc must match if install no musl
    c("x86_64-pc-linux-gnu", "x86_64-pc-linux-musl"),

    # Solaris arch must match
    c("i386-pc-solaris2.10", "x86_64-pc-solaris2.10"),

    # Others must fully match
    c("amd64-portbld-freebsd12.1", "amd64-portbld-freebsd12.2")
  )

  for (case in bad) {
    expect_false(platform_match(case[[1]], case[[2]]))
  }
})

test_that("check_platform", {
  # load_all() is fine without data
  fake(check_platform, "file.exists", function(...) FALSE)
  expect_silent(check_platform(".", "."))

  # during installation?
  fake(check_platform, "file.exists", function(...) TRUE)
  withr::with_envvar(
    c(R_PACKAGE_DIR = "foobar"),
    expect_silent(check_platform(".", "."))
  )

  # clean
  current <- R.Version()$platform
  expect_silent(check_platform(".", ".", data = list(platform = current)))

  # warn for non-match
  expect_warning(
    check_platform(".", ".", data = list(platform = "not-this-time")),
    "Wrong OS or architecture"
  )
})
