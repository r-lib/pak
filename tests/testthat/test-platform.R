
test_that("get_libc_from_os", {
  cases <- list(
    c("mingw32", NA_character_),
    c("darwin17.0", NA_character_),
    c("linux-gnu-ubuntu-18.04", "gnu"),
    c("linux-musl-alpine-3.11.11", "musl"),
    c("solaris2.10", NA_character_),
    c("freebsd12.1", NA_character_)
  )

  for (case in cases) {
    expect_equal(get_libc_from_os(case[[1]]), case[[2]])
  }
})

test_that("platform_match", {
  good <- list(
    # Identical is always good
    c("x86_64-w64-mingw32", "x86_64-w64-mingw32"),
    c("x86_64-apple-darwin17.0", "x86_64-apple-darwin17.0"),
    c("x86_64-pc-linux-musl-alpine-3.11.11", "x86_64-pc-linux-gnu-18.04"),
    c("x86_64-pc-linux-gnu-ubuntu-18.04", "x86_64-pc-linux-gnu-18.04"),
    c("i386-pc-solaris2.10", "i386-pc-solaris2.10"),
    c("amd64-portbld-freebsd12.1", "amd64-portbld-freebsd12.1"),

    # All matches for macOS if arch matches
    c("x86_64-apple-darwin17.0", "x86_64-apple-darwin13.4.0"),
    c("x86_64-apple-darwin13.4.0", "x86_64-apple-darwin17.0"),

    # Linux, arch must match. musl on install is OK
    c("x86_64-pc-linux-musl", "x86_64-pc-linux-gnu")
  )

  for (case in good) {
    expect_true(platform_match(parse_platform(case[[1]]), parse_platform(case[[2]])))
  }

  bad <- list(
    # OS must match
    c("x86_64-pc-linux-gnu", "x86_64-pc-solaris2.10"),

    # Windows, arch must match
    c("x86_64-w64-mingw32", "i386-w64-mingw32"),

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
  mockery::stub(check_platform, "file.exists", FALSE)
  expect_silent(check_platform(".", "."))

  # during installation?
  mockery::stub(check_platform, "file.exists", TRUE)
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
