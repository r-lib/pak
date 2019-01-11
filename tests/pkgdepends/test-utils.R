
context("utils")

test_that("%|z|%", {
  expect_true("" %|z|% TRUE)

  bad <- list(
    character(),
    c("", ""),
    "foo",
    structure("", class = "foo"),
    NULL
  )
  for (b in bad) expect_identical(b %|z|% FALSE, b)
})

test_that("current_r_platform", {
  mockery::stub(current_r_platform, "get_platform",
                list(pkgType = "mac.binary"))
  expect_equal(current_r_platform(), "macos")

  mockery::stub(current_r_platform, "get_platform",
                list(pkgType = "win.binary"))
  expect_equal(current_r_platform(), "windows")

  mockery::stub(current_r_platform, "get_platform", list(pkgType = "source"))
  expect_equal(current_r_platform(), "source")

  mockery::stub(current_r_platform, "get_platform", list(pkgType = "foobar"))
  expect_equal(current_r_platform(), "source")

  mockery::stub(current_r_platform, "get_platform", list(pkgType = NULL))
  expect_equal(current_r_platform(), "source")
})

test_that("default_cran_mirror", {
  m1 <- withr::with_options(
    list(repos = c(CRAN = "@CRAN@")),
    default_cran_mirror()
  )
  m2 <- withr::with_options(
    list(repos = NULL),
    default_cran_mirror()
  )
  m3 <- withr::with_options(
    list(repos = c("foo" = "bar")),
    default_cran_mirror()
  )

  expect_true(is.character(m1) && length(m1) == 1 && !is.na(m1))
  expect_identical(m1, m2)
  expect_identical(m1, m3)

  m4 <- withr::with_options(
    list(repos = c(CRAN = "mymirror")),
    default_cran_mirror()
  )
  expect_identical(m4, c(CRAN = "mymirror"))
})

test_that("current_r_version", {
  ver <- current_r_version()
  expect_true(is.character(ver))
  expect_true(length(ver) == 1)
})

test_that("vlapply", {
  l <- list(NULL, "", character(), 1)
  expect_identical(
    vapply(l, is.character, logical(1)),
    vlapply(l, is.character)
  )
  expect_identical(
    vapply(list(), is.character, logical(1)),
    vlapply(list(), is.character)
  )
  expect_error(vlapply(l, identity), "values must be length 1")
  expect_error(vlapply(1:5, identity), "values must be type .*logical")
})

test_that("vdapply", {
  l <- list(NULL, "", character(), 1)
  f <- function(x) as.double(length(x))
  expect_identical(
    vapply(l, f, double(1)),
    vdapply(l, f)
  )
  expect_identical(
    vapply(list(), f, double(1)),
    vdapply(list(), f)
  )
  expect_error(vdapply(l, identity), "values must be length 1")
  expect_error(vdapply(letters, identity), "values must be type .*double")
})

test_that("update_named_vector", {
  cases <- list(
    list(c(a=1, b=2), c(a=2, c=5), c(a=2, b=2, c=5)),
    list(double(), c(a=2), c(a=2)),
    list(character(), character(), character()),
    list(c(a=1), double(), c(a=1))
  )

  for (c in cases) {
    expect_identical(update_named_vector(c[[1]], c[[2]]), c[[3]])
  }

  expect_error(update_named_vector(1, c(a=1)), "named entries")
  expect_error(update_named_vector(c(a=1), 1), "named entries")
})

test_that("make_dl_status", {
  obj <- list(
    status = "status",
    url = "url",
    target = "target",
    bytes = NA_real_,
    error = NULL
  )

  expect_identical(
    make_dl_status("Got", obj$url, obj$target, 100L),
    update_named_vector(obj, list(status = "Got", bytes = 100))
  )

  expect_identical(
    make_dl_status("Failed", obj$url, obj$target, error = "foobar"),
    update_named_vector(obj, list(status = "Failed", error = "foobar"))
  )

  expect_identical(
    make_dl_status("Had", obj$url, obj$target, 100),
    update_named_vector(obj, list(status = "Had", bytes = 100))
  )

})

test_that("write_bin_atomic", {
  ## temp file is cleaned up
  write_bin_atomic(raw(10), tmp <- tempfile())
  expect_false(file.exists(paste0(tmp, ".tmp")))
  ## otherwise we would need a stress test with multiple processes...
})

test_that("save_rds_atomic", {
  ## temp file is cleaned up
  save_rds_atomic(1:10, tmp <- tempfile())
  expect_false(file.exists(paste0(tmp, ".tmp")))
  ## otherwise we would need a stress test with multiple processes...
})

test_that("comma_wrap", {
  expect_equal(
    withr::with_options(
      list(width = 15),
      comma_wrap(c("foo", "x", "foo2"))
    ),
    "  foo, x,\n  foo2"
  )

  expect_equal(
    withr::with_options(
      list(width = 10),
      comma_wrap(c("foo", "x", "foo2"), indent = 0)
    ),
    "foo, x,\nfoo2"
  )

  expect_equal(
    withr::with_options(
      list(width = 10),
      comma_wrap(c("foo", "x", "foo2"), indent = 0, exdent = 2)
    ),
    "foo, x,\n  foo2"
  )

  expect_equal(
    withr::with_options(
      list(width = 15),
      comma_wrap(c("foo", "x", "foo2"), sep = "xx ")
    ),
    "  fooxx xxx\n  foo2"
  )
})

test_that("is_na_scalar", {
  pos <- list(NA, NA_character_, NA_real_, NA_integer_, NA_complex_)
  neg <- list(logical(), integer(), 1, 1L, NULL, "foobar", c(NA, 1))

  for (p in pos) expect_true(is_na_scalar(p))
  for (n in neg) expect_false(is_na_scalar(n))
})

test_that("omit_cols", {
  df <- data.frame(a = 1:5, b = 5:1, c = letters[1:5])
  expect_identical(omit_cols(df, character()), df)
  expect_identical(omit_cols(df, "x"), df)
  expect_identical(omit_cols(df, "a"), df[, 2:3])
  expect_identical(omit_cols(df, c("a", "b")), df[, 3, drop = FALSE])
  expect_identical(omit_cols(df, c("a", "b", "c")), df[, c(), drop = FALSE])
})

test_that("get_all_package_dirs", {
  res <- get_all_package_dirs(
    unique(c(current_r_platform(), "source")), current_r_version())

  expect_s3_class(res, "tbl_df")
  expect_equal(
    colnames(res),
    c("platform", "rversion", "contriburl", "prefix"))
  expect_gte(nrow(res), 1)
  expect_true(all(sapply(res, is.character)))
})

test_that("same_sha", {
  expect_true(same_sha("badcafe", "b"))
  expect_true(same_sha("b", "badcafe"))
  expect_false(same_sha("badcafe1", "badcafebadcafe"))
})

test_that("format_iso_8601", {
  d <- structure(1266510204, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  expect_equal(format_iso_8601(d), "2010-02-18T16:23:24+00:00")
})
