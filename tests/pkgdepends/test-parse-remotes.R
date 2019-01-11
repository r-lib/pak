
context("parse_remotes")

test_that("parse_remotes, standard", {

  cases <- list(
    list("pkg",
         list(package = "pkg", atleast = "", version = "")),
    list("pkg@0.1-2",
         list(package = "pkg", atleast = "", version = "0.1-2")),
    list("pkg@>=2.9",
         list(package = "pkg", atleast = ">=", version = "2.9")),
    list("pkg@last",
         list(package = "pkg", atleast = "", version = "last")),
    list("standard::pkg",
         list(package = "pkg", atleast = "", version = "")),
    list("standard::pkg@0.1-2",
         list(package = "pkg", atleast = "", version = "0.1-2"))
  )

  expect_equal(
    get_remote_types(vcapply(cases, "[[", 1)),
    rep("standard", length(cases))
  )

  for (case in cases) {
    expect_equal_named_lists(
      p <- parse_remotes(case[[1]])[[1]],
      c(case[[2]], ref = case[[1]], type = "standard")
    )
    expect_s3_class(p, c("remote_ref_cran", "remote_ref"))
  }

})

test_that("parse_remotes, cran", {

  cases <- list(
    list("cran::pkg",
         list(package = "pkg", atleast = "", version = "")),
    list("cran::pkg@0.1-2",
         list(package = "pkg", atleast = "", version = "0.1-2"))
  )

  expect_equal(
    get_remote_types(vcapply(cases, "[[", 1)),
    rep("cran", length(cases))
  )

  for (case in cases) {
    expect_equal_named_lists(
      p <- parse_remotes(case[[1]])[[1]],
      c(case[[2]], ref = case[[1]], type = "cran")
    )
    expect_s3_class(p, c("remote_ref_cran", "remote_ref"))
  }

})

test_that("github regexes", {
  username <- list(
    list("foobar", "foobar"),
    list("", NA_character_),
    list("-bad", NA_character_),
    list("bad-", NA_character_),
    list("still--bad", NA_character_),
    list("123456789012345678901234567890123456789",
         "123456789012345678901234567890123456789"),
    list("1234567890123456789012345678901234567890", NA_character_)
  )
  for (c in username) {
    rx <- paste0("^", github_username_rx(), "$")
    expect_equal(
      rematch2::re_match(c[[1]], rx)$username,
      c[[2]]
    )
  }

  commitish <- list(
    list("", NA_character_),
    list("x", NA_character_),
    list("foobar", NA_character_),
    list("@foobar", "foobar"),
    list("@*foobar", NA_character_)
  )
  for (c in commitish) {
    expect_equal(
      rematch2::re_match(c[[1]], github_commitish_rx())$commitish,
      c[[2]]
    )
  }

  pull <- list(
    list("", NA_character_),
    list("x", NA_character_),
    list("@foobar", NA_character_),
    list("#", NA_character_),
    list("#123", "123"),
    list("#1", "1"),
    list("#foobar", NA_character_),
    list("#1foo", "1")
  )
  for (c in pull) {
    expect_equal(
      rematch2::re_match(c[[1]], github_pull_rx())$pull,
      c[[2]]
    )
  }

  release <- list(
    list("", NA_character_),
    list("x", NA_character_),
    list("@foobar", NA_character_),
    list("@*foobar", NA_character_),
    list("@*release", "*release")
  )
  for (c in release) {
    expect_equal(
      rematch2::re_match(c[[1]], github_release_rx())$release,
      c[[2]]
    )
  }

  detail <- list(
    list("@foobar",   c("foobar", "",    "")),
    list("#123",       c("",       "123", "")),
    list("@*release", c("",       "",    "*release")),
    list("foobar",     c("",       "",    ""))
  )
  for (c in detail) {
    expect_equal(
      unlist(rematch2::re_match(
        c[[1]],
        github_detail_rx())[, c("commitish", "pull", "release")]),
      structure(c[[2]], names = c("commitish", "pull", "release"))
    )
  }
})

test_that("github url regexes", {
  cases <- list(
    list("https://github.com/u/repo.git", c(username = "u", repo = "repo")),
    list("https://github.com/u/re.po", c(username = "u", repo = "re.po")),
    list("https://github.com/u/re.po.git", c(username = "u", repo = "re.po"))
  )
  for (c in cases) {
    m <- rematch2::re_match(c[[1]], github_url_rx())
    for (n in names(c[[2]])) expect_equal(c[[2]][[n]], m[[n]])
  }
})

test_that("parse_remotes, github", {

  cases <- list(
    list("user/repo"),
    list("github::user/repo"),
    list("pkg=user/repo", package = "pkg"),
    list("pkg=github::user/repo", package = "pkg"),
    list("user/repo/subdir", subdir = "subdir"),
    list("user/repo@badcafe", commitish = "badcafe"),
    list("user/repo#123", pull = "123"),
    list("user/repo@*release", release = "*release"),
    list("github::user/repo/subdir", subdir = "subdir"),
    list("github::user/repo@badcafe", commitish = "badcafe"),
    list("github::user/repo#123", pull = "123"),
    list("github::user/repo@*release", release = "*release"),
    list("pkg=user/repo/subdir", package = "pkg", subdir = "subdir"),
    list("pkg=user/repo@badcafe", package = "pkg", commitish = "badcafe"),
    list("pkg=user/repo#123", package = "pkg", pull = "123"),
    list("pkg=user/repo@*release", package = "pkg", release = "*release"),

    # github url cases
    list("git@github.com:user/repo.git"),
    list("git@github.ubc.ca:user/repo.git"),
    list("https://github.com/user/repo"),
    list("https://github.ubc.ca/user/repo"),
    list("https://github.com/user/repo/tree/i-am-a-branch", commitish = "i-am-a-branch"),
    list("https://github.com/user/repo/commit/1234567", commitish = "1234567"),
    list("https://github.com/user/repo/pull/108", pull = "108"),
    list("https://github.com/user/repo/releases/tag/1.0.0", commitish = "1.0.0"),
    list("https://github.com/user/repo/releases/latest", release = "latest"),
    list("https://github.com/user/repo/releases/latest", release = "latest"),
    list("https://github.com/foo/bar", username = "foo", repo = "bar"),
    list("git@github.com:foo/bar.git", username = "foo", repo = "bar"),

    # Username and repo can have hyphens in them
    list("git@github.com:foo-bar/baz-qux.git", username = "foo-bar", repo = "baz-qux")
  )

  for (case in cases) {
    expect_equal_named_lists(
      p <- parse_remotes(case[[1]])[[1]],
      utils::modifyList(
        list(package = case$repo %||% "repo", username = "user",
             repo = "repo", subdir = "", commitish = "", pull = "",
             release = "", ref = case[[1]], type = "github"),
        case[-1]
      )
    )
    expect_s3_class(p, c("remote_ref_github", "remote_ref"))
  }
})

test_that("parse_remotes error on unknown type", {
  expect_error(parse_remotes("my_package"), "parse remotes")
})

test_that("custom remote types", {
  xspecs <- NULL
  xargs <- NULL
  parse_remote_foo <- function(specs, ...) {
    xspecs <<- specs
    xargs <<- list(...)
    list(list())
  }
  res <- withr::with_options(
    list(pkg.remote_types = list(foo = list(parse = parse_remote_foo))),
    parse_remotes("foo::arbitrary_string/xxx", ex1 = "1", ex2 = "2")
  )
  expect_identical(
    res,
    list(structure(list(type = "foo"),
                   class = c("remote_ref_foo", "remote_ref", "list")))
  )
  expect_identical(xspecs, "foo::arbitrary_string/xxx")
  expect_identical(xargs, list(ex1 = "1", ex2 = "2"))

  res2 <- parse_remotes(
    "foo::arbitrary_string/xxx", ex1 = "1", ex2 = "2",
    remote_types = list(foo = list(parse = parse_remote_foo)))
  expect_identical(res, res2)
})

test_that("type_default_parse", {
  res <- type_default_parse(c("foo::bar", "package=foo2::bar2"))
  expect_identical(res,
    list(
      list(package = "", type = "foo", rest = "bar", ref = "foo::bar"),
      list(package = "package", type = "foo2", rest = "bar2",
           ref = "package=foo2::bar2")
    )
  )
})

test_that("default parse function", {
  res <- withr::with_options(
    list(pkg.remote_types = list(foo = list(), foo2 = list())),
    parse_remotes(c("foo::bar", "package=foo2::bar2"))
  )
  expect_identical(res,
    list(
      structure(list(package = "", type = "foo", rest = "bar", ref = "foo::bar"),
                class = c("remote_ref_foo", "remote_ref", "list")),
      structure(list(package = "package", type = "foo2", rest = "bar2",
                     ref = "package=foo2::bar2"),
                class = c("remote_ref_foo2", "remote_ref", "list"))
    )
  )

  res2 <- parse_remotes(
    c("foo::bar", "package=foo2::bar2"),
    remote_types = list(foo = list(), foo2 = list()))
  expect_identical(res, res2)
})
