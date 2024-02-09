test_that("JSON is standalone", {
  ## baseenv() makes sure that the remotes package env is not used
  env <- new.env(parent = baseenv())
  env$json <- json
  stenv <- env$json$.internal
  objs <- ls(stenv, all.names = TRUE)
  funs <- Filter(function(x) is.function(stenv[[x]]), objs)
  funobjs <- mget(funs, stenv)

  expect_message(
    mapply(codetools::checkUsage, funobjs, funs,
      MoreArgs = list(report = message)
    ),
    NA
  )
})

test_that("JSON parser scalars", {
  expect_equal(json$parse('"foobar"'), "foobar")
  expect_equal(json$parse('""'), "")

  expect_equal(json$parse("42"), 42)
  expect_equal(json$parse("-42"), -42)
  expect_equal(json$parse("42.42"), 42.42)
  expect_equal(json$parse("1e2"), 1e2)
  expect_equal(json$parse("-0.1e-2"), -0.1e-2)

  expect_equal(json$parse("null"), NULL)
  expect_equal(json$parse("true"), TRUE)
  expect_equal(json$parse("false"), FALSE)
})

test_that("JSON parser arrays", {
  cases <- list(
    list("[1,2,3]", list(1, 2, 3)),
    list("[1]", list(1)),
    list("[]", list()),
    list('["foo"]', list("foo")),
    list('["foo", 1, "bar", true]', list("foo", 1, "bar", TRUE))
  )

  for (c in cases) {
    r <- json$parse(c[[1]])
    expect_equal(r, c[[2]], info = c[[1]])
  }
})

test_that("JSON parser nested arrays", {
  cases <- list(
    list('[1,2, ["foo", "bar"], 3]', list(1, 2, list("foo", "bar"), 3)),
    list("[ [ [ 1 ] ] ]", list(list(list(1)))),
    list("[ [ [ ] ] ]", list(list(list())))
  )

  for (c in cases) {
    r <- json$parse(c[[1]])
    expect_equal(r, c[[2]], info = c[[1]])
  }
})

test_that("JSON parser, real examples", {
  inp <- '
{
  "sha": "e183ccdc515bbb8e7f32d8d16586aed9eea6de0b",
  "commit": {
    "author": {
      "name": "Hadley Wickham",
      "email": "h.wickham@gmail.com",
      "date": "2015-03-30T13:55:18Z"
    },
    "committer": {
      "name": "Hadley Wickham",
      "email": "h.wickham@gmail.com",
      "date": "2015-03-30T13:55:18Z"
    },
    "message": "Merge pull request #22 from paulstaab/HEAD\\n\\nImprove error message for assertions of length 0",
    "tree": {
      "sha": "f2e840b7a134fbc118597842992aa50048e0fa04",
      "url": "https://api.github.com/repos/hadley/assertthat/git/trees/f2e840b7a134fbc118597842992aa50048e0fa04"
    },
    "url": "https://api.github.com/repos/hadley/assertthat/git/commits/e183ccdc515bbb8e7f32d8d16586aed9eea6de0b",
    "comment_count": 0
  }
}'

  exp <- list(
    sha = "e183ccdc515bbb8e7f32d8d16586aed9eea6de0b",
    commit = list(
      author = list(
        name = "Hadley Wickham",
        email = "h.wickham@gmail.com",
        date = "2015-03-30T13:55:18Z"
      ),
      committer = list(
        name = "Hadley Wickham",
        email = "h.wickham@gmail.com",
        date = "2015-03-30T13:55:18Z"
      ),
      message = "Merge pull request #22 from paulstaab/HEAD\\n\\nImprove error message for assertions of length 0",
      tree = list(
        sha = "f2e840b7a134fbc118597842992aa50048e0fa04",
        url = "https://api.github.com/repos/hadley/assertthat/git/trees/f2e840b7a134fbc118597842992aa50048e0fa04"
      ),
      url = "https://api.github.com/repos/hadley/assertthat/git/commits/e183ccdc515bbb8e7f32d8d16586aed9eea6de0b",
      comment_count = 0
    )
  )

  expect_equal(json$parse(inp), exp)
})

test_that("JSON parser, errors", {
  expect_error(
    json$parse("[1,2,3,"),
    "EXPECTED value GOT EOF"
  )

  expect_error(
    json$parse('{ 123: "foo" }'),
    "EXPECTED string GOT 123"
  )

  expect_error(
    json$parse('{ "foo" "foobar" }'),
    'EXPECTED : GOT "foobar"'
  )

  expect_error(
    json$parse('{ "foo": "foobar" "foo2": "foobar2" }'),
    'EXPECTED , or } GOT "foo2"'
  )

  expect_error(
    json$parse("[1,2,3 4]"),
    "EXPECTED , GOT 4"
  )
})

test_that("parse_file", {
  local <- withr::local_tempdir()
  path <- file.path(local, "test.json")
  writeLines("[1,2,3,4]", path)
  expect_equal(json$parse_file(path), as.list(1:4))
})
