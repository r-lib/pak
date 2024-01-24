test_that("%||%", {
  expect_equal("foo" %||% "bar", "foo")
  expect_equal(NULL %||% "bar", "bar")
  expect_equal(NULL %||% NULL, NULL)
})

test_that("vcapply", {
  expect_equal(
    vcapply(character(0), function(x) "foo"),
    structure(character(), names = character())
  )
  expect_equal(
    vcapply("foo", function(x) x),
    c(foo = "foo")
  )
  expect_equal(
    vcapply(c(bar = "foo"), function(x) x),
    c(bar = "foo")
  )
  expect_equal(
    vcapply(letters, function(x) x),
    structure(letters, names = letters)
  )
  expect_snapshot(error = TRUE, {
    vcapply(letters, function(x) 1L)
    vcapply(1:5, function(x) c("foo", "bar"))
  })
})

test_that("vlapply", {
  expect_equal(
    vlapply(character(0), function(x) TRUE),
    structure(logical(), names = character())
  )
  expect_equal(
    vlapply("foo", function(x) TRUE),
    c(foo = TRUE)
  )
  expect_equal(
    vlapply(c(bar = "foo"), function(x) TRUE),
    c(bar = TRUE)
  )
  expect_equal(
    vlapply(letters, function(x) match(x, letters) %% 2 == 0),
    structure(seq_along(letters) %% 2 == 0, names = letters)
  )
  expect_snapshot(error = TRUE, {
    vlapply(letters, function(x) 1L)
    vlapply(1:5, function(x) c(TRUE, FALSE))
  })
})

test_that("viapply", {
  expect_equal(
    viapply(character(0), function(x) 1L),
    structure(integer(), names = character())
  )
  expect_equal(
    viapply("foo", function(x) 1L),
    c(foo = 1L)
  )
  expect_equal(
    viapply(c(bar = "foo"), function(x) 1L),
    c(bar = 1L)
  )
  expect_equal(
    viapply(letters, function(x) match(x, letters)),
    structure(seq_along(letters), names = letters)
  )
  expect_snapshot(error = TRUE, {
    viapply(letters, function(x) 1.0)
    viapply(1:5, function(x) 1:2)
  })
})

test_that("vdapply", {
  expect_equal(
    vdapply(character(0), function(x) 1.0),
    structure(double(), names = character())
  )
  expect_equal(
    vdapply("foo", function(x) 1.0),
    c(foo = 1.0)
  )
  expect_equal(
    vdapply(c(bar = "foo"), function(x) 1.0),
    c(bar = 1.0)
  )
  expect_equal(
    vdapply(letters, function(x) match(x, letters) / 2),
    structure(seq_along(letters) / 2, names = letters)
  )
  expect_snapshot(error = TRUE, {
    vdapply(letters, function(x) "boo")
    vdapply(1:5, function(x) 1:2 / 2)
  })
})

test_that("str_trim", {
  expect_equal(
    str_trim(c(
      "x", " x", "x ", " x ", "  x  ", "\nx\n", "\tx\t",
      " \u00a0x\u00a0\n"
    )),
    rep("x", 8)
  )
})

test_that("get_minor_r_version", {
  expect_equal(get_minor_r_version("4.3.2"), "4.3")
  expect_equal(get_minor_r_version("4.3.0"), "4.3")
  expect_equal(get_minor_r_version("3.6.3.2"), "3.6")
  expect_equal(get_minor_r_version("4.3"), "4.3")
})

test_that("get_os", {
  expect_true(get_os() %in% c("win", "mac", "unix", "unknown"))
})

test_that("user_cache_dir", {
  withr::local_envvar(R_PKG_CACHE_DIR = tmp <- basename(tempfile()))
  expect_equal(user_cache_dir(), tmp)

  withr::local_envvar(R_PKG_CACHE_DIR = NA_character_)
  withr::local_envvar(R_USER_CACHE_DIR = tmp <- basename(tempfile()))
  expect_equal(user_cache_dir("pak"), file_path(tmp, "R", "pak"))

  withr::local_envvar(R_USER_CACHE_DIR = NA_character_)
  mockery::stub(user_cache_dir, "get_os", "win")
  withr::local_envvar(LOCALAPPDATA = "windir")
  expect_equal(
    user_cache_dir("pak"),
    file_path("windir", "R", "Cache", "pak")
  )

  mockery::stub(user_cache_dir, "get_os", "mac")
  expect_equal(
    user_cache_dir("pak"),
    path.expand(
      file_path("~/Library/Caches", "org.R-project.R", "R", "pak")
    )
  )

  mockery::stub(user_cache_dir, "get_os", "unix")
  withr::local_envvar(XDG_CACHE_HOME = "unixdir")
  expect_equal(
    user_cache_dir("pak"),
    file_path("unixdir", "R", "pak")
  )
  withr::local_envvar(XDG_CACHE_HOME = NA_character_)
  expect_equal(
    user_cache_dir("pak"),
    path.expand(file_path("~/.cache", "R", "pak"))
  )

  mockery::stub(user_cache_dir, "get_os", "unknown")
  mockery::stub(user_cache_dir, "tempdir", "tempdir")
  expect_equal(
    user_cache_dir("pak"),
    file_path("tempdir", "r-pkg-cache", "pak")
  )
})

test_that("file_path", {
  expect_equal(
    file_path(c("foo", "bar", tmp <- basename(tempfile()))),
    normalizePath(file.path("foo", "bar", tmp), mustWork = FALSE)
  )
})

test_that("win_path_local", {
  withr::local_envvar(LOCALAPPDATA = "localappdatadir")
  withr::local_envvar(USERPROFILE = "userdir")
  expect_equal(win_path_local(), "localappdatadir")

  withr::local_envvar(LOCALAPPDATA = NA_character_)
  expect_equal(win_path_local(), file.path("userdir", "AppData", "Local"))

  withr::local_envvar(USERPROFILE = NA_character_)
  mockery::stub(win_path_local, "tempdir", "tempdir")
  expect_equal(win_path_local(), file.path("tempdir", "r-pkg-cache"))
})

test_that("cat0", {
  expect_snapshot({
    cat0(c("foo", "bar"), "foobar")
  })
})

test_that("mkdirp", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  dir <- file.path(tmp, "sub", "dir")
  expect_silent(mkdirp(dir))
  expect_true(dir.exists(dir))

  withr::local_dir(dir)
  expect_snapshot(
    mkdirp(c("foo", "bar"), "Created these")
  )
  expect_true(dir.exists("foo"))
  expect_true(dir.exists("bar"))
})

test_that("fix_macos_path_in_rstudio", {
  # This is to restore PATH at the end of the test case
  withr::local_path()

  get_path <- function() {
    strsplit(Sys.getenv("PATH"), .Platform$path.sep)[[1]]
  }

  mockery::stub(fix_macos_path_in_rstudio, "readLines", "extraaaa")

  withr::local_envvar(RSTUDIO = NA_character_)
  fix_macos_path_in_rstudio()
  expect_false("extraaaa" %in% get_path())

  withr::local_envvar(RSTUDIO = "1")
  mockery::stub(fix_macos_path_in_rstudio, "Sys.info", c(sysname = "Linux"))
  fix_macos_path_in_rstudio()
  expect_false("extraaaa" %in% get_path())

  mockery::stub(fix_macos_path_in_rstudio, "Sys.info", c(sysname = "Darwin"))
  mockery::stub(fix_macos_path_in_rstudio, "file.exists", FALSE)
  fix_macos_path_in_rstudio()
  expect_false("extraaaa" %in% get_path())

  mockery::stub(fix_macos_path_in_rstudio, "file.exists", TRUE)
  fix_macos_path_in_rstudio()
  expect_true("extraaaa" %in% get_path())
})

test_that("rimraf", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  mkdirp(file.path(tmp, "sub", "dir"))
  expect_true(dir.exists(file.path(tmp, "sub", "dir")))

  rimraf(file.path(tmp))
  expect_false(dir.exists(file.path(tmp, "sub", "dir")))
  expect_false(dir.exists(tmp))
})

test_that("is_interactive", {
  withr::local_options(rlib_interactive = TRUE)
  expect_true(is_interactive())

  withr::local_options(rlib_interactive = FALSE)
  expect_false(is_interactive())

  withr::local_options(rlib_interactive = NULL)
  withr::local_options(knitr.in.progress = TRUE)
  expect_false(is_interactive())

  withr::local_options(knitr.in.progress = NULL)
  withr::local_options(rstudio.notebook.executing = TRUE)
  expect_false(is_interactive())

  withr::local_options(rstudio.notebook.executing = NULL)
  withr::local_envvar(TESTTHAT = "true")
  expect_false(is_interactive())

  withr::local_envvar(TESTTHAT = NULL)
  mockery::stub(is_interactive, "interactive", FALSE)
  expect_false(is_interactive())

  mockery::stub(is_interactive, "interactive", TRUE)
  expect_true(is_interactive())
})

test_that("loaded_packages", {
  pkgs <- loaded_packages(.Library)
  expect_false("base" %in% pkgs)
})

test_that("lapply_with_names", {
  expect_equal(
    lapply_with_names(character(), identity),
    structure(list(), names = character())
  )
  expect_equal(
    lapply_with_names(letters, function(x) paste(x, x)),
    structure(as.list(paste(letters, letters)), names = letters)
  )
})

test_that("na_omit", {
  expect_equal(na_omit(integer()), integer())
  expect_equal(na_omit(letters), letters)
  expect_equal(na_omit(c(1, NA, 2)), c(1, 2))
})

test_that("base_packages", {
  expect_snapshot(
    base_packages()
  )
})

test_that("is_flag", {
  bad <- list(
    list(1),
    1L,
    logical(),
    c(TRUE, FALSE),
    NA
  )
  for (b in bad) expect_false(is_flag(b))

  expect_true(is_flag(TRUE))
  expect_true(is_flag(FALSE))
  expect_true(is_flag(c(x = TRUE)))
})

test_that("is_string", {
  bad <- list(
    list(1),
    1L,
    character(),
    c("foo", "bar"),
    NA_character_
  )
  for (b in bad) expect_false(is_string(b))

  expect_true(is_string("foo"))
  expect_true(is_string(c(x = "foo")))
})

test_that("map_named", {
  expect_equal(
    map_named(c(a = 1, b = 2), list),
    list(a = list("a", 1), b = list("b", 2))
  )
})

test_that("cisort", {
  vec <- c("abc", "ABD", "abE")
  expect_equal(cisort(sample(vec)), vec)
})

test_that("read_char", {
  local <- withr::local_tempdir()
  path <- file.path(local, "foo")
  file.create(path)
  expect_equal(read_char(path), "")
  writeLines(c("foo", "bar", "\u00f0", "foobar"), path)
  expect_equal(read_char(path), "foo\nbar\n\u00f0\nfoobar\n")
  expect_equal(Encoding(read_char(path)), "UTF-8")
})
