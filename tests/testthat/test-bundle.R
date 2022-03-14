
test_that("do not embed on check, except windows", {
  skip_on_cran()
  lib <- private_lib_dir()
  win <- .Platform$OS.type == "windows"
  chk <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  load <- !is.null(asNamespace("pak")$.__DEVTOOLS__)
  rmts <- "Remotes" %in% names(packageDescription("pak"))

  mthd <- pak_sitrep_data$method
  if ( win && !chk && !load && !rmts) expect_equal(mthd, "download") # !L !R  W
  if ( win && !chk && !load &&  rmts) expect_equal(mthd, "none")     # !L  R
  if ( win && !chk &&  load && !rmts) expect_equal(mthd, "copy")     # L
  if ( win && !chk &&  load &&  rmts) expect_equal(mthd, "copy")     # L

  if ( win &&  chk && !load && !rmts) expect_equal(mthd, "download") # !L !R  W
  if ( win &&  chk && !load &&  rmts) expect_equal(mthd, "none")     # !L  R
  if ( win &&  chk &&  load && !rmts) expect_equal(mthd, "copy")     # L
  if ( win &&  chk &&  load &&  rmts) expect_equal(mthd, "copy")     # L

  if (!win && !chk && !load && !rmts) {                              # !L !R !W !C
    # chk is not detected properly on R 3.4.x
    if (getRversion() >= "3.5.0") expect_equal(mthd, "download")
  }
  if (!win && !chk && !load &&  rmts) expect_equal(mthd, "none")     # !L  R
  if (!win && !chk &&  load && !rmts) expect_equal(mthd, "copy")     # L
  if (!win && !chk &&  load &&  rmts) expect_equal(mthd, "copy")     # L

  if (!win &&  chk && !load && !rmts) expect_equal(mthd, "none")     # !L !R !W  C
  if (!win &&  chk && !load &&  rmts) expect_equal(mthd, "none")     # !L  R
  if (!win &&  chk &&  load && !rmts) expect_equal(mthd, "copy")     # L
  if (!win &&  chk &&  load &&  rmts) expect_equal(mthd, "copy")     # L

  if (mthd == "download") {
    expect_equal(names(lib), "embedded")
    expect_false(pak_sitrep_data$failed)
  }
})

test_package_root <- function() {
  x <- tryCatch(
    rprojroot::find_package_root_file(),
    error = function(e) NULL)

  if (!is.null(x)) return(x)

  pkg <- testthat::testing_package()
  x <- tryCatch(
    rprojroot::find_package_root_file(
      path = file.path("..", "..", "00_pkg_src", pkg)),
    error = function(e) NULL)

  if (!is.null(x)) return(x)

  stop("Cannot find package root")
}

test_that("bundle_deps", {
  skip_on_cran()
  skip_on_covr()
  pkg <- test_package_root()
  bnd <- eval(parse(file.path(pkg, "data", "pak_sitrep_data.R")))
  deps <- bnd$bundle_deps()
  dsc <- desc::desc(pkg)
  # seems like a useless test case...
  std <- function(x) unname(gsub("[ \n]", "", x))
  expect_equal(
    std(deps$deps),
    std(dsc$get("Config/needs/dependencies"))
  )
})

test_that("bundle_download", {
  skip_on_cran()
  skip_on_covr()
  rmts <- "Remotes" %in% names(packageDescription("pak"))
  if (rmts) skip("Has Remotes")

  pkg <- test_package_root()
  bnd <- eval(parse(file.path(pkg, "data", "pak_sitrep_data.R")))

  mkdirp(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  withr::local_envvar(PAK_LIBRARY_DIR = lib)

  bnd$bundle_download()

  deps <- parse_dep_fields(bnd$bundle_deps()$deps)
  expect_true(all(deps %in% dir(lib)))
})

test_that("bundle_download with local repo", {
  skip_on_cran()
  skip_on_covr()
  rmts <- "Remotes" %in% names(packageDescription("pak"))
  if (rmts) skip("Has Remotes")

  pkg <- test_package_root()
  bnd <- eval(parse(file.path(pkg, "data", "pak_sitrep_data.R")))

  mkdirp(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  mkdirp(repo <- tempfile())
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)
  types <- unique(c("source", .Platform$pkgType))

  pkgs <- parse_dep_fields(bnd$bundle_deps()$deps)

  for (type in types) {
    pkgdir <- paste0(repo, contrib.url(repos = "", type = type))
    mkdirp(pkgdir)
    utils::download.packages(
      pkgs, pkgdir, type = type, quiet = TRUE, repos = default_cran_mirror()
    )
    ptype <- type
    if (grepl("^mac.binary", type)) ptype <- "mac.binary"
    tools::write_PACKAGES(pkgdir, type = ptype)
  }

  proto <- if (.Platform$OS.type == "windows") {
    "file:///"
  } else {
    "file://"
  }
  withr::local_options(repos = c(CRAN = paste0(proto, normalizePath(repo, "/"))))
  withr::local_envvar(PAK_LIBRARY_DIR = lib)
  bnd$bundle_download()

  expect_true(all(pkgs %in% dir(lib)))
})
