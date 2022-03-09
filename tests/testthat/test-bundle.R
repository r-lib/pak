
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

  if (!win && !chk && !load && !rmts) expect_equal(mthd, "download") # !L !R !W !C
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
