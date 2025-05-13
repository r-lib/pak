test_that("add_repo_links", {
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  dir.create(tmp <- tempfile())
  setwd(tmp)

  add_repo_links <- import_from("create_pak_repo", "add_repo_links")
  mkdirp <- import_from("create_pak_repo", "mkdirp")

  plt <- rbind(
    c("linux", "x86_64"),
    c("linux", "aarch64"),
    c("darwin15.6.0", "x86_64"),
    c("darwin17.0", "x86_64"),
    c("darwin20", "aarch64"),
    c("mingw32", "x86_64")
  )

  mkdirp(paste0("p", "/", "pak", "/", "stable", "/", plt[, 1], "/", plt[, 2]))

  for (i in seq_len(nrow(plt))) {
    path <- file.path("p/pak/stable", plt[i, 1], plt[i, 2], "PACKAGES")
    writeLines(
      path,
      text = c(
        "Package: pak",
        "Version: 1.0.0",
        paste0("Depends: ", plt[i, 1], "/", plt[i, 2])
      )
    )
  }

  add_repo_links("p/pak", "stable")

  # ----------------------------------------------------------------------
  # test the new repo form first: pkgtype/os/arch

  tsts <- read.table(
    stringsAsFactors = FALSE,
    header = TRUE,
    textConnection(
      "
    pkg_type                 os           arch    rver result
    source                   linux-gnu    x86_64  4.1  linux/x86_64
    source                   linux-musl   x86_64  4.1  linux/x86_64
    source                   linux-gnu    aarch64 4.1  linux/aarch64
    # Linux, unsupported arch
    source                   linux-gnu    s390x   4.1  NA
    win.binary               mingw32      x86_64  4.1  mingw32/x86_64
    # Arm Windows, unsupported
    win.binary               mingw32      aarch64 4.1  NA
    mac.binary.el-capitan    darwin15.6.0 x86_64  3.6  darwin15.6.0/x86_64
    mac.binary               darwin17.0   x86_64  4.1  darwin17.0/x86_64
    mac.binary.big-sur-arm64 darwin20     aarch64 4.1  darwin20/aarch64
    # Variations for Brew R, unsupported
    source                   darwin21.1.0 aarch64 4.1  NA
    source                   darwin21.1.0 x86_64  4.1  NA
    source                   darwin15.6.0 x86_64  3.6  NA
  "
    )
  )

  pref <- if (.Platform$OS.type == "windows") "file:///" else "file://"
  base <- paste0(pref, normalizePath("p/pak/stable"))
  repo <- paste0(base, "/", tsts$pkg_type, "/", tsts$os, "/", tsts$arch)
  for (i in seq_len(nrow(tsts))) {
    curl <- utils::contrib.url(repo[i], tsts$pkg_type[i])
    curl <- sub("[0-9]+[.][0-9]+$", tsts$rver[i], curl)
    pkgs <- suppressWarnings(tryCatch(
      if (getRversion() >= "3.5.0") {
        utils::available.packages(curl, ignore_repo_cache = TRUE)
      } else {
        utils::available.packages(curl)
      },
      error = function(err) NULL
    ))
    if (is.na(tsts$result[i])) {
      expect_null(pkgs)
    } else {
      expect_equal(pkgs[, "Depends"], tsts$result[i])
    }
  }

  # ----------------------------------------------------------------------
  # now test the old form, where we don't include the additional data

  tsts2 <- read.table(
    stringsAsFactors = FALSE,
    header = TRUE,
    textConnection(
      "
    pkg_type                  rver result
    source                    4.1  linux/x86_64
    win.binary                4.1  mingw32/x86_64
    mac.binary.el-capitan     3.6  darwin15.6.0/x86_64
    mac.binary                4.1  darwin17.0/x86_64
    mac.binary.big-sur-arm64  4.1  darwin20/aarch64
  "
    )
  )

  for (i in seq_len(nrow(tsts2))) {
    curl <- utils::contrib.url(base, tsts2$pkg_type[i])
    curl <- sub("[0-9]+[.][0-9]+$", tsts2$rver[i], curl)
    if (getRversion() >= "3.5.0") {
      pkgs <- utils::available.packages(curl, ignore_repo_cache = TRUE)
    } else {
      pkgs <- utils::available.packages(curl)
    }
    expect_equal(pkgs[, "Depends"], tsts2$result[i])
  }
})
