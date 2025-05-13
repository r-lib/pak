if (Sys.getenv("NOT_CRAN") != "true") return()

if (Sys.getenv("PAK_TESTS") != "true") return()

repo_url <- Sys.getenv(
  "PAK_TEST_REPO_URL",
  "https://raw.githubusercontent.com/r-lib/r-lib.github.io/test/p/pak/"
)

branches <- c("stable", "rc", "devel")

get_curl <- function(repo, type, rver) {
  url <- utils::contrib.url(repo, type)
  rver <- package_version(rver)[, 1:2]
  sub("[0-9]+[.][0-9]+$", rver, url)
}

test_that("Old URL", {
  for (br in branches) {
    rver_flt <- utils:::available_packages_filters_db$R_version
    repo <- paste0(repo_url, "/", br)

    tsts <- read.table(
      stringsAsFactors = FALSE,
      header = TRUE,
      textConnection(
        "
      pkg_type                 rver
      mac.binary.big-sur-arm64 4.1.2
      mac.binary.big-sur-arm64 4.2.3
      mac.binary.big-sur-arm64 4.3.1
      mac.binary.big-sur-arm64 4.4.0
      mac.binary.el-capitan    3.5.3
      mac.binary.el-capitan    3.6.3
      mac.binary               4.0.5
      mac.binary               4.1.2
      mac.binary               4.2.3
      mac.binary               4.3.1
      mac.binary               4.4.0
      win.binary               3.5.3
      win.binary               3.6.3
      win.binary               4.0.5
      win.binary               4.1.2
      win.binary               4.2.3
      win.binary               4.3.1
      win.binary               4.4.0
      source                   3.5.3
      source                   3.6.3
      source                   4.0.5
      source                   4.1.2
      source                   4.2.3
      source                   4.3.1
      source                   4.4.0
    "
      )
    )

    for (i in seq_len(nrow(tsts))) {
      cu <- get_curl(repo, tsts$pkg_type[i], tsts$rver[i])
      av <- available.packages(cu, filters = list(), ignore_repo_cache = TRUE)
      fake(rver_flt, "getRversion", package_version(tsts$rver[i]))
      res <- rver_flt(av)

      expect_equal(nrow(res), 1L)
      if (nrow(res) != 1) next

      pkgurl <- paste0(res[, "Repository"], "/", res[, "File"])
      hd <- curlGetHeaders(pkgurl)
      expect_equal(attr(hd, "status"), 200L)
    }
  }
})

test_that("New URL", {
  for (br in branches) {
    rver_flt <- utils:::available_packages_filters_db$R_version

    tsts <- read.table(
      stringsAsFactors = FALSE,
      header = TRUE,
      textConnection(
        "
      pkg_type                 os           arch    rver  branch
      source                   linux-gnu    x86_64  3.5.3 all
      source                   linux-gnu    x86_64  3.6.3 all
      source                   linux-gnu    x86_64  4.0.5 all
      source                   linux-gnu    x86_64  4.1.2 all
      source                   linux-gnu    x86_64  4.2.3 all
      source                   linux-gnu    x86_64  4.3.1 all
      source                   linux-gnu    x86_64  4.4.0 all
      source                   linux-musl   x86_64  4.1.2 all
      source                   linux-gnu    aarch64 4.1.2 devel
      win.binary               mingw32      x86_64  3.5.3 all
      win.binary               mingw32      x86_64  3.6.3 all
      win.binary               mingw32      x86_64  4.0.5 all
      win.binary               mingw32      x86_64  4.1.2 all
      win.binary               mingw32      x86_64  4.2.3 all
      win.binary               mingw32      x86_64  4.3.1 all
      win.binary               mingw32      x86_64  4.4.0 all
      mac.binary.el-capitan    darwin15.6.0 x86_64  3.5.3 all
      mac.binary.el-capitan    darwin15.6.0 x86_64  3.6.3 all
      mac.binary               darwin17.0   x86_64  4.0.5 all
      mac.binary               darwin17.0   x86_64  4.1.2 all
      mac.binary               darwin17.0   x86_64  4.2.3 all
      mac.binary               darwin17.0   x86_64  4.3.1 all
      mac.binary               darwin17.0   x86_64  4.4.0 all
      mac.binary.big-sur-arm64 darwin20     aarch64 4.1.2 all
      mac.binary.big-sur-arm64 darwin20     aarch64 4.2.3 all
      mac.binary.big-sur-arm64 darwin20     aarch64 4.3.1 all
      mac.binary.big-sur-arm64 darwin20     aarch64 4.4.0 all
    "
      )
    )

    for (i in seq_len(nrow(tsts))) {
      if (br != tsts$branch[i] && tsts$branch[i] != "all") next

      repo <- paste0(
        repo_url,
        "/",
        br,
        "/",
        tsts$pkg_type[i],
        "/",
        tsts$os[i],
        "/",
        tsts$arch[i]
      )

      cu <- get_curl(repo, tsts$pkg_type[i], tsts$rver[i])
      av <- available.packages(cu, filters = list(), ignore_repo_cache = TRUE)
      fake(rver_flt, "getRversion", package_version(tsts$rver[i]))
      res <- rver_flt(av)

      expect_equal(nrow(res), 1L)
      if (nrow(res) != 1) next

      pkgurl <- paste0(res[, "Repository"], "/", res[, "File"])
      hd <- curlGetHeaders(pkgurl)
      expect_equal(attr(hd, "status"), 200L)
    }
  }
})
