
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

    tsts <- read.table(stringsAsFactors = FALSE, header = TRUE, textConnection("
      pkg_type                 rver
      mac.binary.big-sur-arm64 4.1.2
      mac.binary.big-sur-arm64 4.2.0
      mac.binary.el-capitan    3.4.4
      mac.binary.el-capitan    3.5.3
      mac.binary.el-capitan    3.6.3
      mac.binary               4.0.5
      mac.binary               4.1.2
      mac.binary               4.2.0
      win.binary               3.4.4
      win.binary               3.5.3
      win.binary               3.6.3
      win.binary               4.0.5
      win.binary               4.1.2
      win.binary               4.2.0
      source                   3.4.4
      source                   3.5.3
      source                   3.6.3
      source                   4.0.5
      source                   4.1.2
      source                   4.2.0
    "))

    for (i in seq_len(nrow(tsts))) {
      cu <- get_curl(repo, tsts$pkg_type[i], tsts$rver[i])
      av <- available.packages(cu, filters = list(), ignore_repo_cache = TRUE)
      mockery::stub(rver_flt, "getRversion", package_version(tsts$rver[i]))
      res <- rver_flt(av)
      expect_equal(nrow(res), 1L)
    }
  }
})

test_that("New URL", {

})
