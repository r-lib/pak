
local({
  pkgdir <- Sys.getenv("R_PACKAGE_DIR", "")
  # This is not set in `devtools::load_all()`
  if (pkgdir != "") {
    path <- file.path(pkgdir, "pak-version.dcf")
    data <- c(
      platform = R.Version()$platform,
      "github-repository" = Sys.getenv("GITHUB_REPOSITORY", "-"),
      "github-sha" = Sys.getenv("GITHUB_SHA", "-"),
      "github-ref" = Sys.getenv("GITHUB_REF", "-")
    )
    write.dcf(rbind(data), path)
  }
})
