pak_sitrep_data <- local({
  pkgdir <- Sys.getenv("R_PACKAGE_DIR", "")

  rot1 <- function(x) {
    as.integer(charToRaw(as.character(x))) + 1L
  }

  if (pkgdir != "") {
    desc <- file.path(pkgdir, "DESCRIPTION")
    dcf <- read.dcf(desc)
    remotes <- if ("Remotes" %in% colnames(dcf)) dcf[, "Remotes"]
  } else {
    dcf <- utils::packageDescription("pak")
    remotes <- dcf$Remotes
  }

  bundledata <- list(
    pkgdir = rot1(Sys.getenv("R_PACKAGE_DIR", "")),
    rpkgname = rot1(Sys.getenv("R_PACKAGE_NAME", "")),
    devtools = !is.null(asNamespace("pak")$.__DEVTOOLS__),
    ghworkflow = rot1(Sys.getenv("GITHUB_WORKFLOW", "")),
    shost = rot1(Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_", "")),
    remotes = if (!is.null(remotes)) rot1(remotes)
  )

  list(
    platform = if (target_platform != "") {
      target_platform
    } else {
      R.Version()$platform
    },
    build_platform = if (build_platform != "") {
      build_platform
    } else {
      R.Version()$platform
    },
    build_platform_arg = build_platform,
    target_platform_arg = target_platform,
    "github-repository" = Sys.getenv("GITHUB_REPOSITORY", "-"),
    "github-sha" = Sys.getenv("GITHUB_SHA", "-"),
    "github-ref" <- Sys.getenv("GITHUB_REF", "-"),
    bundledata = bundledata
  )
})
