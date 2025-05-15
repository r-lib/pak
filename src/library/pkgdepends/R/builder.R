#' Create a binary package from an installed package
#'
#' The built package will be in the current working directory.
#'
#' This function is currently experimental.
#'
#' @param pkg Package name.
#' @param library Library path.
#' @param flavor Platform flavor. Defaults to the `PKG_BUILD_FLAVOR`
#'   environment variable. If not `NULL` or an empty string, then it is
#'   appended to the platform string with a dash.
#' @param build_number An integer number that is added to the file name,
#'   after the version number, to be able to have multiple builds for the
#'   same package version.
#' @return Path to the built package.
#'
#' @export
#' @keywords internal

pkg_build <- function(
  pkg,
  library = .libPaths()[1],
  flavor = Sys.getenv("PKG_BUILD_FLAVOR"),
  build_number = 1L
) {
  pkgdir <- file.path(library, pkg)
  if (!dir.exists(pkgdir)) {
    throw(pkg_error(
      "Cannot find package {.pkg {pkg}} in library at {.path {library}}."
    ))
  }
  platform <- pkgcache::current_r_platform()
  if (nzchar(flavor %||% "")) {
    platform <- paste0(platform, "-", flavor)
  }
  meta <- c(
    RemoteBuildPlatform = platform,
    GraphicsAPIVersion = pkgcache::get_graphics_api_version(),
    InternalsId = pkgcache::get_internals_id()
  )
  add_metadata(pkgdir, meta)
  dsc <- desc::desc(file = pkgdir)
  version <- dsc$get_field("Version")
  rversion <- get_minor_r_version(getRversion())

  sys <- sysname()
  if (sys == "windows") {
    install_md5_sums(pkg)
    fn <- paste0(
      pkg,
      "_",
      version,
      "_",
      "b",
      build_number,
      "_",
      "R",
      rversion,
      if (nzchar(flavor %||% "")) paste0("_", flavor),
      ".zip"
    )
    zip::zip(fn, pkgdir, mode = "cherry-pick")
  } else {
    ext <- if (sys == "mac") ".tgz" else ".tar.gz"
    fn <- paste0(
      pkg,
      "_",
      version,
      "_",
      "b",
      build_number,
      "_",
      "R",
      rversion,
      "_",
      platform,
      ext
    )
    ffn <- file.path(normalizePath("."), fn)
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(dirname(pkgdir))
    utils::tar(ffn, pkg, compression = "gzip", compression_level = 9)
  }

  fn
}

install_md5_sums <- function(pkgdir) {
  old <- getwd()
  on.exit(setwd(old), add = TRUE)

  setwd(pkgdir)
  fns <- setdiff(dir(".", recursive = TRUE), "MD5")
  md5 <- cli::hash_file_md5(fns)
  writeLines(
    paste0(md5, " *", fns),
    "MD5"
  )
}
