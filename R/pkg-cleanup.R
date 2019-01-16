
#' Clean up pkg caches and/or the pkg library
#'
#' @param package_cache Whether to clean up the cache of package files.
#' @param metadata_cache Whether to clean up the cache of package meta
#'   data.
#' @param pkg_lib Whethe to clean up the pkg package library.
#' @param force Do not ask for confirmation. Note that to use this function
#'   in non-interactive mode, you have to specify `force = FALSE`.
#'
#' @export

pkg_cleanup <- function(package_cache = TRUE, metadata_cache = TRUE,
                           pkg_lib = TRUE, force = FALSE) {

  if (!force && !interactive()) {
    stop("Refused to clean up, please specify `force = TRUE`")
  }

  if (package_cache) package_cache <- pkg_cleanup_package_cache(force)
  if (metadata_cache) metadata_cache <- pkg_cleanup_metadata_cache(force)
  if (pkg_lib) pkg_lib <- pkg_cleanup_lib(force)
  all <- package_cache && metadata_cache && pkg_lib

  if (all) {
    root <- user_cache_dir("R-pkg")
    if (length(dir(root)) == 0) unlink(root, recursive = TRUE)
  }

  invisible()
}

pkg_cleanup_package_cache <- function(force) {
  if (!force) {
    remote(
      function(...) {
        asNamespace("pkg")$pkg_cleanup_package_cache_print(...)
      })
    force <- get_confirmation2("? Do you want to remove it? (Y/n) ")
  }

  if (force) {
    remote(
      function(...) {
        asNamespace("pkg")$pkg_cleanup_package_cache2()
      })
  }
  force
}

pkg_cleanup_package_cache_print <- function() {
  sum <- pkgcache::pkg_cache_summary()
  size <- prettyunits::pretty_bytes(sum$size)
  cliapp::cli_alert(
    "{emph Package cache} is in {path {sum$cachepath}} ({size})")
}

pkg_cleanup_package_cache2 <- function() {
  sum <- pkgcache::pkg_cache_summary()
  unlink(sum$cachepath, recursive = TRUE)
  cliapp::cli_alert_success("Cleaned up package cache")
  invisible()
}

pkg_cleanup_metadata_cache <- function(force) {
  if (!force) {
    remote(
      function(...) {
        asNamespace("pkg")$pkg_cleanup_metadata_cache_print(...)
      })
    force <- get_confirmation2("? Do you want to remove it? (Y/n) ")
  }

  if (force) {
    remote(
      function(...) {
        asNamespace("pkg")$pkg_cleanup_metadata_cache2()
      })
  }
  force
}

pkg_cleanup_metadata_cache_print <- function() {
  sum <- pkgcache::meta_cache_summary()
  size <- prettyunits::pretty_bytes(sum$size)
  cliapp::cli_alert(
    "{emph Metadata cache} is in {path {sum$cachepath}} ({size})")
}

pkg_cleanup_metadata_cache2 <- function() {
  sum <- pkgcache::meta_cache_summary()
  unlink(sum$cachepath, recursive = TRUE)
  unlink(sum$lockfile, recursive = TRUE)
  cliapp::cli_alert_success("Cleaned up metadata cache")
  invisible()
}

pkg_cleanup_lib <- function(force) {
  if (!force) {
    remote(
      function(...) {
        asNamespace("pkg")$pkg_cleanup_lib_print(...)
      })
    force <- get_confirmation2("? Do you want to remove it? (Y/n) ")
  }

  if (force) {
    remote(
      function(...) {
        asNamespace("pkg")$pkg_cleanup_lib2()
      })
  }
  force
}

pkg_cleanup_lib_print <- function() {
  lib <- dirname(private_lib_dir())
  num <- viapply(dir(lib, full.names = TRUE), function(x) length(dir(x)))
  cliapp::cli_alert(
    "{emph pkg library} is in {path {lib}} ({num} packages)")
}

pkg_cleanup_lib2 <- function() {
  lib <- dirname(private_lib_dir())
  unlink(lib, recursive = TRUE)
  cliapp::cli_alert_success("Cleaned up pkg library")
  invisible()
}
