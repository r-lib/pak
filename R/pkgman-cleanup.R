
#' Clean up pkgman caches and/or the pkgman library
#'
#' @param package_cache Whether to clean up the cache of package files.
#' @param metadata_cache Whether to clean up the cache of package meta
#'   data.
#' @param pkgman_lib Whethe to clean up the pkgman package library.
#' @param force Do not ask for confirmation. Note that to use this function
#'   in non-interactive mode, you have to specify `force = FALSE`.
#'
#' @export

pkgman_cleanup <- function(package_cache = TRUE, metadata_cache = TRUE,
                           pkgman_lib = TRUE, force = FALSE) {

  if (!force && !interactive()) {
    stop("Refused to clean up, please specify `force = TRUE`")
  }

  all <- package_cache && metadata_cache && pkgman_lib
  if (package_cache)  all <- all && pkgman_cleanup_package_cache(force)
  if (metadata_cache) all <- all && pkgman_cleanup_metadata_cache(force)
  if (pkgman_lib)     all <- all && pkgman_cleanup_lib(force)

  if (all) {
    root <- user_cache_dir("R-pkg")
    if (length(dir(root)) == 0) unlink(root, recursive = TRUE)
  }

  invisible()
}

pkgman_cleanup_package_cache <- function(force) {
  if (!force) {
    remote(
      function(...) {
        asNamespace("pkgman")$pkgman_cleanup_package_cache_print(...)
      })
    force <- get_confirmation2("? Do you want to remove it? (Y/n) ")
  }

  if (force) {
    remote(
      function(...) {
        asNamespace("pkgman")$pkgman_cleanup_package_cache2()
      })
  }
  force
}

pkgman_cleanup_package_cache_print <- function() {
  sum <- pkgcache::pkg_cache_summary()
  size <- prettyunits::pretty_bytes(sum$size)
  cliapp::cli_alert(
    "{emph Package cache} is in {path {sum$cachepath}} ({size})")
}

pkgman_cleanup_package_cache2 <- function() {
  sum <- pkgcache::pkg_cache_summary()
  unlink(sum$cachepath, recursive = TRUE)
  cliapp::cli_alert_success("Cleaned up package cache")
  invisible()
}

pkgman_cleanup_metadata_cache <- function(force) {
  if (!force) {
    remote(
      function(...) {
        asNamespace("pkgman")$pkgman_cleanup_metadata_cache_print(...)
      })
    force <- get_confirmation2("? Do you want to remove it? (Y/n) ")
  }

  if (force) {
    remote(
      function(...) {
        asNamespace("pkgman")$pkgman_cleanup_metadata_cache2()
      })
  }
  force
}

pkgman_cleanup_metadata_cache_print <- function() {
  sum <- pkgcache::meta_cache_summary()
  size <- prettyunits::pretty_bytes(sum$size)
  cliapp::cli_alert(
    "{emph Metadata cache} is in {path {sum$cachepath}} ({size})")
}

pkgman_cleanup_metadata_cache2 <- function() {
  sum <- pkgcache::meta_cache_summary()
  unlink(sum$cachepath, recursive = TRUE)
  unlink(sum$lockfile, recursive = TRUE)
  cliapp::cli_alert_success("Cleaned up metadata cache")
  invisible()
}

pkgman_cleanup_lib <- function(force) {
  if (!force) {
    remote(
      function(...) {
        asNamespace("pkgman")$pkgman_cleanup_lib_print(...)
      })
    force <- get_confirmation2("? Do you want to remove it? (Y/n) ")
  }

  if (force) {
    remote(
      function(...) {
        asNamespace("pkgman")$pkgman_cleanup_lib2()
      })
  }
  force
}

pkgman_cleanup_lib_print <- function() {
  lib <- dirname(private_lib_dir())
  num <- viapply(dir(lib, full.names = TRUE), function(x) length(dir(x)))
  cliapp::cli_alert(
    "{emph pkgman library} is in {path {lib}} ({num} packages)")
}

pkgman_cleanup_lib2 <- function() {
  lib <- dirname(private_lib_dir())
  unlink(lib, recursive = TRUE)
  cliapp::cli_alert_success("Cleaned up pkgman library")
  invisible()
}
