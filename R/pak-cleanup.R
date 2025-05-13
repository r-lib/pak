#' Clean up pak caches
#'
#' @param package_cache Whether to clean up the cache of package files.
#' @param metadata_cache Whether to clean up the cache of package meta
#'   data.
#' @param pak_lib This argument is now deprecated and does nothing.
#' @param force Do not ask for confirmation. Note that to use this function
#'   in non-interactive mode, you have to specify `force = TRUE`.
#'
#' @export
#' @family pak housekeeping

pak_cleanup <- function(
  package_cache = TRUE,
  metadata_cache = TRUE,
  pak_lib = TRUE,
  force = FALSE
) {
  if (!force && !interactive()) {
    stop("Refused to clean up, please specify `force = TRUE`")
  }

  if (package_cache) package_cache <- pak_cleanup_package_cache(force)
  if (metadata_cache) metadata_cache <- pak_cleanup_metadata_cache(force)
  all <- package_cache && metadata_cache

  invisible()
}

pak_cleanup_package_cache <- function(force) {
  if (!force) {
    remote(
      function(...) {
        asNamespace("pak")$pak_cleanup_package_cache_print(...)
      }
    )
    force <- get_confirmation2("? Do you want to remove it? (Y/n) ")
  }

  if (force) {
    remote(
      function(...) {
        asNamespace("pak")$pak_cleanup_package_cache2()
      }
    )
  }
  force
}

pak_cleanup_package_cache_print <- function() {
  sum <- pkgcache::pkg_cache_summary()
  size <- format_bytes$pretty_bytes(sum$size)
  cli::cli_alert(
    "{.emph Package cache} is in {.path {sum$cachepath}} ({size})"
  )
}

pak_cleanup_package_cache2 <- function() {
  sum <- pkgcache::pkg_cache_summary()
  unlink(sum$cachepath, recursive = TRUE)
  root <- dirname(sum$cachepath)
  if (length(dir(root)) == 0) unlink(root, recursive = TRUE)
  cli::cli_alert_success("Cleaned up package cache")
  invisible()
}

pak_cleanup_metadata_cache <- function(force) {
  if (!force) {
    remote(
      function(...) {
        asNamespace("pak")$pak_cleanup_metadata_cache_print(...)
      }
    )
    force <- get_confirmation2("? Do you want to remove it? (Y/n) ")
  }

  if (force) {
    remote(
      function(...) {
        asNamespace("pak")$pak_cleanup_metadata_cache2()
      }
    )
  }
  force
}

pak_cleanup_metadata_cache_print <- function() {
  sum <- pkgcache::meta_cache_summary()
  size <- format_bytes$pretty_bytes(sum$size)
  cli::cli_alert(
    "{.emph Metadata cache} is in {.path {sum$cachepath}} ({size})"
  )
}

pak_cleanup_metadata_cache2 <- function() {
  sum <- pkgcache::meta_cache_summary()
  unlink(sum$cachepath, recursive = TRUE)
  unlink(sum$lockfile, recursive = TRUE)
  root <- dirname(sum$cachepath)
  if (length(dir(root)) == 0) unlink(root, recursive = TRUE)
  cli::cli_alert_success("Cleaned up metadata cache")
  invisible()
}
