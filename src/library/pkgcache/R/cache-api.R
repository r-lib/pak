#' Functions to query and manipulate the package cache
#'
#' `pkg_cache_summary()` returns a short summary of the state of the cache,
#' e.g. the number of files and their total size. It returns a named list.
#'
#' @param cachepath Path of the cache. By default the cache directory is in
#'   `pkgcache`, within the user's cache directory.
#'   See `tools::R_user_dir()`.
#'
#' @seealso The [package_cache] R6 class for a more flexible API.
#' @rdname pkg_cache_api
#' @export
#' @examplesIf FALSE
#' pkg_cache_summary()
#' pkg_cache_list()
#' pkg_cache_find(package = "forecast")
#' tmp <- tempfile()
#' pkg_cache_get_file(target = tmp, package = "forecast", version = "8.10")
#' pkg_cache_delete_files(package = "forecast")

pkg_cache_summary <- function(cachepath = NULL) {
  cachepath <- cachepath %||% get_user_cache_dir()$pkg
  l <- pkg_cache_list(cachepath)
  size <- sum(file.info(l$fullpath)$size)
  list(
    cachepath = cachepath,
    files = nrow(l),
    size = size
  )
}

#' `pkg_cache_list()` lists all files in the cache. It returns a data frame.
#'
#' @rdname pkg_cache_api
#' @export

pkg_cache_list <- function(cachepath = NULL) {
  cachepath <- cachepath %||% get_user_cache_dir()$pkg
  as_data_frame(package_cache$new(cachepath)$list())
}

#' `pkg_cache_find()` finds all files in the cache that match the specified
#' attributes. It returns a data frame.
#'
#' @param ... Extra named arguments to select the package file.
#' @rdname pkg_cache_api
#' @export

pkg_cache_find <- function(cachepath = NULL, ...) {
  as_data_frame(pkg_cache_get_file(cachepath, target = NULL, ...))
}

#' `pkg_cache_get_file()` copied a file out of the cache into the specified
#' path. If no file is found, then it returns `NULL`. Otherwise it returns
#' (invisibly) the data frame of all selected files. If multiple
#' files match the specified attributes, then the first one is copied to
#' the `target` path.
#'
#' @param target Path where the selected file is copied.
#' @rdname pkg_cache_api
#' @export

pkg_cache_get_file <- function(cachepath = NULL, target, ...) {
  cachepath <- cachepath %||% get_user_cache_dir()$pkg
  invisible(as_data_frame(package_cache$new(cachepath)$copy_to(target, ...)))
}

#' `pkg_cache_delete_files()` deletes the selected files from the caches.
#' Without any arguments, all files are deleted.
#' @rdname pkg_cache_api
#' @export

pkg_cache_delete_files <- function(cachepath = NULL, ...) {
  cachepath <- cachepath %||% get_user_cache_dir()$pkg
  package_cache$new(cachepath)$delete(...)
}

#' `pkg_cache_add_file` adds a file to the cache.
#' @param file File to add.
#' @param relpath The relative path of the file within the cache.
#' @rdname pkg_cache_api
#' @export

pkg_cache_add_file <- function(
  cachepath = NULL,
  file,
  relpath = dirname(file),
  ...
) {
  cachepath <- cachepath %||% get_user_cache_dir()$pkg
  package_cache$new(cachepath)$add(file = file, path = relpath, ...)
}
