
#' Package cache utilities
#'
#' @description
#' Various utilities to inspect and clean the package cache.
#' See the pkgcache package if you need for control over the package cache.
#'
#' @details`cache_summary()` returns a summary of the package cache.
#'
#' @return `cache_summary()` returns a list with elements:
#' * `cachepath`: absolute path to the package cache
#' * `files`: number of files (packages) in the cache
#' * `size`: total size of package cache in bytes
#'
#' @export
#' @rdname cache
#' @section Examples:
#' ```{asciicast cache-summary}
#' cache_summary()
#' ```

cache_summary <- function() {
  remote(
    function(...) {
      get("cache_summary_internal", asNamespace("pak"))(...)
    },
    list()
  )
}

cache_summary_internal <- function() {
  pkgcache::pkg_cache_summary()
}

#' @details `cache_list()` lists all (by default), or a subset of
#' packages in the package cache.
#'
#' @param ... For `cache_list()` and `cache_delete()`, `...` may contain
#' filters, where the argument name is the column name. E.g. `package`,
#' `version`, etc. Call `cache_list()` without arguments to see the
#' available column names. If you call `cache_delete()` without arguments,
#' it will delete all cached files.
#'
#' @return `cache_list()` returns a data frame with the data about the cache.
#'
#' @rdname cache
#' @export
#' @section Examples:
#' ```{asciicast cache-list}
#' cache_list()
#' ```
#'
#' ```{asciicast cache-list-2}
#' cache_list(package = "recipes")
#' ```
#'
#' ```{asciicast cache-list-3}
#' cache_list(platform = "source")
#' ```

cache_list <- function(...) {
  load_extra("pillar")
  remote(
    function(...) {
      get("cache_list_internal", asNamespace("pak"))(...)
    },
    list(...)
  )
}

cache_list_internal <- function(...) {
  pkgcache::pkg_cache_find(...)
}

#' @details `cache_delete()` deletes files from the cache.
#'
#' @return `cache_delete()` returns nothing.
#' @export
#' @rdname cache
#' @section Examples:
#' ```{r cache-delete, eval = FALSE}
#' cache_delete(package = "knitr")
#' cache_delete(platform = "macos")
#' ```

cache_delete <- function(...) {
  remote(
    function(...) {
      get("cache_delete_internal", asNamespace("pak"))(...)
    },
    list(...)
  )
  invisible()
}

cache_delete_internal <- function(...) {
  pkgcache::pkg_cache_delete_files(...)
}

#' @details `cache_clean()` deletes all files from the cache.
#'
#' @return `cache_clean()` returns nothing.
#'
#' @export
#' @rdname cache
#' @section Examples:
#' ```{r cache-clean, eval = FALSE}
#' cache_clean()
#' ```

cache_clean <- function() {
  remote(
    function(...) {
      get("cache_clean_internal", asNamespace("pak"))(...)
    },
    list()
  )
  invisible()
}

cache_clean_internal <- function() {
  pkgcache::pkg_cache_delete_files()
}

#' Metadata cache utilities
#'
#' @description
#' Various utilities to inspect, update and clean the metadata cache.
#' See the pkgcache package if you need for control over the metadata cache.
#'
#' @details `meta_summary()` returns a summary of the metadata cache.
#'
#' @return `meta_summary()` returns a list with entries:
#' * `cachepath`: absolute path of the metadata cache.
#' * `current_db`: the file that contains the current metadata database.
#'   It is currently an RDS file, but this might change in the future.
#' * `raw_files`: the files that are the downloaded `PACKAGES*` files.
#' * `db_files`: all metadata database files.
#' * `size`: total size of the metadata cache.
#'
#' @export
#' @rdname metadata
#' @section Examples:
#' Metadata cache summary:
#'
#' ```{r meta-summary}
#' meta_summary()
#' ```

meta_summary <- function() {
  remote(
    function(...) {
      get("meta_summary_internal", asNamespace("pak"))(...)
    },
    list()
  )
}

meta_summary_internal <- function() {
  ret <- pkgcache::meta_cache_summary()
  list(
    cachepath = ret$cachepath,
    current_db = ret$current_rds,
    raw_files = ret$raw_files,
    db_files = ret$rds_files,
    size = ret$size
  )
}

#' @details `meta_list()` lists all (or some) packages in the metadata
#' database.
#'
#' @param pkg Package names, if specified then only entries for `pkg`
#' are returned.
#' @return `meta_list()` returns a data frame of all available
#' packages in the configured repositories.
#'
#' @export
#' @rdname metadata
#' @section Examples:
#' The current metadata DB:
#' ```{asciicast meta-list}
#' meta_list()
#' ```
#'
#' Selected packages only:
#' ```{asciicast meta-list-2}
#' meta_list(pkg = c("shiny", "htmlwidgets"))
#' ```

meta_list <- function(pkg = NULL) {
  load_extra("pillar")
  remote(
    function(...) {
      get("meta_list_internal", asNamespace("pak"))(...)
    },
    list(pkg = pkg)
  )
}

meta_list_internal <- function(pkg) {
  pkgcache::meta_cache_list(packages = pkg)
}

#' @details `meta_update()` updates the metadata database. You don't
#' normally need to call this function manually, because all pak functions
#' (e.g. [pkg_install()], [pkg_download()], etc.) call it automatically,
#' to make sure that they use the latest available metadata.
#'
#' @return `meta_update()` returns nothing.
#'
#' @export
#' @rdname metadata
#' @section Examples:
#' Update the metadata DB
#' ```{asciicast meta-update}
#' meta_update()
#' ```

meta_update <- function() {
  remote(
    function(...) {
      get("meta_update_internal", asNamespace("pak"))(...)
    },
    list()
  )
  invisible()
}

meta_update_internal <- function() {
  pkgcache::meta_cache_update()
}

#' @details `meta_clean()` deletes the whole metadata DB.
#'
#' @param force If `FALSE`, then pak will ask for confirmation.
#' @return `meta_clean()` returns nothing
#'
#' @export
#' @rdname metadata
#' @section Examples:
#' Delete the metadata DB
#' ```{asciicast meta-clean}
#' meta_clean()
#' ```

meta_clean <- function(force = FALSE) {
  if (!force) {
    force <- get_confirmation2(
      "? Do you want to delete all package metadata (Y/n) "
    )
  }
  if (!force) {
    msg("x Metadata cleanup aborted")
    return(invisible())
  }

  remote(
    function(...) {
      get("meta_clean_internal", asNamespace("pak"))(...)
    },
    list()
  )
  invisible()
}

meta_clean_internal <- function() {
  pkgcache::meta_cache_cleanup(force = TRUE)
}
