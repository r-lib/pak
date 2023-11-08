#' Cache for CRAN archive data
#'
#' This is an R6 class that implements a cache from older CRAN package
#' versions. For a higher level interface see the functions documented
#' with [cran_archive_list()].
#'
#' The cache is similar to [cranlike_metadata_cache] and has the following
#' layers:
#' * The data inside the `cran_archive_cache` object.
#' * Cached data in the current R session.
#' * An RDS file in the current session's temporary directory.
#' * An RDS file in the user's cache directory.
#'
#' It has a synchronous and an asynchronous API.
#'
#' @section Usage:
#' ```
#' cac <- cran_archive_cache$new(
#'   primary_path = NULL,
#'   replica_path = tempfile(),
#'   cran_mirror = default_cran_mirror(),
#'   update_after = as.difftime(7, units = "days"),
#' )
#'
#' cac$list(packages = NULL, update_after = NULL)
#' cac$async_list(packages = NULL, update_after = NULL)
#'
#' cac$update()
#' cac$async_update()
#'
#' cac$check_update()
#' cac$async_check_update()
#'
#' cac$summary()
#'
#' cac$cleanup(force = FALSE)
#'
#' @section Arguments:
#' * `primary_path`: Path of the primary, user level cache. Defaults to
#'   the user level cache directory of the machine.
#' * `replica_path`: Path of the replica. Defaults to a temporary directory
#'   within the session temporary directory.
#' * `cran_mirror`: CRAN mirror to use, this takes precedence over `repos`.
#' * `update_after`: `difftime` object. Automatically update the cache if
#'   it gets older than this. Set it to `Inf` to avoid updates. Defaults
#'   to seven days.
#' * `packages`: Packages to query, character vector.
#' * `force`: Whether to force cleanup without asking the user.
#'
#' @section Details:
#'
#' Create a new archive cache with `cran_archive_cache$new()`. Multiple
#' caches are independent, so e.g. if you update one of them, the other
#' existing caches are not affected.
#'
#' `cac$list()` lists the versions of the specified packages, or all
#' packages, if none were specified. `cac$async_list()` is the same, but
#' asynchronous.
#'
#' `cac$update()` updates the cache. It always downloads the new metadata.
#' `cac$async_update()` is the same, but asynchronous.
#'
#' `cac$check_update()` updates the cache if there is a newer version
#' available. `cac$async_check_update()` is the same, but asynchronous.
#'
#' `cac$summary()` returns a summary of the archive cache, a list with
#' entries:
#' * `cachepath`: path to the directory of the main archive cache,
#' * `current_rds`: the RDS file that stores the cache. (This file might
#'   not exist, if the cache is not downloaded yet.)
#' * `lockfile`: the file used for locking the cache.
#' * `timestamp: time stamp for the last update of the cache.
#' * `size`: size of the cache file in bytes.
#'
#' `cac$cleanup()` cleans up the cache files.
#'
#' @section Columns:
#' `cac$list()` returns a data frame with columns:
#'   * `package`: package name,
#'   * `version`: package version. This is a character vector, and not
#'      a [package_version()] object. Some older package versions are not
#'      supported by [package_version()].
#'   * `raw`: the raw row names from the CRAN metadata.
#'   * `mtime`: `mtime` column from the CRAN metadata. This is usually
#'      pretty close to the release date and time of the package.
#'   * `url`: package download URL.
#'   * `mirror`: CRAN mirror that was used to get this data.
#'
#' @export
#' @examplesIf pkgcache:::run_examples()
#' arch <- cran_archive_cache$new()
#' arch$update()
#' arch$list()

cran_archive_cache <- R6Class(
  "cran_archive_cache",

  public = list(
    initialize = function(primary_path = NULL,
                          replica_path = tempfile(),
                          cran_mirror = default_cran_mirror(),
                          update_after = as.difftime(7, units = "days"))
      cac_init(self, private, primary_path, replica_path, cran_mirror,
               update_after),

    list = function(packages = NULL, update_after = NULL)
      synchronise(self$async_list(packages, update_after)),
    async_list = function(packages = NULL, update_after = NULL)
      cac_async_list(self, private, packages,
                     update_after %||% private$update_after),

    update = function()
      synchronise(self$async_update()),
    async_update = function()
      cac_async_update(self, private),

    check_update = function()
      synchronise(self$async_check_update()),
    async_check_update = function()
      cac_async_check_update(self, private),

    summary = function()
      cac_summary(self, private),

    cleanup = function(force = FALSE)
      cac_cleanup(self, private, force)
  ),

  private = list(
    get_hash = function() {
      cli::hash_obj_md5(list(private$cran_mirror, private$cache_version))
    },
    get_cache_file = function(which = c("primary", "replica"))
      cac__get_cache_file(self, private, match.arg(which)),

    async_ensure_cache = function(max_age = private$update_after)
      cac__async_ensure_cache(self, private, max_age),

    get_current_data = function(max_age)
      cac__get_current_data(self, private, max_age),
    get_memory_cache = function(max_age)
      cac__get_memory_cache(self, private, max_age),
    load_replica = function(max_age)
      cac__load_replica(self, private, max_age),
    load_primary = function(max_age)
      cac__load_primary(self, private, max_age),

    update_memory_cache = function()
      cac__update_memory_cache(self, private),
    update_replica = function()
      cac__update_replica(self, private),
    update_primary = function(lock = TRUE)
      cac__update_primary(self, private, lock),
    convert_archive_file = function(raw, out)
      cac__convert_archive_file(self, private, raw, out),

    cache_version = "1",

    data = NULL,
    data_time = NULL,

    update_deferred = NULL,
    chk_update_deferred = NULL,

    primary_path = NULL,
    replica_path = NULL,
    cran_mirror = NULL,
    update_after = NULL,
    lock_timeout = 10000
  )
)

cac_init <- function(self, private, primary_path, replica_path,
                      cran_mirror, update_after) {
  private$primary_path <- primary_path %||% get_user_cache_dir()$root
  private$replica_path <- replica_path
  private$cran_mirror <- cran_mirror
  private$update_after <- update_after
  invisible(self)
}

cac_async_list <- function(self, private, packages, update_after) {
  assert_that(is.null(packages) || is_character(packages))

  private$async_ensure_cache(update_after)$
    then(function(x) {
      if (is.null(packages)) x else x[x$package %in% packages, ]
    })
}

cac_async_update <- function(self, private) {
  hash <- private$get_hash()
  if (!is.null(private$update_deferred)) {
    return(private$update_deferred)                                 # nocov
  }

  private$update_deferred <- private$update_replica()$
    then(function() private$update_primary())$
    then(function() private$data)$
    catch(error = function(err) {
      err$message <- msg_wrap(                                                # nocov
        conditionMessage(err), "\n\n",                                        # nocov
        "Could not load or update archive cache. If you think your local ",   # nocov
        "cache is broken, try deleting it with `cran_archive_cleanup()` or ", # nocov
        "the `$cleanup()` method.")                                           # nocov
      stop(err)                                                               # nocov
    })$
    finally(function() private$update_deferred <- NULL)$
    share()
}

cac_async_check_update <- function(self, private) {
  self; private

  if (!is.null(private$update_deferred)) return(private$update_deferred)          # nocov
  if (!is.null(private$chk_update_deferred)) return(private$chk_update_deferred)  # nocov

  private$chk_update_deferred <- async(private$update_replica)()$
    then(function(ret) {
      rep_file <- private$get_cache_file("replica")
      rep_time <- file_get_time(rep_file)
      stat <- ret$response$status_code
      if (stat < 300) {
        private$update_primary()
        private$data

      } else {
        private$async_ensure_cache()
      }
    })$
    finally(function() private$chk_update_deferred <- NULL)$
    share()
}

cac_summary <- function(self, private) {
  rds_file <- private$get_cache_file("primary")
  ex <- file.exists(rds_file)
  list(
    cachepath = dirname(rds_file),
    current_rds = rds_file,
    lockfile = paste0(rds_file, "-lock"),
    timestamp = if (ex) file_get_time(rds_file) else .POSIXct(NA),
    size = if (ex) file.size(rds_file) else 0L
  )
}

cac_cleanup <- function(self, private, force) {
  if (!force && !interactive()) {
    stop("Not cleaning up cache, please specify `force = TRUE`")
  }
  pri_rds <- private$get_cache_file("primary")
  pri_etag <- paste0(pri_rds, "-etag")
  pri_lock <- paste0(pri_rds, "-lock")
  if (!force) {
    msg <- sprintf(
      "Are you sure you want to clean up the cache in `%s` (y/N)? ",
      pri_rds
    )
    ans <- readline(msg)
    if (! ans %in% c("y", "Y")) stop("Aborted")
  }

  rep_rds <- private$get_cache_file("replica")
  rep_etag <- paste0(rep_rds, "-etag")
  unlink(c(rep_rds, rep_etag), recursive = TRUE, force = TRUE)
  private$data <- NULL
  cli_alert_info("Cleaning up archive cache in {.path {pri_rds}}.")
  unlink(c(pri_rds, pri_etag, pri_lock), recursive = TRUE, force = TRUE)
  invisible(self)
}

cac__get_cache_file <- function(self, private, which) {
  root <- private[[paste0(which, "_path")]]
  hash <- private$get_hash()
  rds_file <- paste0("archive-", substr(hash, 1, 10), ".rds")
  file.path(root, "_archive", rds_file)
}

cac__async_ensure_cache <- function(self, private, max_age) {
  r <-
    try_catch_null(private$get_current_data(max_age)) %||%
    try_catch_null(private$get_memory_cache(max_age)) %||%
    try_catch_null(private$load_replica(max_age)) %||%
    try_catch_null(private$load_primary(max_age))

  if (is.null(r)) {
    self$async_update()
  } else {
    async_constant(r)
  }
}

cac__get_current_data <- function(self, private, max_age) {
  if (is.null(private$data)) stop("No data loaded")
  if (is.null(private$data_time) ||
      Sys.time() - private$data_time > max_age) {
    stop("Loaded data outdated")
  }
  private$data
}

cac__get_memory_cache <- function(self, private, max_age) {
  rds <- private$get_cache_file("primary")
  hash <- private$get_hash()
  hit <- cmc__data[[hash]]
  if (is.null(hit)) stop("Not in memory cache")
  if (is.null(hit$data_time) || Sys.time() - hit$data_time > max_age) {
    stop("Memory cache outdated")
  }
  private$data <- hit$data
  private$data_time <- hit$data_time
  private$data
}

cac__load_replica <- function(self, private, max_age) {
  rds <- private$get_cache_file("replica")
  if (!file.exists(rds)) stop("No replica RDS in cache")

  time <- file_get_time(rds)
  if (Sys.time() - time > max_age) stop("Replica RDS file outdated")

  private$data <- readRDS(rds)
  private$data_time <- time
  private$update_memory_cache()
  private$data
}

cac__load_primary <- function(self, private, max_age) {
  pri_file <- private$get_cache_file("primary")
  rep_file <- private$get_cache_file("replica")

  pri_lock <- paste0(pri_file, "-lock")
  mkdirp(dirname(pri_lock))
  l <- lock(pri_lock, exclusive = FALSE, private$lock_timeout)
  if (is.null(l)) stop("Cannot acquire lock to copy RDS")
  on.exit(unlock(l), add = TRUE)

  if (!file.exists(pri_file)) stop("No primary RDS file in cache")
  time <- file_get_time(pri_file)
  if (Sys.time() - time > max_age) stop("Primary RDS cache file outdated")

  file_copy_with_time(pri_file, rep_file)

  pri_etag <- paste0(pri_file, "-etag")
  rep_etag <- paste0(rep_file, "-etag")
  file_copy_with_time(pri_etag, rep_etag)

  unlock(l)

  private$data <- readRDS(rep_file)
  private$data_time <- time
  private$update_memory_cache()

  private$data
}

cac__update_memory_cache <- function(self, private) {
  hash <- private$get_hash()
  cmc__data[[hash]] <- list(data = private$data, data_time = private$data_time)
}

cac__update_replica <- function(self, private) {
  url <- paste0(private$cran_mirror, "/src/contrib/Meta/archive.rds")
  rep_file <- private$get_cache_file("replica")
  if (!file.exists(rep_file)) {
   tryCatch(private$load_primary(private$update_after), error = function(e) e)
  }

  etag_file <- paste0(rep_file, "-etag")
  tmp <- tempfile()
  # we need to create the file, the etag is not used otherwise in
  # download_if_newer(). If the response is 304, then we'll ignore the file.
  file.create(tmp)

  download_if_newer(url, tmp, etag_file, error_on_status = FALSE)$
    then(function(dl) {
      if (dl$response$status_code >= 300 && dl$response$status_code != 304) {
        stop("Failed to update package archive metadata")
      }
      dl
    })$
    then(function(dl) {
      if (dl$response$status_code != 304) {
        private$convert_archive_file(tmp, rep_file)
      }
      dl
    })$
    finally(function() unlink(tmp))
}

cac__convert_archive_file <- function(self, private, raw, out) {
  archive <- readRDS(raw)
  lens <- viapply(archive, nrow)
  raw <- unlist(lapply(archive, rownames), use.names = FALSE)
  vers <- sub("[.]tar[.]gz$", "", sub("^.*_", "", raw))
  mtime <- unlist(lapply(archive, function(x) x$mtime), use.names = FALSE)
  class(mtime) <- class(archive[[1]]$mtime)
  res <- data_frame(
    package = rep(names(archive), lens),
    version = vers,
    raw = raw,
    mtime = mtime,
    url = paste0(private$cran_mirror, "/src/contrib/Archive/", raw),
    mirror = private$cran_mirror
  )

  ord <- order(tolower(res$package), res$mtime)
  res <- res[ord, ]

  private$data <- res
  save_rds(res, out)
  private$data_time <- file_get_time(out)
  private$update_memory_cache()

  private$data
}

cac__update_primary <- function(self, private, lock) {
  pri_file <- private$get_cache_file("primary")
  rep_file <- private$get_cache_file("replica")

  if (lock) {
    pri_lock <- paste0(pri_file, "-lock")
    mkdirp(dirname(pri_lock))
    l <- lock(pri_lock, exclusive = FALSE, private$lock_timeout)
    if (is.null(l)) stop("Cannot acquire lock to copy RDS")
    on.exit(unlock(l), add = TRUE)
  }

  file_copy_with_time(rep_file, pri_file)
  rep_etag <- paste0(rep_file, "-etag")
  pri_etag <- paste0(pri_file, "-etag")
  file_copy_with_time(rep_etag, pri_etag)

  invisible()
}

get_archive_cache <- function(cran_mirror) {
  hash <- cli::hash_obj_md5(cran_mirror)
  if (is.null(pkgenv$archive_cache[[hash]])) {
    pkgenv$archive_cache[[hash]] <-
      cran_archive_cache$new(cran_mirror = cran_mirror)
  }
  pkgenv$archive_cache[[hash]]
}

#' Data about older versions of CRAN packages
#'
#' CRAN mirrors store older versions of packages in `/src/contrib/Archive`,
#' and they also store some metadata about them in
#' `/src/contrib/Meta/archive.rds`. pkgcache can download and cache this
#' metadata.
#'
#' `cran_archive_list()` lists all versions of all (or some) packages.
#' It updates the cached data first, if it is older than the specified
#' limit.
#'
#' @param cran_mirror CRAN mirror to use, see [default_cran_mirror()].
#' @param update_after `difftime` object. Automatically update the cache if
#'   it gets older than this. Set it to `Inf` to avoid updates. Defaults
#'   to seven days.
#' @param packages Character vector. Only report these packages.
#' @return `cran_archive_list()` returns a data frame with columns:
#'   * `package`: package name,
#'   * `version`: package version. This is a character vector, and not
#'      a [package_version()] object. Some older package versions are not
#'      supported by [package_version()].
#'   * `raw`: the raw row names from the CRAN metadata.
#'   * `mtime`: `mtime` column from the CRAN metadata. This is usually
#'      pretty close to the release date and time of the package.
#'   * `url`: package download URL.
#'   * `mirror`: CRAN mirror that was used to get this data.
#' The output is ordered according to package names (case insensitive) and
#' release dates.
#'
#' @seealso The `cran_archive_cache` class for more flexibility.
#' @export
#' @examplesIf pkgcache:::run_examples()
#' cran_archive_list(packages = "readr")

cran_archive_list <- function(cran_mirror = default_cran_mirror(),
                              update_after = as.difftime(7, units = "days"),
                              packages = NULL) {
  get_archive_cache(cran_mirror)$list(
    update_after = update_after,
    package = packages
  )
}

#' @export
#' @rdname cran_archive_list
#' @details `cran_archive_update()` updates the archive cache.
#' @return `cran_archive_update()` returns all archive data in a data frame,
#' in the same format as `cran_archive_list()`, invisibly.

cran_archive_update <- function(cran_mirror = default_cran_mirror()) {
  invisible(get_archive_cache(cran_mirror)$update())
}

#' @export
#' @rdname cran_archive_list
#' @param force Force cleanup in non-interactive mode.
#' @details `cran_archive_cleanup()` cleans up the archive cache for
#'   `cran_mirror`.
#' @return `cran_archive_cleanup()` returns nothing.

cran_archive_cleanup <- function(cran_mirror = default_cran_mirror(),
                                 force = FALSE) {
  get_archive_cache(cran_mirror)$cleanup(force = force)
  invisible()
}

#' @export
#' @rdname cran_archive_list
#' @details `cran_archive_summary()` prints a summary about the archive
#' cache.
#' @return `cran_archive_summary()` returns a named list with elements:
#' * `cachepath`: Path to the directory that contains all archive cache.
#' * `current_rds`: Path to the RDS file that contains the data for
#'   the specified `cran_mirror`.
#' * `lockfile`: Path to the lock file for `current_rds`.
#' * `timestamp`: Path to the time stamp for `current_rds`. `NA` if the
#'   cache is empty.
#' * `size`: Size of `current_rds`. Zero if the cache is empty.

cran_archive_summary <- function(cran_mirror = default_cran_mirror()) {
  get_archive_cache(cran_mirror)$summary()
}
