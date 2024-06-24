
cmc__data <- new.env(parent = emptyenv())

#' Metadata cache for a CRAN-like repository
#'
#' This is an R6 class that implements the metadata cache of a CRAN-like
#' repository. For a higher level interface, see the [meta_cache_list()],
#' [meta_cache_deps()], [meta_cache_revdeps()] and [meta_cache_update()]
#' functions.
#'
#' The cache has several layers:
#' * The data is stored inside the `cranlike_metadata_cache` object.
#' * It is also stored as an RDS file, in the session temporary directory.
#'   This ensures that the same data is used for all queries of a
#'   `cranlike_metadata_cache` object.
#' * It is stored in an RDS file in the user's cache directory.
#' * The downloaded raw `PACKAGES*` files are cached, together with HTTP
#'   ETags, to minimize downloads.
#'
#' It has a synchronous and an asynchronous API.
#'
#' @section Usage:
#' ```
#' cmc <- cranlike_metadata_cache$new(
#'   primary_path = NULL, replica_path = tempfile(),
#'   platforms = default_platforms(), r_version = getRversion(),
#'   bioc = TRUE, cran_mirror = default_cran_mirror(),
#'   repos = getOption("repos"),
#'   update_after = as.difftime(7, units = "days"))
#'
#' cmc$list(packages = NULL)
#' cmc$async_list(packages = NULL)
#'
#' cmc$deps(packages, dependencies = NA, recursive = TRUE)
#' cmc$async_deps(packages, dependencies = NA, recursive = TRUE)
#'
#' cmc$revdeps(packages, dependencies = NA, recursive = TRUE)
#' cmc$async_revdeps(packages, dependencies = NA, recursive = TRUE)
#'
#' cmc$update()
#' cmc$async_update()
#' cmc$check_update()
#' cmc$asnyc_check_update()
#'
#' cmc$summary()
#'
#' cmc$cleanup(force = FALSE)
#' ```
#'
#' @section Arguments:
#' * `primary_path`: Path of the primary, user level cache. Defaults to
#'   the user level cache directory of the machine.
#' * `replica_path`: Path of the replica. Defaults to a temporary directory
#'   within the session temporary directory.
#' * `platforms`: see [default_platforms()] for possible values.
#' * `r_version`: R version to create the cache for.
#' * `bioc`: Whether to include BioConductor packages.
#' * `cran_mirror`: CRAN mirror to use, this takes precedence over `repos`.
#' * `repos`: Repositories to use.
#' * `update_after`: `difftime` object. Automatically update the cache if
#'   it gets older than this. Set it to `Inf` to avoid updates. Defaults
#'   to seven days.
#' * `packages`: Packages to query, character vector.
#' * `dependencies`: Which kind of dependencies to include. Works the same
#'   way as the `dependencies` argument of [utils::install.packages()].
#' * `recursive`: Whether to include recursive dependencies.
#' * `force`: Whether to force cleanup without asking the user.
#'
#' @section Details:
#'
#' `cranlike_metadata_cache$new()` creates a new cache object. Creation
#' does not trigger the population of the cache. It is only populated on
#' demand, when queries are executed against it. In your package, you may
#' want to create a cache instance in the `.onLoad()` function of the
#' package, and store it in the package namespace. As this is a cheap
#' operation, the package will still load fast, and then the package code
#' can refer to the common cache object.
#'
#' `cmc$list()` lists all (or the specified) packages in the cache.
#' It returns a data frame, see the list of columns below.
#'
#' `cmc$async_list()` is similar, but it is asynchronous, it returns a
#' `deferred` object.
#'
#' `cmc$deps()` returns a data frame, with the (potentially recursive)
#' dependencies of `packages`.
#'
#' `cmc$async_deps()` is the same, but it is asynchronous, it
#' returns a `deferred` object.
#'
#' `cmc$revdeps()` returns a data frame, with the (potentially recursive)
#' reverse dependencies of `packages`.
#'
#' `cmc$async_revdeps()` does the same, asynchronously, it returns an
#' `deferred` object.
#'
#' `cmc$update()` updates the the metadata (as needed) in the cache,
#' and then returns a data frame with all packages, invisibly.
#'
#' `cmc$async_update()` is similar, but it is asynchronous.
#'
#' `cmc$check_update()` checks if the metadata is current, and if it is
#' not, it updates it.
#'
#' `cmc$async_check_update()` is similar, but it is asynchronous.
#'
#' `cmc$summary()` lists metadata about the cache, including its
#' location and size.
#'
#' `cmc$cleanup()` deletes the cache files from the disk, and also from
#' memory.
#'
#' @section Columns:
#' The metadata data frame contains all available versions (i.e. sources and
#' binaries) for all packages. It usually has the following columns,
#' some might be missing on some platforms.
#' * `package`: Package name.
#' * `title`: Package title.
#' * `version`: Package version.
#' * `depends`: `Depends` field from `DESCRIPTION`, or `NA_character_`.
#' * `suggests`: `Suggests` field from `DESCRIPTION`, or `NA_character_`.
#' * `built`:  `Built` field from `DESCIPTION`, if a binary package,
#'   or `NA_character_`.
#' * `imports`: `Imports` field from `DESCRIPTION`, or `NA_character_`.
#' * `archs`: `Archs` entries from `PACKAGES` files. Might be missing.
#' * `repodir`: The directory of the file, inside the repository.
#' * `platform`: This is a character vector. See [default_platforms()] for
#'    more about platform names. In practice each value of the `platform`
#'    column is either
#'    * `"source"` for source packages,
#'    * a platform string, e.g. `x86_64-apple-darwin17.0` for macOS
#'      packages compatible with macOS High Sierra or newer.
#' * `needscompilation`: Whether the package needs compilation.
#' * `type`: `bioc` or `cran`  currently.
#' * `target`: The path of the package file inside the repository.
#' * `mirror`: URL of the CRAN/BioC mirror.
#' * `sources`: List column with URLs to one or more possible locations
#'   of the package file. For source CRAN packages, it contains URLs to
#'   the `Archive` directory as well, in case the package has been
#'   archived since the metadata was cached.
#' * `filesize`: Size of the file, if known, in bytes, or `NA_integer_`.
#' * `sha256`: The SHA256 hash of the file, if known, or `NA_character_`.
#' * `deps`: All package dependencies, in a data frame.
#' * `license`: Package license, might be `NA` for binary packages.
#' * `linkingto`: `LinkingTo` field from `DESCRIPTION`, or `NA_character_`.
#' * `enhances`: `Enhances` field from `DESCRIPTION`, or `NA_character_`.
#' * `os_type`: `unix` or `windows` for OS specific packages. Usually `NA`.
#' * `priority`: "optional", "recommended" or `NA`. (Base packages are
#'   normally not included in the list, so "base" should not appear here.)
#' * `md5sum`: MD5 sum, if available, may be `NA`.
#' * `sysreqs`: The `SystemRequirements` field, if available. This lists the
#'   required system libraries or other software for the package. This is
#'   usually available for CRAN and Bioconductor package and when it is
#'   explicitly available in the repository metadata.
#' * `published`: The time the package was published at, in GMT,
#'   `POSIXct` class.
#'
#' The data frame contains some extra columns as well, these are for internal
#' use only.
#'
#' @export
#' @examplesIf pkgcache:::run_examples()
#' dir.create(cache_path <- tempfile())
#' cmc <- cranlike_metadata_cache$new(cache_path, bioc = FALSE)
#' cmc$list()
#' cmc$list("pkgconfig")
#' cmc$deps("pkgconfig")
#' cmc$revdeps("pkgconfig", recursive = FALSE)

cranlike_metadata_cache <- R6Class(
  "cranlike_metadata_cache",

  public = list(
    initialize = function(primary_path = NULL,
                          replica_path = tempfile(),
                          platforms = default_platforms(),
                          r_version = getRversion(), bioc = TRUE,
                          cran_mirror = default_cran_mirror(),
                          repos = getOption("repos"),
                          update_after = as.difftime(7, units = "days"))
      cmc_init(self, private,  primary_path, replica_path, platforms,
               r_version, bioc, cran_mirror, repos, update_after),

    deps = function(packages, dependencies = NA, recursive = TRUE)
      synchronise(self$async_deps(packages, dependencies, recursive)),
    async_deps = function(packages, dependencies = NA, recursive = TRUE)
      cmc_async_deps(self, private, packages, dependencies, recursive),

    revdeps = function(packages, dependencies = NA, recursive = TRUE)
      synchronise(self$async_revdeps(packages, dependencies, recursive)),
    async_revdeps = function(packages, dependencies = NA, recursive = TRUE)
      cmc_async_revdeps(self, private, packages, dependencies, recursive),

    list = function(packages = NULL)
      synchronise(self$async_list(packages)),
    async_list = function(packages = NULL)
      cmc_async_list(self, private, packages),

    update = function()
      synchronise(self$async_update()),
    async_update = function()
      cmc_async_update(self, private),

    check_update = function()
      synchronise(self$async_check_update()),
    async_check_update = function()
      cmc_async_check_update(self, private),

    summary = function()
      cmc_summary(self, private),

    cleanup = function(force = FALSE)
      cmc_cleanup(self, private, force)
  ),

  private = list(
    get_cache_files = function(which = c("primary", "replica"))
      cmc__get_cache_files(self, private, match.arg(which)),

    async_ensure_cache = function(max_age = private$update_after)
      cmc__async_ensure_cache(self, private, max_age),

    get_current_data = function(max_age)
      cmc__get_current_data(self, private, max_age),
    get_memory_cache = function(max_age)
      cmc__get_memory_cache(self, private, max_age),
    load_replica_rds = function(max_age)
      cmc__load_replica_rds(self, private, max_age),
    load_primary_rds = function(max_age)
      cmc__load_primary_rds(self, private, max_age),
    load_primary_pkgs = function(max_age)
      cmc__load_primary_pkgs(self, private, max_age),

    update_replica_pkgs = function()
      cmc__update_replica_pkgs(self, private),
    update_replica_rds = function(alert = TRUE)
      cmc__update_replica_rds(self, private, alert),
    update_primary = function(rds = TRUE, packages = TRUE, lock = TRUE)
      cmc__update_primary(self, private, rds, packages, lock),
    update_memory_cache = function()
      cmc__update_memory_cache(self, private),

    copy_to_replica = function(rds = TRUE, pkgs = FALSE, etags = FALSE)
      cmc__copy_to_replica(self, private, rds, pkgs, etags),

    ## We use this to make sure that different versions of pkgcache can
    ## share the same metadata cache directory. It is used to calculate
    ## the hash of the cached RDS file.
    cache_version = "8",

    data = NULL,
    data_time = NULL,
    data_messaged = NULL,

    update_deferred = NULL,
    chk_update_deferred = NULL,

    primary_path = NULL,
    replica_path = NULL,
    platforms = NULL,
    r_version = NULL,
    bioc = NULL,
    repos = NULL,
    update_after = NULL,
    dirs = NULL,
    lock_timeout = 10000
  )
)

cmc_init <- function(self, private, primary_path, replica_path, platforms,
                     r_version, bioc, cran_mirror, repos, update_after) {

  "!!DEBUG Init metadata cache in '`replica_path`'"
  r_version <- as.character(r_version)
  private$primary_path <- primary_path %||% get_user_cache_dir()$root
  private$replica_path <- replica_path
  private$platforms <- platforms
  private$r_version <- get_minor_r_version(r_version)
  private$bioc <- bioc
  private$repos <- cmc__get_repos(repos, bioc, cran_mirror, r_version)
  private$update_after <- update_after
  private$dirs <- get_all_package_dirs(platforms, r_version)
  invisible(self)
}

cmc_async_deps <- function(self, private, packages, dependencies,
                           recursive) {
  assert_that(
    is_character(packages),
    is_dependencies(dependencies),
    is_flag(recursive))

  "!!DEBUG Getting deps"
  private$async_ensure_cache()$
    then(function(.) extract_deps(., packages, dependencies, recursive))
}

cmc_async_revdeps <- function(self, private, packages, dependencies,
                              recursive) {
  assert_that(
    is_character(packages),
    is_dependencies(dependencies),
    is_flag(recursive))

  "!!DEBUG Getting revdeps"
  private$async_ensure_cache()$
    then(function(.) extract_revdeps(., packages, dependencies, recursive))
}

cmc_async_list <- function(self, private, packages) {
  assert_that(is.null(packages) || is_character(packages))

  "!!DEBUG Listing packages"
  private$async_ensure_cache()$
    then(function(x) {
      if (is.null(packages)) x$pkgs else x$pkgs[x$pkgs$package %in% packages,]
    })
}

cmc_async_update <- function(self, private) {
  self; private
  if (!is.null(private$update_deferred)) return(private$update_deferred)

  private$update_deferred <- async(private$update_replica_pkgs)()$
    then(function() private$update_replica_rds())$
    then(function() private$update_primary())$
    then(function() private$data)$
    finally(function() private$update_deferred <- NULL)$
    share()
}

cmc_async_check_update <- function(self, private) {
  self; private

  if (!is.null(private$update_deferred)) return(private$update_deferred)
  if (!is.null(private$chk_update_deferred)) return(private$chk_update_deferred)

  private$chk_update_deferred <- async(private$update_replica_pkgs)()$
    then(function(ret) {
      ## Some might be NULL, if failure was allowed and indeed it happened.
      ## For these we just pretend that they did not change, so they do
      ## not trigger an update. The metadata RDS builder is robust for
      ## these files to be empty or non-existing.
      stat <- viapply(ret, function(x) x$response$status_code %||% 304L)
      rep_files <- private$get_cache_files("replica")
      pkg_times <- file_get_time(rep_files$pkgs$path)
      if (! file.exists(rep_files$rds) ||
          any(file_get_time(rep_files$rds) < pkg_times) ||
          any(stat < 300)) {
        private$update_replica_rds(alert = FALSE)
        private$update_primary()
        private$data

      } else {
        private$async_ensure_cache()
      }
    })$
    finally(function() private$chk_update_deferred <- NULL)$
    share()
}

cmc_summary <- function(self, private) {
  dirs <- private$get_cache_files("primary")
  pgz <- dir(dirs$meta, recursive = TRUE, pattern = "^PACKAGES\\.gz$",
             full.names = TRUE)
  all <- dir(dirs$meta, recursive = TRUE, full.names = TRUE)
  list(
    cachepath = dirs$meta,
    lockfile = dirs$lock,
    current_rds = dirs$rds,
    raw_files = pgz,
    rds_files = dir(dirs$meta, pattern = "\\.rds$", full.names = TRUE),
    size = sum(file.size(all))
  )
}

cmc_cleanup <- function(self, private, force) {
  if (!force && !interactive()) {
    stop("Not cleaning up cache, please specify `force = TRUE`")
  }
  cache_dir <- private$get_cache_files("primary")$meta
  if (!force) {
    msg <- sprintf(
      "Are you sure you want to clean up the cache in `%s` (y/N)? ",
      cache_dir
    )
    ans <- readline(msg)
    if (! ans %in% c("y", "Y")) stop("Aborted")
  }

  local_cache_dir <- private$get_cache_files("replica")
  unlink(local_cache_dir, recursive = TRUE, force = TRUE)
  private$data <- NULL
  private$data_messaged <- NULL
  cli::cli_alert_info("Cleaning up cache directory {.path {cache_dir}}.")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
}

#' @importFrom utils URLencode

hash_obj_md5 <- function(x, ...) {
  cli::hash_obj_md5(x, ...)
}

repo_encode <- function(repos) {
  paste0(
    vcapply(repos$name, URLencode, reserved = TRUE), "-",
    substr(vcapply(repos$url, "hash_obj_md5"), 1, 10)
  )
}

cran_metadata_url <- function() {
  getOption("pkg.cran_metadata_url",
     Sys.getenv(
       "R_PKG_CRAN_METADATA_URL",
       "https://cran.r-pkg.org/metadata/")
     )
}

cmc__get_cache_files <- function(self, private, which) {
  root <- private[[paste0(which, "_path")]]

  repo_hash <- hash_obj_md5(list(repos = private$repos, dirs = private$dirs,
                                 version = private$cache_version))

  str_platforms <- paste(private$platforms, collapse = "+")
  rds_file <- paste0("pkgs-", substr(repo_hash, 1, 10), ".rds")

  repo_enc <- rep(repo_encode(private$repos), each = nrow(private$dirs))
  pkgs_dirs <- rep(private$dirs$contriburl, nrow(private$repos))
  pkgs_files <- file.path(pkgs_dirs, "PACKAGES.gz")
  pkgs_files2 <- file.path(pkgs_dirs, "PACKAGES")
  mirror <- rep(private$repos$url, each = nrow(private$dirs))
  name <- tolower(rep(private$repos$name, each = nrow(private$dirs)))
  type <- rep(private$repos$type, each = nrow(private$dirs))
  r_version <- rep(private$dirs$rversion, nrow(private$repos))
  bioc_version <- rep(private$repos$bioc_version, each = nrow(private$dirs))

  pkg_path <- file.path(root, "_metadata", repo_enc, pkgs_files)
  meta_path <- ifelse(
    type == "cran" | name == "rspm" | name == "ppm" | name == "p3m",
    file.path(root, "_metadata", repo_enc, pkgs_dirs, "METADATA2.gz"),
    NA_character_)
  meta_etag <- ifelse(
    !is.na(meta_path), paste0(meta_path, ".etag"), NA_character_)
  meta_url <- ifelse(
    !is.na(meta_path),
    paste0(cran_metadata_url(), pkgs_dirs, "/METADATA2.gz"),
    NA_character_)

  bin_url <- ppm_binary_url(mirror, private$r_version)
  bin_path <- ifelse(
    is.na(bin_url),
    NA_character_,
    file.path(root, "_metadata", repo_enc, pkgs_dirs, "PACKAGES2.gz")
  )
  bin_etag <- ifelse(
    is.na(bin_url),
    NA_character_,
    paste0(bin_path, ".etag")
  )

  res <- list(
    root = root,
    meta = file.path(root, "_metadata"),
    lock = file.path(root, "_metadata.lock"),
    rds  = file.path(root, "_metadata", rds_file),
    pkgs = data_frame(
      path = pkg_path,
      etag = file.path(root, "_metadata", repo_enc, paste0(pkgs_files, ".etag")),
      basedir = pkgs_dirs,
      base = pkgs_files,
      mirror = mirror,
      url = paste0(mirror, "/", pkgs_files),
      fallback_url = paste0(mirror, "/", pkgs_files2),
      platform = rep(private$dirs$platform, nrow(private$repos)),
      type = type,
      r_version = r_version,
      bioc_version = bioc_version,
      meta_path = meta_path,
      meta_etag = meta_etag,
      meta_url = meta_url,
      bin_path = bin_path,
      bin_etag = bin_etag,
      bin_url = bin_url
    )
  )

  res
}

ppm_binary_url <- function(urls, r_version) {
  res <- rep(NA_character_, length(urls))

  # If multiple R versions are requested, then we give up, and pretend
  # that PPM binaries are source packages
  if (length(r_version) != 1) return(res)

  # http://rspm.infra/all/__linux__/bionic/latest ->
  # http://rspm.infra/all/latest/bin/linux/4.2-bionic/contrib/4.2/PACKAGES

  has_binary <- re_match(urls, re_ppm_linux())
  mch <- !is.na(has_binary$.match)

  res[mch] <- paste0(
    has_binary$base[mch],
    has_binary$repo[mch], "/",
    has_binary$version[mch], "/bin/linux/",
    r_version, "-", has_binary$distro[mch],
    "/contrib/", r_version, "/",
    "PACKAGES.gz"
  )

  res
}

re_ppm_linux <- function() {
  paste0(
    "^",
    "(?<base>.*/)",
    "(?<repo>cran|all)/",
    "__linux__/",
    "(?<distro>[a-zA-Z0-9]+)/",
    "(?<version>latest|[-0-9]+)",
    "$"
  )
}

is_ppm_linux_repo_url <- function(urls) {
  grepl(re_ppm_linux(), urls, perl = TRUE)
}

#' Load the cache, asynchronously, with as little work as possible
#'
#' 1. If it is already loaded, and fresh return it.
#' 2. Otherwise try the replica RDS.
#' 3. Otherwise try the primary RDS.
#' 4. Otherwise try the primary PACKAGES files.
#' 5. Otherwise update the replica PACKAGES files,
#'    the replica RDS, and then also the primary PACKAGES and RDS.
#'
#' @param self self
#' @param private private self
#' @param max_age Maximum age allowed to consider the data current.
#' @return Metadata.
#' @keywords internal

cmc__async_ensure_cache <- function(self, private, max_age) {
  max_age

  r <- try_catch_null(private$get_current_data(max_age)) %||%
    try_catch_null(private$get_memory_cache(max_age)) %||%
    try_catch_null(private$load_replica_rds(max_age)) %||%
    try_catch_null(private$load_primary_rds(max_age)) %||%
    try_catch_null(private$load_primary_pkgs(max_age))

  if (is.null(r)) {
    self$async_update()
  } else {
    async_constant(r)
  }
}

cmc__get_current_data <- function(self, private, max_age) {
  "!!DEBUG Get current data?"
  if (is.null(private$data)) stop("No data loaded")
  if (is.null(private$data_time) ||
      Sys.time() - private$data_time > max_age) {
    stop("Loaded data outdated")
  }

  "!!DEBUG Got current data!"
  if (! isTRUE(private$data_messaged)) {
    private$data_messaged <- TRUE
  }
  private$data
}

cmc__get_memory_cache  <- function(self, private, max_age) {
  "!!DEBUG Get from memory cache?"
  rds <- private$get_cache_files("primary")$rds
  hit <- cmc__data[[rds]]
  if (is.null(hit)) stop("Not in the memory cache")
  if (is.null(hit$data_time) || Sys.time() - hit$data_time > max_age) {
    stop("Memory cache outdated")
  }
  private$data <- hit$data
  private$data_time <- hit$data_time
  private$data_messaged <- NULL

  private$data
}

#' Try to load the package metadata asynchronously, from the replica RDS
#'
#' If the replica has the RDS data, it is loaded and returned.
#' Otherwise throws an error.
#'
#' @param self Self.
#' @param private Private self.
#' @param max_age Maximum age allowed for the RDS file to be considered
#'   as current.
#' @return The metadata.
#' @keywords internal

cmc__load_replica_rds <- function(self, private, max_age) {
  "!!DEBUG Load replica RDS?"
  rds <- private$get_cache_files("replica")$rds
  if (!file.exists(rds)) stop("No replica RDS file in cache")

  time <- file_get_time(rds)
  if (Sys.time() - time > max_age) stop("Replica RDS cache file outdated")

  sts <- cli::cli_process_start("Loading metadata database")
  private$data <- readRDS(rds)
  private$data_time <- time
  private$data_messaged <- NULL
  "!!DEBUG Loaded replica RDS!"
  private$update_memory_cache()
  cli::cli_process_done(sts)

  private$data
}

#' Load the metadata from the primary cache's RDS file
#'
#' If it exists and current, then the replica RDS is updated to it as well,
#' and the data is returned. Otherwise throws an error.
#'
#' @inheritParams cmc__load_replica_rds
#' @return Metadata.
#' @keywords internal

cmc__load_primary_rds <- function(self, private, max_age) {
  "!!DEBUG Load primary RDS?"
  pri_files <- private$get_cache_files("primary")
  rep_files <- private$get_cache_files("replica")

  mkdirp(dirname(pri_files$lock))
  l <- filelock::lock(pri_files$lock, exclusive = FALSE, private$lock_timeout)
  if (is.null(l)) stop("Cannot acquire lock to copy RDS")
  on.exit(filelock::unlock(l), add = TRUE)

  if (!file.exists(pri_files$rds)) stop("No primary RDS file in cache")
  time <- file_get_time(pri_files$rds)
  if (Sys.time() - time > max_age) stop("Primary RDS cache file outdated")

  ## Metadata files might be missing or outdated, that's ok (?)
  pkgs_times <- file_get_time(pri_files$pkgs$path)
  if (any(is.na(pkgs_times)) || any(pkgs_times >= time)) {
    unlink(pri_files$rds)
    stop("Primary PACKAGES missing or newer than replica RDS, removing")
  }

  sts <- cli::cli_process_start("Loading metadata database")
  file_copy_with_time(pri_files$rds, rep_files$rds)
  filelock::unlock(l)

  private$data <- readRDS(rep_files$rds)
  private$data_time <- time
  private$data_messaged <- NULL

  private$update_memory_cache()
  cli::cli_process_done(sts)

  private$data
}

#' Load metadata from the primary cache's PACKAGES files
#'
#' If they are not available, or outdated, it throws an error.
#' Otherwise they are copied to the replica cache, and then used
#' to create the RDS file. The RDS file is then written back to the
#' primary cache and also loaded.
#'
#' @param self self
#' @param private private self
#' @param max_age Max age to consider the files current.
#' @return Metadata.
#' @keywords internal

cmc__load_primary_pkgs <- function(self, private, max_age) {
  "!!DEBUG Load replica PACKAGES*?"
  pri_files <- private$get_cache_files("primary")
  rep_files <- private$get_cache_files("replica")

  ## Lock
  mkdirp(dirname(pri_files$lock))
  l <- filelock::lock(pri_files$lock, exclusive = FALSE, private$lock_timeout)
  if (is.null(l)) stop("Cannot acquire lock to copy PACKAGES files")
  on.exit(filelock::unlock(l), add = TRUE)

  ## Check if PACKAGES exist and current. It is OK if metadata is missing
  pkg_files <- pri_files$pkgs$path
  if (!all(file.exists(pkg_files))) {
    stop("Some primary PACKAGES files don't exist")
  }
  time <- file_get_time(pkg_files)
  if (any(Sys.time() - time > max_age)) {
    stop("Some primary PACKAGES files are outdated")
  }

  ## Copy to replica, if we cannot copy the etags, that's ok
  sts <- cli::cli_process_start("Loading metadata database")
  private$copy_to_replica(rds = FALSE, pkgs = TRUE, etags = TRUE)

  ## Update RDS in replica, this also loads it
  private$update_replica_rds(alert = FALSE)

  ## Update primary, but not the PACKAGES
  private$update_primary(rds = TRUE, packages = FALSE, lock = FALSE)
  cli::cli_process_done(sts)

  private$data
}

#' Update the PACKAGES files in the replica cache
#'
#' I.e. download them, if they have changed.
#'
#' @param self self
#' @param private private self
#' @keywords internal

cmc__update_replica_pkgs <- function(self, private) {
  "!!DEBUG Update replica PACKAGES"
  tryCatch(
    private$copy_to_replica(rds = TRUE, pkgs = TRUE, etags = TRUE),
    error = function(e) e)

  rep_files <- private$get_cache_files("replica")
  pkgs <- rep_files$pkgs

  bsq_url <- "https://github.com/r-lib/pkgcache/raw/HEAD/inst/bioc-sysreqs.dcf.gz"
  bsq_path <- bioc_sysreqs_cached()
  bsq_etag <- paste0(bsq_path, ".etag")

  meta <- !is.na(pkgs$meta_url)
  bin <- !is.na(pkgs$bin_url)
  dls <- data.frame(
    stringsAsFactors = FALSE,
    url = c(pkgs$url, pkgs$meta_url[meta], pkgs$bin_url[bin], bsq_url),
    fallback_url = c(pkgs$fallback_url, rep(NA_character_, sum(meta) + sum(bin)), NA_character_),
    path = c(pkgs$path, pkgs$meta_path[meta], pkgs$bin_path[bin], bsq_path),
    etag = c(pkgs$etag, pkgs$meta_etag[meta], pkgs$bin_etag[bin], bsq_etag),
    timeout = c(rep(c(200, 100), c(nrow(pkgs), sum(meta) + sum(bin))), 5),
    mayfail = TRUE
  )

  download_files(dls)$
    then(function(result) {
      missing_pkgs_note(pkgs, result)
      load_bioc_sysreqs()
      result
    })
}

# E.g. "R 4.1 macos packages are missing from CRAN and Bioconductor"

missing_pkgs_note <- function(pkgs, result) {
  bad <- vlapply(result[seq_len(nrow(pkgs))], inherits, "error")
  if (!any(bad)) return()

  repo_name <- function(type, url) {
    if (type == "cran") return("CRAN")
    if (type == "bioc") return("Bioconductor")
    sub("^https?://([^/]*).*$", "\\1", url)
  }

  msgs <- lapply(which(bad), function(i) {
    list(
      paste0(
        if (pkgs$r_version[i] != "*") paste0("R ", pkgs$r_version[i], " "),
        pkgs$platform[i]
      ),
      repo_name(pkgs$type[i], pkgs$mirror[i])
    )
  })

  what <- vcapply(msgs, "[[", 1)
  where <- vcapply(msgs, "[[", 2)
  errmsg <- vcapply(which(bad), function(i) conditionMessage(result[[i]]))
  for (wt in unique(what)) {
    wh <- unique(where[what == wt])
    cli::cli_alert_info("{wt} packages are missing from {wh}: {errmsg}")
  }
}

#' Update the replica RDS from the PACKAGES files
#'
#' Also loads it afterwards.
#'
#' @param self self
#' @param private private self
#' @param alert whether to show message about the update
#' @keywords internal

cmc__update_replica_rds <- function(self, private, alert) {
  "!!DEBUG Update replica RDS"
  if (alert) sts <- cli::cli_process_start("Updating metadata database")
  rep_files <- private$get_cache_files("replica")

  data_list <- lapply_rows(
    rep_files$pkgs,
    function(r) {
      rversion <- if (r$platform == "source") "*" else private$r_version
      read_packages_file(r$path, mirror = r$mirror,
                         repodir = r$basedir, platform = r$platform,
                         rversion = rversion, type = r$type,
                         meta_path = r$meta_path, bin_path = r$bin_path,
                         orig_r_version = private$r_version)
    }
  )

  data_list <- data_list[!vlapply(data_list, is.null)]

  if (length(data_list) == 0) stop("No metadata available")

  private$data <- merge_packages_data(.list = data_list)
  save_rds(private$data, rep_files$rds)
  private$data_time <- file_get_time(rep_files$rds)
  private$data_messaged <- NULL

  private$update_memory_cache()

  if (alert) cli::cli_process_done(sts)
  private$data
}

#' Update the primary cache from the replica cache
#'
#' @param self self
#' @param private private self
#' @param rds Whether to update the RDS file.
#' @param packages Whether to update the PACKAGES files (+ ETag files).
#' @return Nothing.
#'
#' @keywords internal

cmc__update_primary <- function(self, private, rds, packages, lock) {

  "!!DEBUG Updata primary cache"
  if (!rds && !packages) return()

  pri_files <- private$get_cache_files("primary")
  rep_files <- private$get_cache_files("replica")

  if (lock) {
    mkdirp(dirname(pri_files$lock))
    l <- filelock::lock(pri_files$lock, exclusive = TRUE, private$lock_timeout)
    if (is.null(l)) stop("Cannot acquire lock to update primary cache")
    on.exit(filelock::unlock(l), add = TRUE)
  }

  if (rds) {
    file_copy_with_time(rep_files$rds, pri_files$rds)
  }
  if (packages) {
    file_copy_with_time(rep_files$pkgs$path, pri_files$pkgs$path)
    file_copy_with_time(rep_files$pkgs$etag, pri_files$pkgs$etag)
    file_copy_with_time(na_omit(rep_files$pkgs$meta_path),
                        na_omit(pri_files$pkgs$meta_path))
    file_copy_with_time(na_omit(rep_files$pkgs$meta_etag),
                        na_omit(pri_files$pkgs$meta_etag))
    file_copy_with_time(na_omit(rep_files$pkgs$bin_path),
                        na_omit(pri_files$pkgs$bin_path))
    file_copy_with_time(na_omit(rep_files$pkgs$bin_etag),
                        na_omit(pri_files$pkgs$bin_etag))
  }
  invisible()
}

cmc__update_memory_cache <- function(self, private) {
  rds <- private$get_cache_files("primary")$rds
  cmc__data[[rds]] <- list(data = private$data, data_time = private$data_time)
}

cmc__copy_to_replica <- function(self, private, rds, pkgs, etags) {
  pri_files <- private$get_cache_files("primary")
  rep_files <- private$get_cache_files("replica")

  mkdirp(dirname(pri_files$lock))
  l <- filelock::lock(pri_files$lock, exclusive = FALSE, private$lock_timeout)
  if (is.null(l)) stop("Cannot acquire lock to copy primary cache")
  on.exit(filelock::unlock(l), add = TRUE)

  if (rds) {
    file_copy_with_time(pri_files$rds, rep_files$rds)
  }

  if (pkgs) {
    file_copy_with_time(pri_files$pkgs$path, rep_files$pkgs$path)
    file_copy_with_time(na_omit(pri_files$pkgs$meta_path),
                        na_omit(rep_files$pkgs$meta_path))
    file_copy_with_time(na_omit(pri_files$pkgs$bin_path),
                        na_omit(rep_files$pkgs$bin_path))
  }
  if (etags) {
    file_copy_with_time(pri_files$pkgs$etag, rep_files$pkgs$etag)
    file_copy_with_time(na_omit(pri_files$pkgs$meta_etag),
                        na_omit(rep_files$pkgs$meta_etag))
    file_copy_with_time(na_omit(pri_files$pkgs$bin_etag),
                        na_omit(rep_files$pkgs$bin_etag))
  }
}

extract_deps <- function(pkgs, packages, dependencies, recursive) {

  realdep <- interpret_dependencies(dependencies)
  dep <- tolower(realdep$direct)

  new <- packages
  repeat {
    new <- setdiff(
      pkgs$deps$package[pkgs$deps$upstream %in% new &
                        pkgs$deps$type %in% dep],
      packages)
    if (!length(new)) break
    packages <- c(packages, new)
    if (!recursive) break
    dep <- tolower(realdep$indirect)
  }

  packages <- setdiff(packages, "R")
  res <- pkgs$pkgs[pkgs$pkgs$package %in% packages, ]

  base <- intersect(packages, base_packages())
  attr(res, "base") <- base
  attr(res, "unknown") <- setdiff(packages, c(res$package, base))

  res
}

extract_revdeps <- function(pkgs, packages, dependencies, recursive) {

  realdep <- interpret_dependencies(dependencies)
  dep <- tolower(realdep$direct)

  new <- packages
  repeat {
    new <- setdiff(
      pkgs$deps$upstream[pkgs$deps$ref %in% new & pkgs$deps$type %in% dep],
      packages)
    if (!length(new)) break
    packages <- c(packages, new)
    if (!recursive) break
    dep <- tolower(realdep$indirect)
  }

  packages <- setdiff(packages, "R")
  res <- pkgs$pkgs[pkgs$pkgs$package %in% packages, ]

  base <- intersect(packages, base_packages())
  attr(res, "base") <- base
  attr(res, "unknown") <- setdiff(packages, c(res$package, base))

  res
}

cmc__get_repos <- function(repos, bioc, cran_mirror, r_version) {
  repos[["CRAN"]] <- cran_mirror
  repos <- unlist(repos)
  bioc_names <- bioconductor$get_repos()
  res <- data_frame(
    name = names(repos),
    url = unname(repos),
    type = ifelse(
      names(repos) == "CRAN",
      "cran",
      ifelse(names(repos) %in% bioc_names, "bioc", "cranlike")
    ),
    r_version = "*",
    bioc_version = NA_character_
  )

  if (bioc) {
    for (rver in r_version) {
      bioc_version <- as.character(bioconductor$get_bioc_version(rver))
      bioc_repos <- bioconductor$get_repos(bioc_version)

      bioc_res <- data_frame(
        name = names(bioc_repos),
        url = unname(bioc_repos),
        type = "bioc",
        r_version = rver,
        bioc_version = bioc_version
      )
      res <- rbind(res, bioc_res)
    }
  }

  res <- res[!duplicated(res$url), ]

  res
}

#' Query CRAN(like) package data
#'
#' It uses CRAN and BioConductor packages, for the current platform and
#' R version, from the default repositories.
#'
#' `meta_cache_list()` lists all packages.
#'
#' `meta_cache_update()` updates all metadata. Note that metadata is
#' automatically updated if it is older than seven days.
#'
#' `meta_cache_deps()` queries packages dependencies.
#'
#' `meta_cache_revdeps()` queries reverse package dependencies.
#'
#' `meta_cache_summary()` lists data about the cache, including its location
#' and size.
#'
#' `meta_cache_cleanup()` deletes the cache files from the disk.
#'
#' @param packages Packages to query.
#' @param dependencies Dependency types to query. See the `dependencies`
#'   parameter of [utils::install.packages()].
#' @param recursive Whether to query recursive dependencies.
#' @param force Whether to force cleanup without asking the user.
#' @return A data frame of the dependencies. For
#'   `meta_cache_deps()` and `meta_cache_revdeps()` it includes the
#'   queried `packages` as well.
#'
#' @export
#' @examplesIf pkgcache:::run_examples()
#' meta_cache_list("pkgdown")
#' meta_cache_deps("pkgdown", recursive = FALSE)
#' meta_cache_revdeps("pkgdown", recursive = FALSE)

meta_cache_deps <- function(packages, dependencies = NA,
                            recursive = TRUE) {
  get_cranlike_metadata_cache()$deps(packages, dependencies, recursive)
}

#' @export
#' @rdname meta_cache_deps

meta_cache_revdeps <- function(packages, dependencies = NA,
                               recursive = TRUE) {
  get_cranlike_metadata_cache()$revdeps(packages, dependencies, recursive)
}

#' @export
#' @rdname meta_cache_deps

meta_cache_update <- function() {
  invisible(get_cranlike_metadata_cache()$update()$pkgs)
}

#' @export
#' @rdname meta_cache_deps

meta_cache_list <- function(packages = NULL) {
  get_cranlike_metadata_cache()$list(packages)
}

#' @export
#' @rdname meta_cache_deps

meta_cache_cleanup <- function(force = FALSE) {
  get_cranlike_metadata_cache()$cleanup(force = force)
}

#' @export
#' @rdname meta_cache_deps

meta_cache_summary <- function() {
  get_cranlike_metadata_cache()$summary()
}
