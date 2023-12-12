# pkgcache 2.2.1

* pkgcache now does a better job when matching the R version to a
  Bioconductor version.

# pkgcache 2.2.0

* New `pkg.current_platform` option and `PKG_CURRENT_PLATFORM` environment
  variable to override the detected platform.

* In `repo_add()` and `repo_resolve()` the `MRAN@` prefix is now deprecated
  and resolves to PPM, because MRAN will be retired soon. See more at
  <https://posit.co/blog/migrating-from-mran-to-posit-package-manager/>.

* The metadata cache now has `SystemRequirements` information for Bioconductor
  packages.

# pkgcache 2.1.1

* `parse_installed()` now has a `packages` argument, to list only a subset
  of all packages.

* `parse_packages()` can now parse `PACKAGES` files with trailing
  whitespace (#93).

* The Bioconductor repositories now include the 'books' repository,
  available since Bioconductor 3.12.

# pkgcache 2.1.0

* pkgcache now supports binary packages on x86_64 macOS and R 4.3.0 and
  later (#89).

* Better Posit Package Manager (PPM) support. New `ppm_has_binaries()`,
  `ppm_r_versions()`, `ppm_repo_url()`, `ppm_snapshots()` and `ppm_platforms()`
  functions to help interacting with PPM. See the new 'pkgcache and Posit
  Package Manager on Linux' article at https://r-lib.github.io/pkgcache.
  (#47 and r-lib/pkgdepends#186).

# pkgcache 2.0.4

* Update R version -> Bioconductor version mapping. R 4.2.x now maps to
  Bioconductor 3.16.

# pkgcache 2.0.3

* The `built` and `sysreqs` columns of the metadata case are always
  character vectors now, and not logicals, as it used to be in some
  edges cases in the past.

* The `deps` column of the metadata cache is not a tibble any more,
  but a data frame with a `tbl` class, as it should be.

* `cran_archive_*()` functions now only download the metadata if it is newer
  than what you have currently.

* `cran_archive_cleanup()` now does not ignore the `force` argument.

* The `sources` column in the metadata cache now has the correct URL for
  packages in the CRAN archive (https://github.com/r-lib/pak/issues/425).

# pkgcache 2.0.2

* pkgcache error messages are better now.

* pkgcache now does not compress the metadata cache files, which makes
  loading the metadata cache faster.

# pkgcache 2.0.1

No user visible changes.

# pkgcache 2.0.0

## BREAKING CHANGE

* Starting from version 2.0.0 pkgcache returns data frames instead
  of tibbles. While data frames and tibbles are very similar, they are
  not completely compatible. To convert the output of pkgcache to tibbles
  call the `as_tibble()` function on them.

  pkgcache loads the pillar package at startup, if available, and uses it
  to improve the printing of pkgcache data frames.

## OTHER CHANGES

* `parse_packages()` now automatically determines the type of the `PACKAGES*`
  file, instead of relying on file extensions.

* pkgcache can now call back again to a `PACKAGES` file if `PACKAGES.gz` is
  not available. (This fixes a regression in pkgcache 1.3.0.)

* pkgcache now uses HTTP 1.1 on Linux as well, because of crashes with
  HTTP/2.

* pkgcache now supports `file:///`, repositories, i.e. repositories on
  the local file system.

# pkgcache 1.3.0

* pkgcache now works better on M1 macs.

* `current_r_platform()` does a much better job now. In particular, on
  Linux it includes the name and release of the distribution.
  The new `current_r_platform_data()` function returns the platform information
  as a data frame, instead of a single string.

* Metadata is now more accurate for Windows packages that are typically
  not multi-arch any more on R 4.2.0 (current R-devel).

* pkgcache has its own DCF metadata parser now, which is much faster, and
  it parses all fields of `PACAKGES*` and `DESCRIPTION` files.

* New `parse_installed()` function to get the metadata of all installed
  packages in a library. It uses the new DCF parser, so it is quite fast.

* `meta_cache_list()` and related functions now correctly set the
  `rversion` column of source R packages to `"*"`.

* pkgcache now uses HTTP 1.1 on macOS, to work around a possible
  slowdown issue with libcurl for HTTP/2.

* pkgcache now uses our extra metadata (file sizes, system requirements,
  etc.) for RStudio Package Manager (RSPM) repositories as well, as long
  as they are named `RSPM ` in `getOption("repos")`.

# pkgcache 1.2.2

* The default location of the cache has changed to align with the
  standard `tools::R_user_dir()` cache location. To clean up your old
  cache call `pkgcache:::cleanup_old_cache_dir()`.

# pkgcache 1.2.1

No user visible changes.

# pkgcache 1.2.0

* New `repo_add()`, `repo_get()`, `repo_resolve()` and `with_repo()`
  functions to query and manipulate repositories.

* `meta_cache_*()` functions now handle `getOption("repos")` changes
  correctly.

* Failed metadata downloads now do not trigger metadata updates (#52).

* New `bioc_release_version()`, `bioc_devel_version()`, `bioc_repos()`
  helper functions to deal with Bioconductor repositories.

* Metadata cache functions, e.g. `meta_cache_deps()` etc. now allow
  specifying the dependency types in all lowercase (#54).

# pkgcache 1.1.1

* `package_cache` now does not fail if the web server does not send an
  `Etag` header when downloading packages.

* `package_cache` has now much relaxed HTTP timeouts, and handles
  downloading many packages (slowly) much better.

* The package download progress bar can now be suppressed by setting
  the `pkg.show_progress` option to `FALSE`.

# pkgcache 1.1.0

* New `repo_status()` function to query the status and response time
  of CRAN-like repositories.

* New `bioc_version()` and `bioc_version_map()` functions to query
  Bioconductor repositories.

* pkgcache now does not fail if some repositories do not provide
  some package types.

* New `current_r_platform()`, `default_cran_mirror()` and
  `default_platforms()` functions.

* pkgcache now works for R 4.0.x macOS binaries.

# pkgcache 1.0.7

* Metadata is now cached in RDS version 2 formats, so metadata written
  by newer R version can be used by older R versions as well (#36).

# pkgcache 1.0.6

* HTTP timeouts are now much better, and by default they are defined
  in terms of download speed, instead of total download time (#29).

* pkgcache now tries to download metadata from the `PACKAGES` file, if it
  cannot find `PACKAGES.gz` (@timmsm, #27).

* pkgcache is now less verbose when updating or loading metadata.

# pkgcache 1.0.5

* Fix a bug in the download functions, that broke pak downloads.

# pkgcache 1.0.4

* Fix handling of Bioconductor versions and repositories, see
  README for the details.

* Now different versions of pkgcache, that potentially have different
  metadata format, can share the same metadata cache directory.

# pkgcache 1.0.3

* Fix concurrency issues when the async API is used multiple times in the
  same event loop.

* Make package compatible with tibble >= 2.0.0.

* Add `meta_cache_summary()` and a `summary()` method for
  `cranlike_metadata_cache`. Return information about a metadata cache
  instance.

# pkgcache 1.0.2

* First public release
