# Environment variables and options that modify the default behavior

pak behavior can be finetuned with environment variables and options (as
in [`base::options()`](https://rdrr.io/r/base/options.html)).

## R options affecting pak's behavior

### `Ncpus`

Set to the desired number of worker processes for package installation.
If not set, then pak will use the number of logical processors in the
machine.

### `repos`

The CRAN-like repositories to use. See
[`base::options()`](https://rdrr.io/r/base/options.html) for details.

## pak configuration

Configuration entries (unless noted otherwise on this manual page) have
a corresponding environment variable, and a corresponding option.

The environment variable is always uppercase and uses underscores as the
word separator. It always has the `PKG_` prefix.

The option is typically lowercase, use it uses underscores as the word
separator, but it always has the `pkg.` prefix (notice the dot!).

Some examples:

|                   |                   |                   |
|-------------------|-------------------|-------------------|
| Config entry name | Env var name      | Option name       |
| platforms         | `PKG_PLATFORMS`   | `pkg.platforms`   |
| cran_mirror       | `PKG_CRAN_MIRROR` | `pkg.cran_mirror` |

### pak configuration entries

- ‘cache_dir’: (Env var: `PKG_CACHE_DIR`, option: `pkg.cache_dir`.)
  Directory to download the packages to. Defaults to a temporary
  directory within the R session temporary directory, see
  [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html).

- ‘cran_mirror’: (Env var: `PKG_CRAN_MIRROR`, option:
  `pkg.cran_mirror`.) CRAN mirror to use. Defaults to the `repos` option
  (see [`base::options()`](https://rdrr.io/r/base/options.html)), if
  that's not set then `https://cran.rstudio.com`. See also
  [`pak::repo_add()`](https://pak.r-lib.org/dev/reference/repo_add.md)
  and
  [`pak::repo_get()`](https://pak.r-lib.org/dev/reference/repo_get.md)

- ‘git_submodules’: (Env var: `PKG_GIT_SUBMODULES`, option:
  `pkg.git_submodules`.) Whether or not to update submodules in git
  repositories. This affects `git::` and `gitlab::` package sources
  only. If the R package is in a subdirectory then only the submodules
  within that directory are updated. If a submodule appears in
  `.Rbuildignore`, then it is skipped.

- ‘include_linkingto’: (Env var: `PKG_INCLUDE_LINKINGTO`, option:
  `pkg.include_linkingto`.) Whether to always include `LinkingTo`
  dependencies in the solution of and installation, even if they are
  needed because the packages are installed from binaries. This is
  sometimes useful, see e.g. <https://github.com/r-lib/pak/issues/485>
  for an example use case.

- ‘library’: (Env var: `PKG_LIBRARY`, option: `pkg.library`.) Package
  library to install packages to. It is also used for already installed
  packages when considering dependencies.

- ‘metadata_cache_dir’: (Env var: `PKG_METADATA_CACHE_DIR`, option:
  `pkg.metadata_cache_dir`.) Location of metadata replica of
  [`pkgcache::cranlike_metadata_cache`](https://r-lib.github.io/pkgcache/reference/cranlike_metadata_cache.html).
  Defaults to a temporary directory within the R session temporary
  directory, see
  [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html).

- ‘metadata_update_after’: (Env var: `PKG_METADATA_UPDATE_AFTER`,
  option: `pkg.metadata_update_after`.) A time interval as a
  [difftime](https://rdrr.io/r/base/difftime.html) object. pak will
  update the metadata cache if it is older than this. The default is one
  day. The `PKG_METADATA_UPDATE_AFTER` environment variable may be set
  in seconds (`s` suffix), minutes (`m` suffix), hours (`h` suffix), or
  days (`d` suffix). E.g: `1d` means one day.

- ‘package_cache_dir’: (Env var: `PKG_PACKAGE_CACHE_DIR`, option:
  `pkg.package_cache_dir`.) Location of the package cache on the disk.
  See
  [`pak::cache_summary()`](https://pak.r-lib.org/dev/reference/cache.md).
  Default is selected by pkgcache.

- ‘platforms’: (Env var: `PKG_PLATFORMS`, option: `pkg.platforms`.)
  Character vector of platforms to *download* or *install* packages for.
  See
  [`pkgdepends::default_platforms()`](https://r-lib.github.io/pkgdepends/reference/default_platforms.html)
  for possible platform names. Defaults to the platform of the current R
  session, plus `"source"`.

- ‘r_versions’: (Env var: `PKG_R_VERSIONS`, option: `pkg.r_versions`.)
  Character vector, R versions to download or install packages for. It
  defaults to the current R version.

- ‘sysreqs’: (Env var: `PKG_SYSREQS`, option: `pkg.sysreqs`.) Whether to
  automatically look up and install system requirements. If `TRUE`, then
  pkgdepends will try to install required system packages. If `FALSE`,
  then system requirements are still printed (including OS packages on
  supported platforms), but they are not installed. By default it is
  `TRUE` on supported platforms, if the current user is the root user or
  password-less `sudo` is configured for the current user.

- ‘sysreqs_db_update’: (Env var: `PKG_SYSREQS_DB_UPDATE`, option:
  `pkg.sysreqs_db_update`.) Whether to try to update the system
  requirements database from GitHub. If the update fails, then the
  cached or the build-in database if used. Defaults to TRUE.

- ‘sysreqs_db_update_timeout’: (Env var:
  `PKG_SYSREQS_DB_UPDATE_TIMEOUT`, option:
  `pkg.sysreqs_db_update_timeout`.) Timeout for the system requirements
  database update. Defaults to five seconds, except if the `CI`
  environment variable is set, then it is one minute.

- ‘sysreqs_dry_run’: (Env var: `PKG_SYSREQS_DRY_RUN`, option:
  `pkg.sysreqs_dry_run`.) If `TRUE`, then pak only prints the system
  commands to install system requirements, but does not execute them.

- ‘sysreqs_platform’: (Env var: `PKG_SYSREQS_PLATFORM`, option:
  `pkg.sysreqs_platform`.) The platform to use for system requirements
  lookup. On Linux, where system requirements are currently supported,
  it must be a string containing the distribution name and release,
  separated by a dash. E.g.: `"ubuntu-22.04"`, or `"rhel-9"`.

- ‘sysreqs_rspm_repo_id’: (Env var: `PKG_SYSREQS_RSPM_REPO_ID`, option:
  `pkg.sysreqs_rspm_repo_id`.) Posit Package Manager (formerly RStudio
  Package Manager) repository id to use for CRAN system requirements
  lookup. Defaults to the `RSPM_REPO_ID` environment variable, if set.
  If not set, then it defaults to `1`.

- ‘sysreqs_rspm_url’: (Env var: `PKG_SYSREQS_RSPM_URL`, option:
  `pkg.sysreqs_rspm_url`.) Root URL of Posit Package Manager (formerly
  RStudio Package Manager) for system requirements lookup. By default
  the `RSPM_ROOT` environment variable is used, if set. If not set, it
  defaults to `https://packagemanager.posit.co`.

- ‘sysreqs_sudo’: (Env var: `PKG_SYSREQS_SUDO`, option:
  `pkg.sysreqs_sudo`.) Whether to use `sudo` to install system
  requirements, on Unix. By default it is `TRUE` on Linux if the
  effective user id of the current process is not the `root` user.

- ‘sysreqs_update’: (Env var: `PKG_SYSREQS_UPDATE`, option:
  `pkg.sysreqs_update`.) Whether to try to update system packages that
  are already installed. It defaults to `TRUE` on CI systems: if the
  `CI` environment variable is set to `true`.

- ‘sysreqs_verbose’: (Env var: `PKG_SYSREQS_VERBOSE`, option:
  `pkg.sysreqs_verbose`.) Whether to echo the output of system
  requirements installation. Defaults to `TRUE` if the `CI` environment
  variable is set.

- ‘use_bioconductor’: (Env var: `PKG_USE_BIOCONDUCTOR`, option:
  `pkg.use_bioconductor`.) Whether to automatically use the Bioconductor
  repositories. Defaults to `TRUE`.

- ‘windows_archs’: (Env var: `PKG_WINDOWS_ARCHS`, option:
  `pkg.windows_archs`.) Character scalar specifying which architectures
  to download/install for on Windows. Its possible values are:

  - `"prefer-x64"`: Generally prefer x64 binaries. If the current R
    session is `x64`, then we download/install x64 packages. (These
    packages might still be multi-architecture binaries!) If the current
    R session is `i386`, then we download/install packages for both
    architectures. This might mean compiling packages from source if the
    binary packages are for `x64` only, like the CRAN Windows binaries
    for R 4.2.x currently. `"prefer-x64"` is the default for R 4.2.0 and
    later.

  - `"both"`: Always download/install packages for both `i386` and `x64`
    architectures. This might need compilation from source if the
    available binaries are for `x64` only, like the CRAN Windows
    binaries for R 4.2.x currently. `"both"` is the default for R 4.2.0
    and earlier.

### Notes

From version 0.4.0 pak copies the `PKG_*` environment variables and the
`pkg.*` options to the pak subprocess, where they are actually used, so
you don't need to restart R or reload pak after a configuration change.
