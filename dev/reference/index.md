# Package index

## About pak

- [`Installing pak`](https://pak.r-lib.org/dev/reference/install.md) :
  All about installing pak.
- [`Great pak features`](https://pak.r-lib.org/dev/reference/features.md)
  : A list of the most important pak features
- [`Package sources`](https://pak.r-lib.org/dev/reference/pak_package_sources.md)
  : Install packages from CRAN, Bioconductor, GitHub, URLs, etc.
- [`The dependency solver`](https://pak.r-lib.org/dev/reference/pak_solver.md)
  : Find the ideal set of packages and versions to install
- [`Package dependency types`](https://pak.r-lib.org/dev/reference/package-dependency-types.md)
  : Various types of R package dependencies
- [`System requirements`](https://pak.r-lib.org/dev/reference/sysreqs.md)
  [`sysreqs`](https://pak.r-lib.org/dev/reference/sysreqs.md) : System
  requirements
- [`Authenticated repositories`](https://pak.r-lib.org/dev/reference/repo-auth.md)
  : Authenticated repositories
- [`pak configuration`](https://pak.r-lib.org/dev/reference/pak-config.md)
  [`pak-config`](https://pak.r-lib.org/dev/reference/pak-config.md) :
  Environment variables and options that modify the default behavior

## Packages

### Installation

- [`pkg_install()`](https://pak.r-lib.org/dev/reference/pkg_install.md)
  : Install packages
- [`pak()`](https://pak.r-lib.org/dev/reference/pak.md) : Install
  specified required packages
- [`pkg_status()`](https://pak.r-lib.org/dev/reference/pkg_status.md) :
  Display installed locations of a package
- [`pkg_remove()`](https://pak.r-lib.org/dev/reference/pkg_remove.md) :
  Remove installed packages

### Dependencies

- [`pkg_deps()`](https://pak.r-lib.org/dev/reference/pkg_deps.md) : Look
  up the dependencies of a package
- [`pkg_deps_explain()`](https://pak.r-lib.org/dev/reference/pkg_deps_explain.md)
  : Explain how a package depends on other packages
- [`pkg_deps_tree()`](https://pak.r-lib.org/dev/reference/pkg_deps_tree.md)
  : Draw the dependency tree of a package
- [`scan_deps()`](https://pak.r-lib.org/dev/reference/scan_deps.md) :
  Scan R code for dependent packages

### Download

- [`pkg_download()`](https://pak.r-lib.org/dev/reference/pkg_download.md)
  : Download a package and its dependencies

### Metadata

- [`pkg_history()`](https://pak.r-lib.org/dev/reference/pkg_history.md)
  : Query the history of a CRAN package
- [`pkg_name_check()`](https://pak.r-lib.org/dev/reference/pkg_name_check.md)
  : Check if an R package name is available
- [`pkg_search()`](https://pak.r-lib.org/dev/reference/pkg_search.md) :
  Search CRAN packages

## Local Package Trees

- [`local_package_trees`](https://pak.r-lib.org/dev/reference/local_package_trees.md)
  : About local package trees
- [`local_deps()`](https://pak.r-lib.org/dev/reference/local_deps.md)
  [`local_deps_tree()`](https://pak.r-lib.org/dev/reference/local_deps.md)
  [`local_dev_deps()`](https://pak.r-lib.org/dev/reference/local_deps.md)
  [`local_dev_deps_tree()`](https://pak.r-lib.org/dev/reference/local_deps.md)
  : Dependencies of a package tree
- [`local_deps_explain()`](https://pak.r-lib.org/dev/reference/local_deps_explain.md)
  [`local_dev_deps_explain()`](https://pak.r-lib.org/dev/reference/local_deps_explain.md)
  : Explain dependencies of a package tree
- [`local_install()`](https://pak.r-lib.org/dev/reference/local_install.md)
  : Install a package tree
- [`local_install_deps()`](https://pak.r-lib.org/dev/reference/local_install_deps.md)
  : Install the dependencies of a package tree
- [`local_install_dev_deps()`](https://pak.r-lib.org/dev/reference/local_install_dev_deps.md)
  : Install all (development) dependencies of a package tree

## Libraries

- [`lib_status()`](https://pak.r-lib.org/dev/reference/lib_status.md)
  [`pkg_list()`](https://pak.r-lib.org/dev/reference/lib_status.md) :
  Status of packages in a library

## System requirements

- [`pkg_sysreqs()`](https://pak.r-lib.org/dev/reference/pkg_sysreqs.md)
  : Calculate system requirements of one of more packages
- [`sysreqs_check_installed()`](https://pak.r-lib.org/dev/reference/sysreqs_check_installed.md)
  [`sysreqs_fix_installed()`](https://pak.r-lib.org/dev/reference/sysreqs_check_installed.md)
  : Check if installed packages have all their system requirements
- [`sysreqs_db_list()`](https://pak.r-lib.org/dev/reference/sysreqs_db_list.md)
  : List contents of the system requirements DB, for a platform
- [`sysreqs_db_match()`](https://pak.r-lib.org/dev/reference/sysreqs_db_match.md)
  : Match system requirement descriptions to the database
- [`sysreqs_db_update()`](https://pak.r-lib.org/dev/reference/sysreqs_db_update.md)
  : Update the cached copy of the system requirements database
- [`sysreqs_is_supported()`](https://pak.r-lib.org/dev/reference/sysreqs_is_supported.md)
  : Check if a platform has system requirements support
- [`sysreqs_list_system_packages()`](https://pak.r-lib.org/dev/reference/sysreqs_list_system_packages.md)
  : List installed system packages
- [`sysreqs_platforms()`](https://pak.r-lib.org/dev/reference/sysreqs_platforms.md)
  : List platforms with system requirements support

## Repositories

- [`ppm_has_binaries()`](https://pak.r-lib.org/dev/reference/ppm_has_binaries.md)
  : Does PPM build binary packages for the current platform?
- [`ppm_platforms()`](https://pak.r-lib.org/dev/reference/ppm_platforms.md)
  : List all platforms supported by Posit Package Manager (PPM)
- [`ppm_r_versions()`](https://pak.r-lib.org/dev/reference/ppm_r_versions.md)
  : List all R versions supported by Posit Package Manager (PPM)
- [`ppm_repo_url()`](https://pak.r-lib.org/dev/reference/ppm_repo_url.md)
  : Returns the current Posit Package Manager (PPM) repository URL
- [`ppm_snapshots()`](https://pak.r-lib.org/dev/reference/ppm_snapshots.md)
  : List all available Posit Package Manager (PPM) snapshots
- [`repo_add()`](https://pak.r-lib.org/dev/reference/repo_add.md)
  [`repo_resolve()`](https://pak.r-lib.org/dev/reference/repo_add.md) :
  Add a new CRAN-like repository
- [`repo_auth()`](https://pak.r-lib.org/dev/reference/repo_auth.md) :
  Authenticated repositories
- [`repo_auth_key_get()`](https://pak.r-lib.org/dev/reference/repo_auth_key_get.md)
  [`repo_auth_key_set()`](https://pak.r-lib.org/dev/reference/repo_auth_key_get.md)
  [`repo_auth_unlock()`](https://pak.r-lib.org/dev/reference/repo_auth_key_get.md)
  : Query or set repository password in the system credential store
- [`repo_get()`](https://pak.r-lib.org/dev/reference/repo_get.md) :
  Query the currently configured CRAN-like repositories
- [`repo_status()`](https://pak.r-lib.org/dev/reference/repo_status.md)
  [`repo_ping()`](https://pak.r-lib.org/dev/reference/repo_status.md) :
  Show the status of CRAN-like repositories

## Auto-install missing packages

- [`handle_package_not_found()`](https://pak.r-lib.org/dev/reference/handle_package_not_found.md)
  : Install missing packages on the fly

## Lock files

- [`lockfile_create()`](https://pak.r-lib.org/dev/reference/lockfile_create.md)
  : Create a lock file
- [`lockfile_install()`](https://pak.r-lib.org/dev/reference/lockfile_install.md)
  : Install packages based on a lock file

## System information

- [`system_r_platform()`](https://pak.r-lib.org/dev/reference/system_r_platform.md)
  [`system_r_platform_data()`](https://pak.r-lib.org/dev/reference/system_r_platform.md)
  : R platforms

## pak housekeeping

- [`cache_summary()`](https://pak.r-lib.org/dev/reference/cache.md)
  [`cache_list()`](https://pak.r-lib.org/dev/reference/cache.md)
  [`cache_delete()`](https://pak.r-lib.org/dev/reference/cache.md)
  [`cache_clean()`](https://pak.r-lib.org/dev/reference/cache.md) :
  Package cache utilities
- [`meta_summary()`](https://pak.r-lib.org/dev/reference/metadata.md)
  [`meta_list()`](https://pak.r-lib.org/dev/reference/metadata.md)
  [`meta_update()`](https://pak.r-lib.org/dev/reference/metadata.md)
  [`meta_clean()`](https://pak.r-lib.org/dev/reference/metadata.md) :
  Metadata cache utilities
- [`pak_install_extra()`](https://pak.r-lib.org/dev/reference/pak_install_extra.md)
  : Install all optional dependencies of pak
- [`pak_cleanup()`](https://pak.r-lib.org/dev/reference/pak_cleanup.md)
  : Clean up pak caches
- [`pak_sitrep()`](https://pak.r-lib.org/dev/reference/pak_sitrep.md) :
  pak SITuation REPort
- [`pak_update()`](https://pak.r-lib.org/dev/reference/pak_update.md) :
  Update pak itself
