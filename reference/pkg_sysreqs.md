# Calculate system requirements of one of more packages

Calculate system requirements of one of more packages

## Usage

``` r
pkg_sysreqs(pkg, upgrade = TRUE, dependencies = NA, sysreqs_platform = NULL)
```

## Arguments

- pkg:

  Package names or package references. E.g.

  - `ggplot2`: package from CRAN, Bioconductor or a CRAN-like repository
    in general,

  - `tidyverse/ggplot2`: package from GitHub,

  - `tidyverse/ggplot2@v3.4.0`: package from GitHub tag or branch,

  - `https://examples.com/.../ggplot2_3.3.6.tar.gz`: package from URL,

  - `.`: package in the current working directory.

  See "[Package
  sources](https://pak.r-lib.org/reference/pak_package_sources.md)" for
  more details.

- upgrade:

  When `FALSE`, the default, pak does the minimum amount of work to give
  you the latest version(s) of `pkg`. It will only upgrade dependent
  packages if `pkg`, or one of their dependencies explicitly require a
  higher version than what you currently have. It will also prefer a
  binary package over to source package, even it the binary package is
  older.

  When `upgrade = TRUE`, pak will ensure that you have the latest
  version(s) of `pkg` and all their dependencies.

- dependencies:

  What kinds of dependencies to install. Most commonly one of the
  following values:

  - `NA`: only required (hard) dependencies,

  - `TRUE`: required dependencies plus optional and development
    dependencies,

  - `FALSE`: do not install any dependencies. (You might end up with a
    non-working package, and/or the installation might fail.) See
    [Package dependency
    types](https://pak.r-lib.org/reference/package-dependency-types.md)
    for other possible values and more information about package
    dependencies.

- sysreqs_platform:

  System requirements platform.

  If `NULL`, then the `sysreqs_platform` [configuration
  option](https://pak.r-lib.org/reference/pak-config.md) is used, which
  defaults to the current platform.

  Set this option if to one of `"alpine"`, `"centos"`, `"debian"`,
  `"fedora"`, `"opensuse"`, `"opensuse"`, `"opensuse"`, `"redhat"`,
  `"redhat"`, `"redhat"`, `"redhat"`, `"redhat"`, `"redhat"`,
  `"rockylinux"`, `"rockylinux"`, `"rockylinux"`, `"sle"`, `"sle"`,
  `"ubuntu"` if pak fails to correctly detect your platform or if you
  want to see the system requirements for a different platform.

## Value

List with entries:

- `os`: character string. Operating system.

- `distribution`: character string. Linux distribution, `NA` if the OS
  is not Linux.

- `version`: character string. Distribution version, `NA` is the OS is
  not Linux.

- `pre_install`: character vector. Commands to run before the
  installation of system packages.

- `install_scripts`: character vector. Commands to run to install the
  system packages.

- `post_install`: character vector. Commands to run after the
  installation of system packages.

- `packages`: data frame. Information about the system packages that are
  needed. It has columns:

  - `sysreq`: string, cross-platform name of the system requirement.

  - `packages`: list column of character vectors. The names of the R
    packages that have this system requirement.

  - `pre_install`: list column of character vectors. Commands run before
    the package installation for this system requirement.

  - `system_packages`: list column of character vectors. Names of system
    packages to install.

  - `post_install`: list column of character vectors. Commands run after
    the package installation for this system requirement.

## See also

Other package functions:
[`lib_status()`](https://pak.r-lib.org/reference/lib_status.md),
[`pak()`](https://pak.r-lib.org/reference/pak.md),
[`pkg_deps()`](https://pak.r-lib.org/reference/pkg_deps.md),
[`pkg_deps_tree()`](https://pak.r-lib.org/reference/pkg_deps_tree.md),
[`pkg_download()`](https://pak.r-lib.org/reference/pkg_download.md),
[`pkg_install()`](https://pak.r-lib.org/reference/pkg_install.md),
[`pkg_remove()`](https://pak.r-lib.org/reference/pkg_remove.md),
[`pkg_status()`](https://pak.r-lib.org/reference/pkg_status.md)

Other system requirements functions:
[`sysreqs_check_installed()`](https://pak.r-lib.org/reference/sysreqs_check_installed.md),
[`sysreqs_db_list()`](https://pak.r-lib.org/reference/sysreqs_db_list.md),
[`sysreqs_db_match()`](https://pak.r-lib.org/reference/sysreqs_db_match.md),
[`sysreqs_db_update()`](https://pak.r-lib.org/reference/sysreqs_db_update.md),
[`sysreqs_is_supported()`](https://pak.r-lib.org/reference/sysreqs_is_supported.md),
[`sysreqs_list_system_packages()`](https://pak.r-lib.org/reference/sysreqs_list_system_packages.md),
[`sysreqs_platforms()`](https://pak.r-lib.org/reference/sysreqs_platforms.md)
