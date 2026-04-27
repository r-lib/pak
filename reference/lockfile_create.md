# Create a lock file

The lock file can be used later, possibly in a new R session, to carry
out the installation of the dependencies, with
[`lockfile_install()`](https://pak.r-lib.org/reference/lockfile_install.md).

## Usage

``` r
lockfile_create(
  pkg = "deps::.",
  lockfile = "pkg.lock",
  lib = NULL,
  upgrade = FALSE,
  dependencies = NA
)
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

- lockfile:

  Path to the lock file.

- lib:

  Character vector of library paths, or `NULL`. Used when resolving
  package dependencies: packages already installed in any of these paths
  are considered satisfied. If `NULL` (the default), an empty temporary
  library is used, so all dependencies are resolved from scratch
  regardless of what is currently installed.

- upgrade:

  When `FALSE`, the default, pak does the minimum amount of work to give
  you the latest version(s) of `pkg`. It will only upgrade dependent
  packages if `pkg`, or one of their dependencies explicitly require a
  higher version than what you currently have. It will also prefer a
  binary package over to source package, even if the binary package is
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

## Details

Note, since the URLs of CRAN and most CRAN-like repositories change over
time, in practice you cannot use the lock file *much* later. For
example, binary packages of older package version might be deleted from
the repository, breaking the URLs in the lock file.

Currently the intended use case of lock files in on CI systems, to
facilitate caching. The (hash of the) lock file provides a good key for
caching systems.

## See also

Other lock files:
[`lockfile_install()`](https://pak.r-lib.org/reference/lockfile_install.md)
