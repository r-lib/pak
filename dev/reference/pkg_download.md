# Download a package and its dependencies

TODO: explain result

## Usage

``` r
pkg_download(
  pkg,
  dest_dir = ".",
  dependencies = FALSE,
  platforms = NULL,
  r_versions = NULL
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
  sources](https://pak.r-lib.org/dev/reference/pak_package_sources.md)"
  for more details.

- dest_dir:

  Destination directory for the packages. If it does not exist, then it
  will be created.

- dependencies:

  What kinds of dependencies to install. Most commonly one of the
  following values:

  - `NA`: only required (hard) dependencies,

  - `TRUE`: required dependencies plus optional and development
    dependencies,

  - `FALSE`: do not install any dependencies. (You might end up with a
    non-working package, and/or the installation might fail.) See
    [Package dependency
    types](https://pak.r-lib.org/dev/reference/package-dependency-types.md)
    for other possible values and more information about package
    dependencies.

- platforms:

  Types of binary or source packages to download. The default is the
  value of
  [`pkgdepends::default_platforms()`](https://r-lib.github.io/pkgdepends/reference/default_platforms.html).

- r_versions:

  R version(s) to download packages for. (This does not matter for
  source packages, but it does for binaries.) It defaults to the current
  R version.

## Value

Data frame with information about the downloaded packages, invisibly.
Columns:

- `built`: the `Built` field from the `DESCRIPTION` file of binary
  packages, for which this information is available.

- `cache_status`: whether the package file is in the package cache. It
  is `NA` for `installed::` package refs.

- `dep_types`: character vector of dependency types that were considered
  for this package. (This is a list column.)

- `deps`: dependencies of the package, in a data frame. See "Package
  dependency tables" below.

- `direct`: whether this package (ref, really) was directly specified,
  or added as a dependency.

- `error`: this is a list column that contains error objects for the
  refs that pkgdepends failed to resolve.

- `filesize`: the file size in bytes, or `NA` if this information is not
  available.

- `license`: license of the package, or `NA` if not available.

- `md5sum`: MD5 checksum of the package file, if available, or `NA` if
  not.

- `metadata`: a named character vector. These fields will be (should be)
  added to the installed `DESCRIPTION` file of the package.

- `mirror`: URL of the CRAN(-like) mirror site where the metadata was
  obtained from. It is NA for non-CRAN-like sources, e.g. local files,
  installed packages, GitHub, etc.

- `needscompilation`: whether the package needs compilation.

- `package`: package name.

- `priority`: this is `"base"` for base packages, `"recommended"` for
  recommended packages, and `NA` otherwise.

- `ref`: package reference.

- `remote`: the parsed `remote_ref` objects, see `parse_pkg_refs()`.
  This is a list column.

- `repodir`: the directory where this package should be in a CRAN-like
  repository.

- `sha256`: SHA256 hash of the package file, if available, otherwise
  `NA`.

- `sources`: URLs where this package can be downloaded from. This is not
  necessarily a URL that you can download with a HTTP client. E.g. for
  `local::` refs it is a path, and for `git::` refs it is a URL for git.
  It is a zero length vector for `installed::` refs.

- `status`: status of the dependency resolution, `"OK"` or `"FAILED"`.

- `target`: path where this package should be saved in a
  CRAN-repository.

- `type`: ref type.

- `version`: package version.

- `fulltarget`: absolute path to the downloaded file. At most one of
  `fulltarget` and `fulltarget_tree` must exist on the disk.

- `fulltarget_tree`: absolute path to a package tree directory. At most
  one of `fulltarget` and `fulltarget_tree` must exist on the disk.

- `download_status`: `"Had"` or `"Got"`, depending on whether the file
  was obtained from the cache.

- `download_error`: error object for failed downloads.

- `file_size`: Size of the file, or `NA`. For `installed::` refs, it is
  `NA`, and it is also `NA` for refs that created `fulltarget_tree`
  instead of `fulltarget`.

`fulltarget`, if it exists, contains a packaged (via `R CMD build`)
source R package. If `fulltarget_tree` exists, it is a package tree
directory, that still needs an `R CMD build` call.Additional columns
might be present. They are either used internally or they are
experimental. They might be removed or changed at any time.All columns
are of type character, except for `direct` (logical), `needscompilation`
(logical), `filesize` (integer), `deps` (list column, see "Package
dependency tables" below), `sources` (list of character vectors),
`remote` (list), `error` (list), `metadata` (list), `dep_types` (list).

### Package dependency tables

A package dependency tables in the `deps` list column have five columns
currently:

- `ref`: the package ref of the dependency.

- `type`: the dependency type, in all lowercase. I.e. `imports`,
  `suggests`, etc.

- `package`: package name of the dependency.

- `op`: operator for version requirements, e.g. `>=`.

- `version`: version number, for version requirements.

## Examples

    dl <- pkg_download("forcats")

    #> i No downloads are needed, 2 pkgs (641.53 kB) are cached

    dl

    #> # A data frame: 2 × 35
    #>   ref     type     direct direc…¹ status package version license needs…²
    #>   <chr>   <chr>    <lgl>  <lgl>   <chr>  <chr>   <chr>   <chr>   <lgl>
    #> 1 forcats standard TRUE   TRUE    OK     forcats 0.5.2   MIT + … FALSE
    #> 2 forcats standard TRUE   TRUE    OK     forcats 0.5.2   MIT + … FALSE
    #> # … with 26 more variables: priority <chr>, md5sum <chr>, sha256 <chr>,
    #> #   filesize <int>, built <chr>, platform <chr>, rversion <chr>,
    #> #   repotype <chr>, repodir <chr>, target <chr>, deps <list>,
    #> #   mirror <chr>, sources <list>, remote <list>, error <list>,
    #> #   metadata <list>, extra <list>, dep_types <list>, params <list>,
    #> #   sysreqs <chr>, cache_status <chr>, fulltarget <chr>,
    #> #   fulltarget_tree <chr>, download_status <chr>, …

    dl$fulltarget

    #> [1] "./bin/macosx/big-sur-arm64/contrib/4.2/forcats_0.5.2.tgz"
    #> [2] "./src/contrib/forcats_0.5.2.tar.gz"

    pkg_download("r-lib/pak", platforms = "source")

    #> i No downloads are needed, 1 pkg is cached

## See also

Other package functions:
[`lib_status()`](https://pak.r-lib.org/dev/reference/lib_status.md),
[`pak()`](https://pak.r-lib.org/dev/reference/pak.md),
[`pkg_deps_tree()`](https://pak.r-lib.org/dev/reference/pkg_deps_tree.md),
[`pkg_deps()`](https://pak.r-lib.org/dev/reference/pkg_deps.md),
[`pkg_install()`](https://pak.r-lib.org/dev/reference/pkg_install.md),
[`pkg_remove()`](https://pak.r-lib.org/dev/reference/pkg_remove.md),
[`pkg_status()`](https://pak.r-lib.org/dev/reference/pkg_status.md),
[`pkg_sysreqs()`](https://pak.r-lib.org/dev/reference/pkg_sysreqs.md)
