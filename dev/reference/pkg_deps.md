# Look up the dependencies of a package

Look up the dependencies of a package

## Usage

``` r
pkg_deps(pkg, upgrade = TRUE, dependencies = NA)
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

- upgrade:

  Whether to use the most recent available package versions.

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

## Value

A data frame with the dependency data, it includes `pkg` as well. It has
the following columns.

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

Additional columns might be present. They are either used internally or
they are experimental. They might be removed or changed at any time.All
columns are of type character, except for `direct` (logical),
`needscompilation` (logical), `filesize` (integer), `deps` (list column,
see "Package dependency tables" below), `sources` (list of character
vectors), `remote` (list), `error` (list), `metadata` (list),
`dep_types` (list).

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

    pkg_deps("dplyr")

    #> # A data frame: 16 × 32
    #>    ref       type  direct direc…¹ status package version license needs…²
    #>    <chr>     <chr> <lgl>  <lgl>   <chr>  <chr>   <chr>   <chr>   <lgl>
    #>  1 R6        stan… FALSE  FALSE   OK     R6      2.5.1   MIT + … FALSE
    #>  2 cli       stan… FALSE  FALSE   OK     cli     3.4.1   MIT + … FALSE
    #>  3 dplyr     stan… TRUE   TRUE    OK     dplyr   1.0.10  MIT + … FALSE
    #>  4 fansi     stan… FALSE  FALSE   OK     fansi   1.0.3   GPL-2 … FALSE
    #>  5 generics  stan… FALSE  FALSE   OK     generi… 0.1.3   MIT + … FALSE
    #>  6 glue      stan… FALSE  FALSE   OK     glue    1.6.2   MIT + … FALSE
    #>  7 lifecycle stan… FALSE  FALSE   OK     lifecy… 1.0.3   MIT + … FALSE
    #>  8 magrittr  stan… FALSE  FALSE   OK     magrit… 2.0.3   MIT + … FALSE
    #>  9 pillar    stan… FALSE  FALSE   OK     pillar  1.8.1   MIT + … FALSE
    #> 10 pkgconfig stan… FALSE  FALSE   OK     pkgcon… 2.0.3   MIT + … FALSE
    #> 11 rlang     stan… FALSE  FALSE   OK     rlang   1.0.6   MIT + … FALSE
    #> 12 tibble    stan… FALSE  FALSE   OK     tibble  3.1.8   MIT + … FALSE
    #> 13 tidysele… stan… FALSE  FALSE   OK     tidyse… 1.2.0   MIT + … FALSE
    #> 14 utf8      stan… FALSE  FALSE   OK     utf8    1.2.2   Apache… FALSE
    #> 15 vctrs     stan… FALSE  FALSE   OK     vctrs   0.5.1   MIT + … FALSE
    #> 16 withr     stan… FALSE  FALSE   OK     withr   2.5.0   MIT + … FALSE
    #> # … with 23 more variables: priority <chr>, md5sum <chr>, sha256 <chr>,
    #> #   filesize <int>, built <chr>, platform <chr>, rversion <chr>,
    #> #   repotype <chr>, repodir <chr>, target <chr>, deps <list>,
    #> #   mirror <chr>, sources <list>, remote <list>, error <list>,
    #> #   metadata <list>, dep_types <list>, params <list>, sysreqs <chr>,
    #> #   cache_status <chr>, lib_status <chr>, old_version <chr>,
    #> #   new_version <chr>, and abbreviated variable names ¹​directpkg, …

For a package on GitHub:

    pkg_deps("r-lib/callr")

    #> # A data frame: 4 × 32
    #>   ref        type  direct direc…¹ status package version license needs…²
    #>   <chr>      <chr> <lgl>  <lgl>   <chr>  <chr>   <chr>   <chr>   <lgl>
    #> 1 r-lib/cal… gith… TRUE   TRUE    OK     callr   3.7.3.… MIT + … TRUE
    #> 2 R6         stan… FALSE  FALSE   OK     R6      2.5.1   MIT + … FALSE
    #> 3 processx   stan… FALSE  FALSE   OK     proces… 3.8.0   MIT + … FALSE
    #> 4 ps         stan… FALSE  FALSE   OK     ps      1.7.2   MIT + … FALSE
    #> # … with 23 more variables: priority <chr>, md5sum <chr>, sha256 <chr>,
    #> #   filesize <int>, built <chr>, platform <chr>, rversion <chr>,
    #> #   repotype <chr>, repodir <chr>, target <chr>, deps <list>,
    #> #   mirror <chr>, sources <list>, remote <list>, error <list>,
    #> #   metadata <list>, dep_types <list>, params <list>, sysreqs <chr>,
    #> #   cache_status <chr>, lib_status <chr>, old_version <chr>,
    #> #   new_version <chr>, and abbreviated variable names ¹​directpkg, …

## See also

Other package functions:
[`lib_status()`](https://pak.r-lib.org/dev/reference/lib_status.md),
[`pak()`](https://pak.r-lib.org/dev/reference/pak.md),
[`pkg_deps_tree()`](https://pak.r-lib.org/dev/reference/pkg_deps_tree.md),
[`pkg_download()`](https://pak.r-lib.org/dev/reference/pkg_download.md),
[`pkg_install()`](https://pak.r-lib.org/dev/reference/pkg_install.md),
[`pkg_remove()`](https://pak.r-lib.org/dev/reference/pkg_remove.md),
[`pkg_status()`](https://pak.r-lib.org/dev/reference/pkg_status.md),
[`pkg_sysreqs()`](https://pak.r-lib.org/dev/reference/pkg_sysreqs.md)
