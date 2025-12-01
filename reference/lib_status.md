# Status of packages in a library

Status of packages in a library

## Usage

``` r
lib_status(lib = .libPaths()[1])

pkg_list(lib = .libPaths()[1])
```

## Arguments

- lib:

  Path to library.

## Value

Data frame the contains data about the packages installed in the
library. It has always has columns:

- `biocviews`: the corresponding field from `DESCRIPTION`, it must be
  present for all Bioconductor packages, other packages typically don't
  have it.

- `built`: the `Built` field from `DESCRIPTION`.

- `depends`, `suggests`, `Imports`, `linkingto`, `enhances`: the
  corresponding fields from the `DESCRIPTION` files.

- `deps`: A list or data frames, the dependencies of the package. It has
  columns: `ref`, `type` (dependency type in lowercase), `package`
  (dependent package, or `R`), `op` and `version`, for last two are for
  version requirement. `op` can be `>=`, `>`, `==` or `<=`, although the
  only the first one is common in practice.

- `library`: path to the package library containing the package.

- `license`: from `DESCRIPTION`.

- `md5sum`: from `DESCTIPTION`, typically `NA`, except on Windows.

- `needscompilation`: from `DESCRIPTION`, this column is logical.

- `package`: package name.

- `platform`: from the `Built` field in `DESCRIPTION`, the current
  platform if missing from `DESCRIPTION`.

- `priority`: from `DESCRIPTION`, usually `base`, `recommended`, or
  missing.

- `ref`: the corresponding `installed::*` package reference.

- `repository`: from `DESCRIPTION`. For packages from a CRAN repository
  this is `CRAN`, some other repositories, e.g. R-universe adds the
  repository URL here.

- `repotype`: `cran`, `bioc` or missing.

- `rversion`: from the `Built` field. If no such field, then the current
  R version.

- `sysreqs`: the `SystemRequirements` field from `DESCRIPTION`.

- `title`: package title.

- `type`: always `installed`.

- `version`: package version (as string).

Most of these columns are unchanged from `DESCRIPTION`, but pak also
adds a couple.

### Notes:

- In addition, it also has all `remote*` and `config/needs/*` entries
  from the `DESCRIPTION` files. (Case insensitive.)

- All columns are of type `character`, except for `needscompilation`,
  which is logical and `deps`, which is a list columns.

- If an entry is missing for a package, it is set to `NA`.

- Note that column names are lowercase, even if the corresponding
  entries are not in `DESCRIPTION`.

- The order of the columns is not deterministic, so don't assume any
  order.

- Additional columns might be present, these are internal for pak and
  should not be used in user code.

## Examples

    lib_status(.Library)

    #> # A data frame: 31 × 31
    #>    library   package version prior…¹ title license sugge…² built depends
    #>    <chr>     <chr>   <chr>   <chr>   <chr> <chr>   <chr>   <chr> <chr>
    #>  1 /Library… base    4.2.2   base    "The… Part o… methods R 4.… NA
    #>  2 /Library… boot    1.3-28  recomm… "Boo… Unlimi… MASS, … R 4.… R (>= …
    #>  3 /Library… class   7.3-20  recomm… "Fun… GPL-2 … NA      R 4.… R (>= …
    #>  4 /Library… cluster 2.1.4   recomm… "\"F… GPL (>… MASS, … R 4.… R (>= …
    #>  5 /Library… codeto… 0.2-18  recomm… "Cod… GPL     NA      R 4.… R (>= …
    #>  6 /Library… compil… 4.2.2   base    "The… Part o… NA      R 4.… NA
    #>  7 /Library… datase… 4.2.2   base    "The… Part o… NA      R 4.… NA
    #>  8 /Library… filelo… 1.0.2   NA      "Por… MIT + … callr … R 4.… NA
    #>  9 /Library… foreign 0.8-83  recomm… "Rea… GPL (>… NA      R 4.… R (>= …
    #> 10 /Library… graphi… 4.2.2   base    "The… Part o… NA      R 4.… NA
    #> # … with 21 more rows, 22 more variables: needscompilation <lgl>,
    #> #   repository <chr>, imports <chr>, remotetype <chr>,
    #> #   remotepkgref <chr>, remoteref <chr>, remoterepos <chr>,
    #> #   remotepkgplatform <chr>, remotesha <chr>, enhances <chr>,
    #> #   linkingto <chr>, md5sum <chr>, platform <chr>, biocviews <chr>,
    #> #   sysreqs <chr>, ref <chr>, type <chr>, status <chr>, rversion <chr>,
    #> #   sources <list>, repotype <chr>, deps <list>, and abbreviated …

## See also

Other package functions:
[`pak()`](https://pak.r-lib.org/reference/pak.md),
[`pkg_deps_tree()`](https://pak.r-lib.org/reference/pkg_deps_tree.md),
[`pkg_deps()`](https://pak.r-lib.org/reference/pkg_deps.md),
[`pkg_download()`](https://pak.r-lib.org/reference/pkg_download.md),
[`pkg_install()`](https://pak.r-lib.org/reference/pkg_install.md),
[`pkg_remove()`](https://pak.r-lib.org/reference/pkg_remove.md),
[`pkg_status()`](https://pak.r-lib.org/reference/pkg_status.md),
[`pkg_sysreqs()`](https://pak.r-lib.org/reference/pkg_sysreqs.md)
