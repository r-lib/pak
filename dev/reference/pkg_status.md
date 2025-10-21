# Display installed locations of a package

Display installed locations of a package

## Usage

``` r
pkg_status(pkg, lib = .libPaths())
```

## Arguments

- pkg:

  Name of one or more installed packages to display status for.

- lib:

  One or more library paths to lookup packages status in. By default all
  libraries are used.

## Value

Data frame with data about installations of `pkg`. It has always has
columns:

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

    pkg_status("MASS")

    #> # A data frame: 2 × 39
    #>   library    package title version depends repos…¹ license needs…² built
    #> * <chr>      <chr>   <chr> <chr>   <chr>   <chr>   <chr>   <lgl>   <chr>
    #> 1 /Users/ga… MASS    Supp… 7.3-58… R (>= … CRAN    GPL-2 … TRUE    R 4.…
    #> 2 /Library/… MASS    Supp… 7.3-58… R (>= … CRAN    GPL-2 … TRUE    R 4.…
    #> # … with 30 more variables: remotetype <chr>, remotepkgref <chr>,
    #> #   remoteref <chr>, remoterepos <chr>, remotepkgplatform <chr>,
    #> #   remotesha <chr>, imports <chr>, suggests <chr>, linkingto <chr>,
    #> #   remotes <chr>, remotehost <chr>, remoterepo <chr>,
    #> #   remoteusername <chr>, enhances <chr>, biocviews <chr>,
    #> #   remoteurl <chr>, remotesubdir <chr>, priority <chr>,
    #> #   remoteetag <chr>, remotepackaged <chr>, md5sum <chr>, …

## See also

Other package functions:
[`lib_status()`](https://pak.r-lib.org/dev/reference/lib_status.md),
[`pak()`](https://pak.r-lib.org/dev/reference/pak.md),
[`pkg_deps_tree()`](https://pak.r-lib.org/dev/reference/pkg_deps_tree.md),
[`pkg_deps()`](https://pak.r-lib.org/dev/reference/pkg_deps.md),
[`pkg_download()`](https://pak.r-lib.org/dev/reference/pkg_download.md),
[`pkg_install()`](https://pak.r-lib.org/dev/reference/pkg_install.md),
[`pkg_remove()`](https://pak.r-lib.org/dev/reference/pkg_remove.md),
[`pkg_sysreqs()`](https://pak.r-lib.org/dev/reference/pkg_sysreqs.md)
