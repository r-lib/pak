# Remove installed packages

Remove installed packages

## Usage

``` r
pkg_remove(pkg, lib = .libPaths()[[1L]])
```

## Arguments

- pkg:

  A character vector of packages to remove.

- lib:

  library to remove packages from.

## Value

Nothing.

## See also

Other package functions:
[`lib_status()`](https://pak.r-lib.org/dev/reference/lib_status.md),
[`pak()`](https://pak.r-lib.org/dev/reference/pak.md),
[`pkg_deps_tree()`](https://pak.r-lib.org/dev/reference/pkg_deps_tree.md),
[`pkg_deps()`](https://pak.r-lib.org/dev/reference/pkg_deps.md),
[`pkg_download()`](https://pak.r-lib.org/dev/reference/pkg_download.md),
[`pkg_install()`](https://pak.r-lib.org/dev/reference/pkg_install.md),
[`pkg_status()`](https://pak.r-lib.org/dev/reference/pkg_status.md),
[`pkg_sysreqs()`](https://pak.r-lib.org/dev/reference/pkg_sysreqs.md)
