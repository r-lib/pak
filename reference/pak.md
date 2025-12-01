# Install specified required packages

Install the specified packages, or the ones required by the package or
project in the current working directory.

## Usage

``` r
pak(pkg = NULL, ...)
```

## Arguments

- pkg:

  Package names or remote package specifications to install. See [pak
  package
  sources](https://pak.r-lib.org/reference/pak_package_sources.md) for
  details. If `NULL`, will install all development dependencies for the
  current package.

- ...:

  Extra arguments are passed to
  [`pkg_install()`](https://pak.r-lib.org/reference/pkg_install.md) or
  [`local_install_dev_deps()`](https://pak.r-lib.org/reference/local_install_dev_deps.md).

## Details

This is a convenience function:

- If you want to install some packages, it is easier to type than
  [`pkg_install()`](https://pak.r-lib.org/reference/pkg_install.md).

- If you want to install all the packages that are needed for the
  development of a package or project, then it is easier to type than
  [`local_install_dev_deps()`](https://pak.r-lib.org/reference/local_install_dev_deps.md).

- You don't need to remember two functions to install packages, just
  one.

## See also

Other package functions:
[`lib_status()`](https://pak.r-lib.org/reference/lib_status.md),
[`pkg_deps_tree()`](https://pak.r-lib.org/reference/pkg_deps_tree.md),
[`pkg_deps()`](https://pak.r-lib.org/reference/pkg_deps.md),
[`pkg_download()`](https://pak.r-lib.org/reference/pkg_download.md),
[`pkg_install()`](https://pak.r-lib.org/reference/pkg_install.md),
[`pkg_remove()`](https://pak.r-lib.org/reference/pkg_remove.md),
[`pkg_status()`](https://pak.r-lib.org/reference/pkg_status.md),
[`pkg_sysreqs()`](https://pak.r-lib.org/reference/pkg_sysreqs.md)

Other local package trees:
[`local_deps_explain()`](https://pak.r-lib.org/reference/local_deps_explain.md),
[`local_deps()`](https://pak.r-lib.org/reference/local_deps.md),
[`local_install_deps()`](https://pak.r-lib.org/reference/local_install_deps.md),
[`local_install_dev_deps()`](https://pak.r-lib.org/reference/local_install_dev_deps.md),
[`local_install()`](https://pak.r-lib.org/reference/local_install.md),
[`local_package_trees`](https://pak.r-lib.org/reference/local_package_trees.md)
