# Explain dependencies of a package tree

These functions are similar to
[`pkg_deps_explain()`](https://pak.r-lib.org/dev/reference/pkg_deps_explain.md),
but work on a local package tree. `local_dev_deps_explain()` also
includes development dependencies.

## Usage

``` r
local_deps_explain(deps, root = ".", upgrade = TRUE, dependencies = NA)

local_dev_deps_explain(deps, root = ".", upgrade = TRUE, dependencies = TRUE)
```

## Arguments

- deps:

  Package names of the dependencies to explain.

- root:

  Path to the package tree.

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

## See also

Other local package trees:
[`local_deps()`](https://pak.r-lib.org/dev/reference/local_deps.md),
[`local_install_deps()`](https://pak.r-lib.org/dev/reference/local_install_deps.md),
[`local_install_dev_deps()`](https://pak.r-lib.org/dev/reference/local_install_dev_deps.md),
[`local_install()`](https://pak.r-lib.org/dev/reference/local_install.md),
[`local_package_trees`](https://pak.r-lib.org/dev/reference/local_package_trees.md),
[`pak()`](https://pak.r-lib.org/dev/reference/pak.md)
