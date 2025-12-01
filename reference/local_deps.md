# Dependencies of a package tree

Dependencies of a package tree

## Usage

``` r
local_deps(root = ".", upgrade = TRUE, dependencies = NA)

local_deps_tree(root = ".", upgrade = TRUE, dependencies = NA)

local_dev_deps(root = ".", upgrade = TRUE, dependencies = TRUE)

local_dev_deps_tree(root = ".", upgrade = TRUE, dependencies = TRUE)
```

## Arguments

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
    types](https://pak.r-lib.org/reference/package-dependency-types.md)
    for other possible values and more information about package
    dependencies.

## Value

All of these functions return the dependencies in a data frame.
`local_deps_tree()` and `local_dev_deps_tree()` also print the
dependency tree.

## See also

Other local package trees:
[`local_deps_explain()`](https://pak.r-lib.org/reference/local_deps_explain.md),
[`local_install_deps()`](https://pak.r-lib.org/reference/local_install_deps.md),
[`local_install_dev_deps()`](https://pak.r-lib.org/reference/local_install_dev_deps.md),
[`local_install()`](https://pak.r-lib.org/reference/local_install.md),
[`local_package_trees`](https://pak.r-lib.org/reference/local_package_trees.md),
[`pak()`](https://pak.r-lib.org/reference/pak.md)
