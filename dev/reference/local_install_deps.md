# Install the dependencies of a package tree

Installs the hard dependencies of a package tree (or source package
file), without installing the package tree itself.

## Usage

``` r
local_install_deps(
  root = ".",
  lib = .libPaths()[1],
  upgrade = TRUE,
  ask = interactive(),
  dependencies = NA
)
```

## Arguments

- root:

  Path to the package tree.

- lib:

  Package library to install the packages to. Note that *all* dependent
  packages will be installed here, even if they are already installed in
  another library. The only exceptions are base and recommended packages
  installed in `.Library`. These are not duplicated in `lib`, unless a
  newer version of a recommended package is needed.

- upgrade:

  When `FALSE`, the default, pak does the minimum amount of work to give
  you the latest version(s) of `pkg`. It will only upgrade dependent
  packages if `pkg`, or one of their dependencies explicitly require a
  higher version than what you currently have. It will also prefer a
  binary package over to source package, even it the binary package is
  older.

  When `upgrade = TRUE`, pak will ensure that you have the latest
  version(s) of `pkg` and all their dependencies.

- ask:

  Whether to ask for confirmation when installing a different version of
  a package that is already installed. Installations that only add new
  packages never require confirmation.

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

Data frame, with information about the installed package(s).

## Details

Note that development (and optional) dependencies, under `Suggests` in
`DESCRIPTION`, are not installed. If you want to install them as well,
use
[`local_install_dev_deps()`](https://pak.r-lib.org/dev/reference/local_install_dev_deps.md).

## See also

Other local package trees:
[`local_deps_explain()`](https://pak.r-lib.org/dev/reference/local_deps_explain.md),
[`local_deps()`](https://pak.r-lib.org/dev/reference/local_deps.md),
[`local_install_dev_deps()`](https://pak.r-lib.org/dev/reference/local_install_dev_deps.md),
[`local_install()`](https://pak.r-lib.org/dev/reference/local_install.md),
[`local_package_trees`](https://pak.r-lib.org/dev/reference/local_package_trees.md),
[`pak()`](https://pak.r-lib.org/dev/reference/pak.md)
