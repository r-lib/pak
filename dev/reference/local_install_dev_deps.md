# Install all (development) dependencies of a package tree

Installs all dependencies of a package tree (or source package file),
without installing the package tree itself. It installs the development
dependencies as well, specified in the `Suggests` field of
`DESCRIPTION`.

## Usage

``` r
local_install_dev_deps(
  root = ".",
  lib = .libPaths()[1],
  upgrade = TRUE,
  ask = interactive(),
  dependencies = TRUE
)
```

## Arguments

- root:

  Path to the package tree.

- lib:

  Character vector of library paths to consider when creating the
  installation plan.

  - The first library path is the target where packages will be
    installed.

  - Additional library paths, if provided, are visible to the solver as
    candidates for satisfying dependency requirements. If a needed
    package is found here at an acceptable version, it won't be
    re-installed in `lib[1]`.

  - Base and recommended packages in `.Library` are always considered,
    i.e. a recommended package is only duplicated in `lib[1]` if a newer
    version is required.

- upgrade:

  When `FALSE`, the default, pak does the minimum amount of work to give
  you the latest version(s) of `pkg`. It will only upgrade dependent
  packages if `pkg`, or one of their dependencies explicitly require a
  higher version than what you currently have. It will also prefer a
  binary package over to source package, even if the binary package is
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

## See also

Other local package trees:
[`local_deps()`](https://pak.r-lib.org/dev/reference/local_deps.md),
[`local_deps_explain()`](https://pak.r-lib.org/dev/reference/local_deps_explain.md),
[`local_install()`](https://pak.r-lib.org/dev/reference/local_install.md),
[`local_install_deps()`](https://pak.r-lib.org/dev/reference/local_install_deps.md),
[`local_package_trees`](https://pak.r-lib.org/dev/reference/local_package_trees.md),
[`pak()`](https://pak.r-lib.org/dev/reference/pak.md)
