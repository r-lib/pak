# About local package trees

pak can install packages from local package trees. This is convenient
for package development. See the following functions:

- [`local_install()`](https://pak.r-lib.org/reference/local_install.md)
  installs a package from a package tree and all of its dependencies.

- [`local_install_deps()`](https://pak.r-lib.org/reference/local_install_deps.md)
  installs all hard dependencies of a package.

- [`local_install_dev_deps()`](https://pak.r-lib.org/reference/local_install_dev_deps.md)
  installs all hard and soft dependencies of a package. This function is
  intended for package development.

## Details

Note that the last two functions do not install the package in the
specified package tree itself, only its dependencies. This is convenient
if the package itself is loaded via some other means, e.g.
`devtools::load_all()`, for development.

## See also

Other local package trees:
[`local_deps_explain()`](https://pak.r-lib.org/reference/local_deps_explain.md),
[`local_deps()`](https://pak.r-lib.org/reference/local_deps.md),
[`local_install_deps()`](https://pak.r-lib.org/reference/local_install_deps.md),
[`local_install_dev_deps()`](https://pak.r-lib.org/reference/local_install_dev_deps.md),
[`local_install()`](https://pak.r-lib.org/reference/local_install.md),
[`pak()`](https://pak.r-lib.org/reference/pak.md)
