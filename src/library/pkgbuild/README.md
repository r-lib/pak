# pkgbuild

<!-- badges: start -->
[![R-CMD-check](https://github.com/r-lib/pkgbuild/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/pkgbuild/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/r-lib/pkgbuild/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/pkgbuild?branch=main)
<!-- badges: end -->

The goal of pkgbuild is to make it easy to build packages with compiled code. It provides tools to configure your R session, and check that everything is working ok. If you are using RStudio, it also helps you trigger automatic install of the build tools.

## Installation

 Install the released version from CRAN
 
```r
install.packages("pkgbuild")
```

Or install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("r-lib/pkgbuild")
```

## Example

``` r
# Check that you have the build tools installed
pkgbuild::check_build_tools(debug = TRUE)

# Build a package
pkgbuild::build("/path/to/my/package")

# Run your own code in an environment guaranteed to
# have build tools available
pkgbuild::with_build_tools(my_code)
```

## Configuration

### `DESCRIPTION` entries

* `Config/build/clean-inst-doc` can be set to `FALSE` to avoid cleaning up
  `inst/doc` when building a source package. Set it to `TRUE` to force a
  cleanup. See the `clean_doc` argument of `build()` as well.

* `Config/build/copy-method` can be used to avoid copying large directories
  in `R CMD build`. It works by copying (or linking) the files of the
  package to a temporary directory, leaving out the (possibly large) files
  that are not part of the package. Possible values:

  - `none`: pkgbuild does not copy the package tree. This is the default.
  - `copy`: the package files are copied to a temporary directory before
    ` R CMD build`.
  - `link`: the package files are symbolic linked to a temporary directory
    before `R CMD build`. Windows does not have symbolic links, so on Windows
    this is equivalent to `copy`.

  You can also use the `pkg.build_copy_method` option or the
  `PKG_BUILD_COPY_METHOD` environment variable to set the copy method.
  The option is consulted first, then the `DESCRIPTION` entry, then the
  environment variable.

* `Config/build/extra-sources` can be used to define extra source files for
  pkgbuild to decide whether a package DLL needs to be recompiled in
  `needs_compile()`. The syntax is a comma separated list of file names,
  or globs. (See `?utils::glob2rx()`.) E.g. `src/rust/src/*.rs` or `configure*`.

### Options

* `pkg.build_copy_method`: use this option to avoid copying large directories
  when building a package. See possible values above, at the
  `Config/build/copy-method` `DESCRIPTION` entry.

* `pkg.build_extra_flags`: set this to `FALSE` to to opt out from adding
  debug compiler flags in `compile_dll()`. Takes precedence over the
  `PKG_BUILD_EXTRA_FLAGS` environment variable. Possible values:

  - `TRUE`: add extra flags,
  - `FALSE`: do not add extra flags,
  - `"missing"`: add extra flags if the user does not have a
    `$HOME/.R/Makevars` file.

* `pkg.build_stop_for_warnings`: if it is set to `TRUE`, then pkgbuild will stop
  for `R CMD build` errors. It takes precedence over the
  `PKG_BUILD_STOP_FOR_WARNINGS` environment variable.

### Environment variables

* `PKG_BUILD_COLOR_DIAGNOSTICS`: set it to `false` to opt out of colored
  compiler diagnostics. Set it to `true` to force colored compiler
  diagnostics.

* `PKG_BUILD_COPY_METHOD`: use this environment variable to avoid copying
  large directories when building a package. See possible values above,
  at the `Config/build/copy-method` `DESCRIPTION` entry.

* `PKG_BUILD_EXTRA_FLAGS`: set this to `false` to to opt out from adding
  debug compiler flags in `compile_dll()`. The `pkg.build_extra_flags` option
  takes precedence over this environment variable. Possible values:

  - `"true"`: add extra flags,
  - `"false"`: do not add extra flags,
  - `"missing"`: add extra flags if the user does not have a
    `$HOME/.R/Makevars` file.

* `PKG_BUILD_STOP_FOR_WARNINGS`: if it is set to `true`, then pkgbuild will stop
  for `R CMD build` errors. The `pkg.build_stop_for_warnings` option takes
  precedence over this environment variable.

## Code of Conduct

Please note that the pkgbuild project is released with a
[Contributor Code of Conduct](https://pkgbuild.r-lib.org/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
