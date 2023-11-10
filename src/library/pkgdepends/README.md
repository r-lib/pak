pkgdepends
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

> Package Dependency Resolution, Downloads and Installation

<!-- badges: start -->

[![lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R build
status](https://github.com/r-lib/pkgdepends/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/pkgdepends/actions)
[![Codecov test
coverage](https://codecov.io/gh/r-lib/pkgdepends/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/pkgdepends?branch=main)
[![R-CMD-check](https://github.com/r-lib/pkgdepends/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/pkgdepends/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

pkgdepends is a toolkit for package dependencies, downloads and
installations, to be used in other packages. If you are looking for a
package manager, see [pak](https://github.com/r-lib/pak).

# Features

- Look up package dependencies recursively.
- Visualize package dependencies.
- Download packages and their dependencies.
- Install downloaded packages.
- Includes a dependency solver to find a consistent set of dependencies.
- Supports CRAN and Bioconductor packages automatically.
- Supports packages on GitHub and GitLab.
- Supports packages in git repositories.
- Supports package bundles or files on the web.
- Supports local package file and trees.
- Supports the `Remotes` entry in the `DESCRIPTION` file.
- Caches metadata and downloaded packages via
  [pkgcache](https://github.com/r-lib/pkgcache)
- Performs all downloads and HTTP queries concurrently.
- Builds and installs packages in parallel.

# Install

Install the package with:

``` r
install.packages("pkgdepends")
```

If you need the development version, install it with

``` r
pak::pak("r-lib/pkgdepends")
```

# Usage

``` r
library(pkgdepends)
```

## Package references

A package reference (ref) specifies a location from which an R package
can be obtained from. Examples:

    devtools
    cran::devtools
    bioc::Biobase
    r-lib/pkgdepends
    https://github.com/r-lib/pkgdepends
    local::~/works/shiny

See [“Package
references”](https://r-lib.github.io/pkgdepends/reference/pkg_refs.html)
for details.

## Package dependencies

Dependencies of the development version of the cli package:

``` r
pd <- new_pkg_deps("r-lib/pkgcache")
pd$solve()
pd$draw()
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/deps-dark.svg">
<img src="man/figures/README/deps.svg" width="100%" />
</picture>

See the
[`pkg_deps`](https://r-lib.github.io/pkgdepends/reference/pkg_deps.html)
class for details.

## Package downloads

Downloading all dependencies of a package:

``` r
pdl <- new_pkg_download_proposal("r-lib/cli")
pdl$resolve()
pdl$download()
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/download-dark.svg">
<img src="man/figures/README/download.svg" width="100%" />
</picture>

See the
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_download_proposal.html)
class for details.

## Package installation

Installing or updating a set of package:

``` r
lib <- tempfile()
dir.create(lib)
pdi <- new_pkg_installation_proposal(
  "r-lib/cli",
  config = list(library = lib)
)
pdi$solve()
pdi$download()
pdi$install()
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/install-dark.svg">
<img src="man/figures/README/install.svg" width="100%" />
</picture>

## Dependency resolution

[`pkg_deps`](https://r-lib.github.io/pkgdepends/reference/pkg_deps.html),
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_download_proposal.html)
and
[`pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_installation_proposal.html)
all resolve their dependencies recursively, to obtain information about
all packages needed for the specified [package
references](https://r-lib.github.io/pkgdepends/reference/pkg_refs.html).
See [“Dependency
resolution”](https://r-lib.github.io/pkgdepends/reference/pkg_resolution.html)
for details.

## The dependency solver

The dependency solver takes the resolution information, and works out
the exact versions of each package that must be installed, such that
version and other requirements are satisfied. See [“The dependency
solver”](https://r-lib.github.io/pkgdepends/reference/pkg_solution.html)
for details.

## Installation plans

[`pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_installation_proposal.html)
can create installation plans, and then also install them. It is also
possible to import installation plans that were created by other tools.
See [“Installation
plans”](https://r-lib.github.io/pkgdepends/reference/install_plans.html)
for details.

## Configuration

The details of
[`pkg_deps`](https://r-lib.github.io/pkgdepends/reference/pkg_deps.html),
[`pkg_download_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_download_proposal.html)
and
[`pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_installation_proposal.html)
can be tuned with a list of configuration options. See
[“Configuration”](https://r-lib.github.io/pkgdepends/reference/pkg_config.html)
for details.

# Related

- [pak](https://github.com/r-lib/pak) – R package manager
- [pkgcache](https://github.com/r-lib/pkgcache) – Metadata and package
  cache
- [devtools](https://github.com/r-lib/devtools) – Tools for R package
  developers

## Code of Conduct

Please note that the pkgdepends project is released with a [Contributor
Code of
Conduct](https://r-lib.github.io/pkgdepends/dev/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

# License

MIT (c) RStudio
