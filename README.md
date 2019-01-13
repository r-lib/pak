
# pkgman

> Manage Package Libraries

![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![Linux Build Status](https://travis-ci.org/r-lib/pkgman.svg?branch=master)](https://travis-ci.org/r-lib/pkgman)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/4sir94ye38nwgxpx/branch/master?svg=true)](https://ci.appveyor.com/project/gaborcsardi/pkgman)
[![](https://www.r-pkg.org/badges/version/pkgman)](https://cran.r-project.org/package=pkgman)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/pkgman)](https://www.r-pkg.org/pkg/pkgman)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-lib/pkgman/master.svg)](https://codecov.io/github/r-lib/pkgman?branch=master)

pkgman installs R packages from various sources.

## Installation

Install the package from CRAN:

``` r
install.packages("pkgman")
pkgman::pkgman_install_deps()
```

The second line creates pkgman's own package library, to avoid interference
between pkgman's dependencies and the user's regular packages. Run this
command any time to update pkgman's package library.

## Usage

Simply call `pkg_install` to install packages:

```r
pkgman::pkg_install("dplyr", lib = "/tmp/lib")
```

All dependencies will be installed as well, to the same library.

## Goals

* Make package installation cheaper and more reliable.

* Cheaper, reliable package installation allows safer, more convenient
  workflows.

## Features

### Speedy Features

* Fast downloads and queries (all HTTP is concurrent).

* Fast installs (package installations and builds are concurrent).

* Package cache (all downloaded and locally built packages are cached).

* Lazy downloads, only download metadata and package files if needed.

### Safety features

* Private library (pkgman's own dependencies do not affect your "regular"
  packages and vice versa).

* Do load any package in the main process (except for pkgman itself).
  Every operation runs in the sub-process, and the packages are loaded
  from the private library.

* Dependency solver: makes sure that you end up in a consistent, working
  state of dependencies. Find conflicts up front, before actually installing
  anything.

* Confirmation, warning for overwriting loaded packages.

* Concurrency safe, locks library, locks caches. Lock is released when the
  process terminates.

### Convenience features

* GitHub packages.

* BioC packages.

* Show download sizes.

## License

GPL-3 © RStudio
