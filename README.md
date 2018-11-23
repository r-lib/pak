
# pkgman

> Manage Package Libraries

![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![Linux Build Status](https://travis-ci.org/r-lib/pkgman.svg?branch=master)](https://travis-ci.org/r-lib/pkgman)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/4sir94ye38nwgxpx/branch/master?svg=true)](https://ci.appveyor.com/project/gaborcsardi/pkgman)
[![](http://www.r-pkg.org/badges/version/pkgman)](http://www.r-pkg.org/pkg/pkgman)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/pkgman)](http://www.r-pkg.org/pkg/pkgman)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-lib/pkgman/master.svg)](https://codecov.io/github/r-lib/pkgman?branch=master)

The goal of pkgman is to install packages and manage libraries and
repositories.

## Installation

``` r
devtools::install_github("r-lib/pkgman")
```

## Usage

``` r
library(pkgman)
pkg_install("dplyr", num_workers = 4, lib = "/tmp/lib")
```

## Goals

* Make package installation cheaper and more reliable.

* Cheaper, reliable package installation allows safer, more convenient
  workflows.

## Features

### Speedy Features

* Fast downloads and queries (all HTTP is concurrent).

* Fast installs (package installations and builds are concurrent).

* Package cache (all downloaded and locally built packages are cached).

* Lazy downloads, only download if needed.

### Safety features

* Private library (pkgman's own dependencies do not affect your "regular"
  packages and vice versa).

* Do load any package in the main process (except for pkgman itself).
  Every operation runs in the sub-process.

* Dependency solver: makes sure that you end up in a consistent, working
  state of dependencies.

* Confirmation, warning for overwriting loaded packages.

* Concurrency safe, locks library, locks caches. Lock is released when the
  process terminates.

### Convenience features

* GitHub packages.

* BioC packages.

* Show download sizes.

## Family of Packages

* `pkgman`: main user facing package

* `pkgdepends`: dependency resolution, package downloads

* `pkginstall`: install downloaded packages

* `pkgcache`: metadata and package cache

## License

GPL-3 © RStudio
