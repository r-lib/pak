
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
pkg_install("dplyr", num_workers = 4)
```

## License

GPL-3 © RStudio
