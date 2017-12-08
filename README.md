pkgman
------

[![Travis build status](https://travis-ci.org/r-lib/pkgman.svg?branch=master)](https://travis-ci.org/r-lib/pkgman)

Overview
--------
The goal of pkgman is to install packages and manage libraries and repositories.


Installation
------------
You can install the development version from github:

```r
# install.packages("devtools")
devtools::install_github("r-lib/pkgman")
```

Usage
-----

```r
library(pkgman)

pkg_install("dplyr", num_workers = 4)
```
