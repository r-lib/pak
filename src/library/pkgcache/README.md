
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgcache

> Cache CRAN-like metadata and package files

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/pkgcache)](https://cran.r-project.org/package=pkgcache)
[![R-CMD-check](https://github.com/r-lib/pkgcache/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/pkgcache/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/r-lib/pkgcache/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/pkgcache?branch=main)
<!-- badges: end -->

Metadata and package cache for CRAN-like repositories. This is a utility
package to be used by package management tools that want to take
advantage of caching.

## Installation

You can install the released version of pkgcache from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("pkgcache")
```

## Metadata cache

`meta_cache_list()` lists all packages in the metadata cache. It
includes Bioconductor package, and all versions (i.e. both binary and
source) of the packages for the current platform and R version.

(We load the pillar package, because it makes the pkgcache data frames
print nicer, similarly to tibbles.)

``` r
library(pkgcache)
library(pillar)
meta_cache_list()
#> # A data frame: 42,012 x 32
#>    package     version depends suggests license imports linkingto archs enhances
#>    <chr>       <chr>   <chr>   <chr>    <chr>   <chr>   <chr>     <chr> <chr>   
#>  1 A3          1.0.0   R (>= ~ randomF~ GPL (>~ <NA>    <NA>      <NA>  <NA>    
#>  2 AATtools    0.0.1   R (>= ~ <NA>     GPL-3   magrit~ <NA>      <NA>  <NA>    
#>  3 ABACUS      1.0.0   R (>= ~ rmarkdo~ GPL-3   ggplot~ <NA>      <NA>  <NA>    
#>  4 ABC.RAP     0.9.0   R (>= ~ knitr, ~ GPL-3   graphi~ <NA>      <NA>  <NA>    
#>  5 ABCanalysis 1.2.1   R (>= ~ <NA>     GPL-3   plotrix <NA>      <NA>  <NA>    
#>  6 ABCoptim    0.15.0  <NA>    testtha~ MIT + ~ Rcpp, ~ Rcpp      ABCo~ <NA>    
#>  7 ABCp2       1.2     MASS    <NA>     GPL-2   <NA>    <NA>      <NA>  <NA>    
#>  8 ABHgenotyp~ 1.0.1   <NA>    knitr, ~ GPL-3   ggplot~ <NA>      <NA>  <NA>    
#>  9 ABPS        0.3     <NA>    testthat GPL (>~ kernlab <NA>      <NA>  <NA>    
#> 10 ACA         1.1     R (>= ~ <NA>     GPL     graphi~ <NA>      <NA>  <NA>    
#> # ... with 42,002 more rows, and 23 more variables: os_type <chr>,
#> #   priority <chr>, license_is_foss <chr>, license_restricts_use <chr>,
#> #   repodir <chr>, rversion <chr>, platform <chr>, needscompilation <chr>,
#> #   ref <chr>, type <chr>, direct <lgl>, status <chr>, target <chr>,
#> #   mirror <chr>, sources <list>, filesize <dbl>, sha256 <chr>, sysreqs <chr>,
#> #   built <chr>, published <dttm>, deps <list>, md5sum <chr>, path <chr>
```

`meta_cache_deps()` and `meta_cache_revdeps()` can be used to look up
dependencies and reverse dependencies.

The metadata is updated automatically if it is older than seven days,
and it can also be updated manually with `meta_cache_update()`.

See the `cranlike_metadata_cache` R6 class for a lower level API, and
more control.

## Package cache

Package management tools may use the `pkg_cache_*` functions and in
particular the `package_cache` class, to make use of local caching of
package files.

The `pkg_cache_*` API is high level, and uses a user level cache:

``` r
pkg_cache_summary()
#> $cachepath
#> [1] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/pkg"
#> 
#> $files
#> [1] 413
#> 
#> $size
#> [1] 3493123806
```

``` r
pkg_cache_list()
#> # A data frame: 413 x 11
#>    fullpath    path  package url   etag  sha256 version platform built vignettes
#>    <chr>       <chr> <chr>   <chr> <chr> <chr>  <chr>   <chr>    <chr> <chr>    
#>  1 /Users/gab~ src/~ pak     file~  <NA> 4e451~ 0.2.0.~ source   <NA>  <NA>     
#>  2 /Users/gab~ src/~ pak     <NA>   <NA> <NA>   0.2.0.~ aarch64~ TRUE  FALSE    
#>  3 /Users/gab~ bin/~ cli     http~ "\"1~ cbf6f~ 3.1.1   aarch64~ <NA>  <NA>     
#>  4 /Users/gab~ src/~ R7      <NA>   <NA> 76af5~ <NA>    <NA>     FALSE <NA>     
#>  5 /Users/gab~ src/~ Annota~ http~ "\"4~ 9c18b~ 1.18.0  source   <NA>  <NA>     
#>  6 /Users/gab~ bin/~ GGally  http~ "\"1~ a8ab8~ 2.1.2   aarch64~ <NA>  <NA>     
#>  7 /Users/gab~ src/~ ensemb~ http~ "\"3~ 0913f~ 2.18.3  source   <NA>  <NA>     
#>  8 /Users/gab~ src/~ bioviz~ http~ "\"2~ 5faa6~ 1.42.0  source   <NA>  <NA>     
#>  9 /Users/gab~ src/~ Annota~ http~ "\"4~ 283e2~ 1.56.2  source   <NA>  <NA>     
#> 10 /Users/gab~ src/~ rtrack~ http~ "\"3~ 0c5e4~ 1.54.0  source   <NA>  <NA>     
#> # ... with 403 more rows, and 1 more variable: rversion <chr>
```

``` r
pkg_cache_find(package = "dplyr")
#> # A data frame: 3 x 11
#>   fullpath     path  package url   etag  sha256 version platform built vignettes
#>   <chr>        <chr> <chr>   <chr> <chr> <chr>  <chr>   <chr>    <chr> <chr>    
#> 1 /Users/gabo~ src/~ dplyr   http~ "\"d~ 3b6aa~ 1.0.8   source   <NA>  <NA>     
#> 2 /Users/gabo~ bin/~ dplyr   http~ "\"6~ e46b3~ 1.0.7   aarch64~ <NA>  <NA>     
#> 3 /Users/gabo~ bin/~ dplyr   http~ "\"1~ 94913~ 1.0.8   aarch64~ <NA>  <NA>     
#> # ... with 1 more variable: rversion <chr>
```

`pkg_cache_add_file()` can be used to add a file,
`pkg_cache_delete_files()` to remove files, `pkg_cache_get_files()` to
copy files out of the cache.

The `package_cache` class provides a finer API.

## Installed packages

pkgcache contains a very fast DCF parser to parse `PACKAGES*` files, or
the `DESCRIPTION` files in installed packages. `parse_packages()` parses
all fields from `PACKAGES`, `PACKAGES.gz` or `PACKAGES.rds` files.
`parse_installed()` reads *all* metadata from packages installed into a
library:

``` r
parse_installed()
#> # A data frame: 329 × 97
#>    Package    Version Title Author Maintainer Depends Imports LinkingTo Suggests
#>    <chr>      <chr>   <chr> <chr>  <chr>      <chr>   <chr>   <chr>     <chr>   
#>  1 ade4       1.7-22  "Ana… "Stép… Aurélie S… R (>= … "graph… Rcpp, Rc… "ade4Tk…
#>  2 archive    1.1.5   "Mul… "Jim … Gábor Csá… R (>= … "cli, … cli, cpp… "covr, …
#>  3 asciicast  2.3.0.… "Cre… "Gábo… Gábor Csá… <NA>    "cli (… processx  "callr,…
#>  4 AsioHeade… 1.22.1… "'As… "Dirk… Dirk Edde… <NA>     <NA>   <NA>       <NA>   
#>  5 askpass    1.1     "Saf… "Jero… Jeroen Oo… <NA>    "sys (… <NA>      "testth…
#>  6 assertthat 0.2.1   "Eas… "Hadl… Hadley Wi… <NA>    "tools" <NA>      "testth…
#>  7 async      0.0.0.… "Asy… "Gábo… Gábor Csá… R (>= … "callr… <NA>      "cli, c…
#>  8 attempt    0.3.1   "Too… "Coli… Colin Fay… <NA>    "rlang" <NA>      "testth…
#>  9 babynames  1.0.1   "US … "Hadl… Hadley Wi… R (>= … "tibbl… <NA>      "testth…
#> 10 backports  1.4.1   "Rei… "Mich… Michel La… R (>= …  <NA>   <NA>       <NA>   
#> # ℹ 319 more rows
#> # ℹ 88 more variables: Description <chr>, License <chr>, URL <chr>,
#> #   BugReports <chr>, Encoding <chr>, NeedsCompilation <chr>, Packaged <chr>,
#> #   Repository <chr>, `Date/Publication` <chr>, Built <chr>, Archs <chr>,
#> #   RemoteType <chr>, RemotePkgRef <chr>, RemoteRef <chr>, RemoteRepos <chr>,
#> #   RemotePkgPlatform <chr>, RemoteSha <chr>, `Authors@R` <chr>,
#> #   ByteCompile <chr>, RoxygenNote <chr>, SystemRequirements <chr>, …
```

## Bioconductor support

Both the metadata cache and the package cache support Bioconductor by
default, automatically. See the `BioC_mirror` option and the
`R_BIOC_MIRROR` and `R_BIOC_VERSION` environment variables below to
configure Bioconductor support.

## Package Options

- The `BioC_mirror` option can be used to select a Bioconductor mirror.
  This takes priority over the `R_BIOC_MIRROR` environment variable.
- You can use the `pkg.current_platform` option to set the platform
  string for the current platform for the `current_r_platform()`
  function. This is useful if pkgcache didn’t detect the platform
  correctly. Alternatively, you can use the `PKG_CURRENT_PLATFORM`
  environment variable. The option takes priority.
- `pkgcache_timeout` is the HTTP timeout for all downloads. It is in
  seconds, and the limit for downloading the whole file. Defaults to
  3600, one hour. It corresponds to the [`TIMEOUT` libcurl
  option](https://curl.se/libcurl/c/CURLOPT_TIMEOUT.html).
- `pkgcache_connecttimeout` is the HTTP timeout for the connection
  phase. It is in seconds and defaults to 30 seconds. It corresponds to
  the [`CONNECTTIMEOUT` libcurl
  option](https://curl.se/libcurl/c/CURLOPT_CONNECTTIMEOUT.html).
- `pkgcache_low_speed_limit` and `pkgcache_low_speed_time` are used for
  a more sensible HTTP timeout. If the download speed is less than
  `pkgcache_low_speed_limit` bytes per second for at least
  `pkgcache_low_speed_time` seconds, the download errors. They
  correspond to the
  [`LOW_SPEED_LIMIT`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_LIMIT.html)
  and
  [`LOW_SPEED_TIME`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_TIME.html)
  curl options.

## Package environment variables

- The `R_BIOC_VERSION` environment variable can be used to override the
  default Bioconductor version detection and force a given version. E.g.
  this can be used to force the development version of Bioconductor.
- The `R_BIOC_MIRROR` environment variable can be used to select a
  Bioconductor mirror. The `BioC_mirror` option takes priority over
  this, if set.
- You can use the `PKG_CURRENT_PLATFORM` environment variable to set the
  platform string for the current platform for the
  `current_r_platform()` function. This is useful if pkgcache didn’t
  detect the platform correctly. Alternatively, you can use the
  `pkg.current_platofrm` option, which takes. priority over the
  environment variable.
- `PKGCACHE_PPM_REPO` is the name of the Posit Package Manager
  repository to use. Defaults to `"cran"`.
- `PKGCACHE_PPM_URL` is the base URL of the Posit Package Manager
  instance to use. It defaults to the URL of the Posit Public Package
  Manager instance at <https://packagemanager.posit.co/client/#/>.
- `PKGCACHE_TIMEOUT` is the HTTP timeout for all downloads. It is in
  seconds, and the limit for downloading the whole file. Defaults to
  3600, one hour. It corresponds to the [`TIMEOUT` libcurl
  option](https://curl.se/libcurl/c/CURLOPT_TIMEOUT.html). The
  `pkgcache_timeout` option has priority over this, if set.
- `PKGCACHE_CONNECTTIMEOUT` is the HTTP timeout for the connection
  phase. It is in seconds and defaults to 30 seconds. It corresponds to
  the [`CONNECTTIMEOUT` libcurl
  option](https://curl.se/libcurl/c/CURLOPT_CONNECTTIMEOUT.html). The
  `pkgcache_connecttimeout` option takes precedence over this, if set.
- `PKGCACHE_LOW_SPEED_LIMIT` and `PKGCACHE_LOW_SPEED_TIME` are used for
  a more sensible HTTP timeout. If the download speed is less than
  `PKGCACHE_LOW_SPEED_LIMIT` bytes per second for at least
  `PKGCACHE_LOW_SPEED_TIME` seconds, the download errors. They
  correspond to the
  [`LOW_SPEED_LIMIT`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_LIMIT.html)
  and
  [`LOW_SPEED_TIME`](https://curl.se/libcurl/c/CURLOPT_LOW_SPEED_TIME.html)
  curl options. The `pkgcache_low_speed_time` and
  `pkgcache_low_speed_limit` options have priority over these
  environment variables, if they are set.
- `R_PKG_CACHE_DIR` is used for the cache directory, if set. (Otherwise
  `rappdirs::user_cache_dir()` is used, see also `meta_cache_summary()`
  and `pkg_cache_summary()`).

## Using pkgcache in CRAN packages

If you use pkgcache in your CRAN package, please make sure that

- you don’t use pkgcache in your examples, and
- you set the `R_USER_CACHE_DIR` environment variable to a temporary
  directory (e.g. via `tempfile()`) during test cases. See the
  `tests/testthat/setup.R` file in pkgcache for an example.

This is to make sure that pkgcache does not modify the user’s files
while running `R CMD check`.

## Code of Conduct

Please note that the pkgcache project is released with a [Contributor
Code of Conduct](https://r-lib.github.io/pkgcache/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## License

MIT (c) [Posit Software, PBC](https://posit.co)
