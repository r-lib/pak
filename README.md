
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pak

> A Fresh Approach to R Package
Installation

<!-- badges: start -->

![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![Linux Build
Status](https://travis-ci.org/r-lib/pak.svg?branch=master)](https://travis-ci.org/r-lib/pak)
[![Windows Build
status](https://ci.appveyor.com/api/projects/status/vu03c1csxutwq1mr/branch/master?svg=true)](https://ci.appveyor.com/project/gaborcsardi/pak)
[![](https://www.r-pkg.org/badges/version/pak)](https://cran.r-project.org/package=pak)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/pak)](https://www.r-pkg.org/pkg/pak)
[![Coverage
Status](https://img.shields.io/codecov/c/github/r-lib/pak/master.svg)](https://codecov.io/github/r-lib/pak?branch=master)
<!-- badges: end -->

pak installs R packages from CRAN, Bioconductor, GitHub, and local files
and directories. It is an alternative to `install.packages()` and
`devtools::install_github()`. pak is fast, safe and
convenient.

<p align="center">

<img width="1000" src="https://cdn.jsdelivr.net/gh/r-lib/pak@master/tools/images/install-2.svg">

</p>

## Installation

Install the package from CRAN:

``` r
install.packages("pak")
```

(After installation, you might also want to run `pak::pak_setup()`;
it’ll be run automatically when needed but you might want to do it now
to save some time later.)

## Usage

Call `pkg_install()` to install CRAN or Bioconductor packages:

``` r
pak::pkg_install("usethis")
```

To install GitHub packages, use the `user/repo` syntax:

``` r
pak::pkg_install("r-lib/usethis")
```

All dependencies will be installed as well, to the same library.

## Why pak?

### Fast

  - Fast downloads and HTTP queries. pak performs all HTTP requests
    concurrently.

  - Fast installs. pak builds and installs packages concurrently.

  - Metadata and package cache. pak caches package metadata and all
    downloaded packages locally. It does not download the same package
    files over and over again.

  - Lazy installation. pak only installs the packages that are really
    necessary for the installation. If the requested package and its
    dependencies are already installed, pak does nothing.

### Safe

  - Private library (pak’s own package dependencies do not affect your
    regular package libraries and vice versa).

  - Every pak operation runs in a sub-process, and the packages are
    loaded from the private library. pak avoids loading packages from
    your regular package libraries. (These package files would be locked
    on some systems, and locked packages cannot be updated. pak does not
    load any package in the main process, except for pak itself).

  - To avoid updating locked packages, pak warns and requests
    confirmation for loaded packages.

  - Dependency solver. pak makes sure that you end up in a consistent,
    working state of dependencies. If finds conflicts up front, before
    attempting installation.

### Convenient

  - BioC packages. pak supports Bioconductor packages out of the box. It
    uses the Bioconductor version that is appropriate for your R
    version.

  - GitHub packages. pak supports GitHub packages out of the box. It
    also supports the `Remotes` entry in `DESCRIPTION` files, so that
    GitHub dependencies of GitHub packages will also get installed. See
    e.g.
    <https://cran.r-project.org/package=remotes/vignettes/dependencies.html>

  - Package sizes. For CRAN packages pak shows the total sizes of
    packages it needs to download.

## Roadmap

  - Support GitLab repositories
  - Support Bitbucket repositories
  - Support package URLs
  - Support system requirements
  - Support older CRAN package versions
  - Support older BioConductor package versions
  - Support GitHub pull requests
  - Support local CRAN mirrors
  - Support the `Additional_repositories` `DESCRIPTION` field
  - Support SVN repos

## License

GPL-3 © RStudio
