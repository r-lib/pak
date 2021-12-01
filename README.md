
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pak

> A Fresh Approach to R Package Installation

<!-- badges: start -->

![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![](https://www.r-pkg.org/badges/version/pak)](https://cran.r-project.org/package=pak)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/pak)](https://www.r-pkg.org/pkg/pak)
[![Codecov test
coverage](https://codecov.io/gh/r-lib/pak/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/pak?branch=main)
[![R build
status](https://github.com/r-lib/pak/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/pak/actions)
<!-- badges: end -->

pak installs R packages from CRAN, Bioconductor, GitHub, and local files
and directories. It is an alternative to `install.packages()` and
`devtools::install_github()`. pak is fast, safe and convenient.

<p align="center">
<img width="1000" src="https://cdn.jsdelivr.net/gh/r-lib/pak@main/tools/images/fast.svg">
</p>

## Installation

Install the released version of the package from CRAN:

``` r
install.packages("pak")
```

(After installation, you might also want to run `pak::pak_setup()`;
it’ll be run automatically when needed but you might want to do it now
to save some time later.)

Install the development version from our repository on GitHub:

``` r
install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")
```

This is currently supported for

-   macOS (High Sierra or later) and R 3.3.x or later on Intel and R 4.1
    or later on arm64, including R-devel.
-   Windows and R 3.3.x, or later, including R-devel,
-   Linux (any 64-bit x86_64 distribution) and R 3.3.x or later,
    including R-devel.

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

### On GitHub Actions

The [`r-lib/actions`](https://github.com/r-lib/actions) repository has
an `R CMD check` GitHub Action that uses pak to install packages:
<https://github.com/r-lib/actions/blob/master/examples/check-pak.yaml>
To set up this action for your own repository, run

``` r
usethis::use_github_action("check-pak")
usethis::use_github_action("test-coverage-pak")
usethis::use_github_action("pkgdown-pak")
```

from R. The last two are a test coverage action and a pkgdown action.

## Why pak?

### Fast

-   Fast downloads and HTTP queries. pak performs all HTTP requests
    concurrently.

-   Fast installs. pak builds and installs packages concurrently.

-   Metadata and package cache. pak caches package metadata and all
    downloaded packages locally. It does not download the same package
    files over and over again.

-   Lazy installation. pak only installs the packages that are really
    necessary for the installation. If the requested package and its
    dependencies are already installed, pak does nothing.

### Safe

-   Private library (pak’s own package dependencies do not affect your
    regular package libraries and vice versa).

-   Every pak operation runs in a sub-process, and the packages are
    loaded from the private library. pak avoids loading packages from
    your regular package libraries. (These package files would be locked
    on some systems, and locked packages cannot be updated. pak does not
    load any package in the main process, except for pak itself).

-   To avoid updating locked packages, pak offers the choice of
    unloading them from the current R session, and/or killing other R
    sessions locking them.

-   Dependency solver. pak makes sure that you end up in a consistent,
    working state of dependencies. It finds conflicts up front, before
    attempting installation.

### Convenient

-   BioC packages. pak supports Bioconductor packages out of the box. It
    uses the Bioconductor version that is appropriate for your R
    version.

-   GitHub packages. pak supports GitHub packages out of the box. It
    also supports the `Remotes` entry in `DESCRIPTION` files, so that
    GitHub dependencies of GitHub packages will also get installed. See
    e.g.
    <https://cran.r-project.org/package=remotes/vignettes/dependencies.html>

-   Easy time travel with [MRAN](https://mran.microsoft.com/timemachine)
    and [RSPM](https://packagemanager.rstudio.com/client/#/), to a
    specific date or when a certain R or package version was released.

-   Package sizes. For CRAN packages pak shows the total sizes of
    packages it needs to download.

-   Other package sources:

    -   local package files and directories,
    -   URLs to package files or package tree archives.

## Roadmap

-   Support GitLab repositories
-   Support Bitbucket repositories
-   Support system requirements
-   Support older CRAN package versions
-   Support older BioConductor package versions
-   Support local CRAN mirrors
-   Support the `Additional_repositories` `DESCRIPTION` field
-   Support SVN repos

## License

GPL-3 © RStudio
