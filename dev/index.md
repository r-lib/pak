# pak

> A Fresh Approach to R Package Installation

pak installs R packages from CRAN, Bioconductor, GitHub, URLs, git
repositories, local files and directories. It is an alternative to
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
and `devtools::install_github()`. pak is fast, safe and convenient.

- [🚀 Short tour](#rocket-short-tour)
- [🔗 Quick links (start here if in
  doubt!)](#link-quick-links-start-here-if-in-doubt)
- [✨ Features](#sparkles-features)
- [⬇️ Installation](#arrow_down-installation)
- [**📘** License](#blue_book-license)

## 🚀 Short tour

#### Install or update packages from CRAN or Bioconductor

``` r
pak::pkg_install("tibble")
```

    #> ✔ Loading metadata database ... done
    #>
    #> → Will install 10 packages.
    #> → Will download 10 CRAN packages (4.22 MB).
    #> + cli         3.6.5  [bld][cmp][dl] (640.24 kB)
    #> + glue        1.8.0  [bld][cmp][dl] (126.68 kB)
    #> + lifecycle   1.0.5  [bld][dl] (107.14 kB)
    #> + magrittr    2.0.4  [bld][cmp][dl] (281.79 kB)
    #> + pillar      1.11.1 [bld][dl] (409.51 kB)
    #> + pkgconfig   2.0.3  [bld][dl] (6.08 kB)
    #> + rlang       1.1.7  [bld][cmp][dl] (770.34 kB)
    #> + tibble      3.3.1  [bld][cmp][dl] (557.13 kB)
    #> + utf8        1.2.6  [bld][cmp][dl] (243.86 kB)
    #> + vctrs       0.7.1  [bld][cmp][dl] (1.08 MB)
    #>
    #> ℹ Getting 10 pkgs (4.22 MB)
    #> ✔ Got glue 1.8.0 (source) (126.68 kB)
    #> ✔ Got lifecycle 1.0.5 (source) (107.14 kB)
    #> ✔ Got rlang 1.1.7 (source) (770.34 kB)
    #> ✔ Got utf8 1.2.6 (source) (243.86 kB)
    #> ✔ Got cli 3.6.5 (source) (640.24 kB)
    #> ✔ Got pillar 1.11.1 (source) (409.51 kB)
    #> ✔ Got pkgconfig 2.0.3 (source) (6.08 kB)
    #> ✔ Got vctrs 0.7.1 (source) (1.08 MB)
    #> ✔ Got tibble 3.3.1 (source) (557.13 kB)
    #> ✔ Got magrittr 2.0.4 (source) (281.79 kB)
    #> ℹ Building cli 3.6.5
    #> ℹ Building glue 1.8.0
    #> ℹ Building magrittr 2.0.4
    #> ℹ Building pkgconfig 2.0.3
    #> ✔ Built pkgconfig 2.0.3 (1.6s)
    #> ℹ Building rlang 1.1.7
    #> ✔ Built magrittr 2.0.4 (2.1s)
    #> ℹ Building utf8 1.2.6
    #> ✔ Built glue 1.8.0 (2.8s)
    #> ✔ Installed glue 1.8.0  (55ms)
    #> ✔ Installed magrittr 2.0.4  (43ms)
    #> ✔ Installed pkgconfig 2.0.3  (37ms)
    #> ✔ Built utf8 1.2.6 (4.7s)
    #> ✔ Installed utf8 1.2.6  (74ms)
    #> ✔ Built cli 3.6.5 (12.2s)
    #> ✔ Installed cli 3.6.5  (39ms)
    #> ✔ Built rlang 1.1.7 (12.6s)
    #> ✔ Installed rlang 1.1.7  (42ms)
    #> ℹ Building lifecycle 1.0.5
    #> ✔ Built lifecycle 1.0.5 (1.4s)
    #> ✔ Installed lifecycle 1.0.5  (1.1s)
    #> ℹ Building vctrs 0.7.1
    #> ✔ Built vctrs 0.7.1 (22.8s)
    #> ✔ Installed vctrs 0.7.1  (62ms)
    #> ℹ Building pillar 1.11.1
    #> ✔ Built pillar 1.11.1 (3.1s)
    #> ✔ Installed pillar 1.11.1  (1s)
    #> ℹ Building tibble 3.3.1
    #> ✔ Built tibble 3.3.1 (3s)
    #> ✔ Installed tibble 3.3.1  (1s)
    #> ✔ 1 pkg + 9 deps: added 10, dld 10 (4.22 MB) [54.7s]

#### Install packages from GitHub

``` r
pak::pkg_install("tidyverse/tibble")
```

    #>
    #> → Will update 1 package.
    #> → The package (0 B) is cached.
    #> + tibble 3.3.1 → 3.3.1.9009 [bld][cmp] (GitHub: 8b004f2)
    #>
    #> ℹ No downloads are needed, 1 pkg is cached
    #> ✔ Got tibble 3.3.1.9009 (source) (1.39 MB)
    #> ℹ Packaging tibble 3.3.1.9009
    #> ✔ Packaged tibble 3.3.1.9009 (813ms)
    #> ℹ Building tibble 3.3.1.9009
    #> ✔ Built tibble 3.3.1.9009 (2.8s)
    #> ✔ Installed tibble 3.3.1.9009 (github::tidyverse/tibble@8b004f2) (1s)
    #> ✔ 1 pkg + 9 deps: kept 9, upd 1, dld 1 (NA B) [6.9s]

#### Look up dependencies

``` r
pak::pkg_deps_tree("tibble")
```

    #> tibble 3.3.1 [new][bld][cmp]
    #> ├─cli 3.6.5 [new][bld][cmp]
    #> ├─lifecycle 1.0.5 [new][bld]
    #> │ ├─cli
    #> │ └─rlang 1.1.7 [new][bld][cmp]
    #> ├─magrittr 2.0.4 [new][bld][cmp]
    #> ├─pillar 1.11.1 [new][bld]
    #> │ ├─cli
    #> │ ├─glue 1.8.0 [new][bld][cmp]
    #> │ ├─lifecycle
    #> │ ├─rlang
    #> │ ├─utf8 1.2.6 [new][bld][cmp]
    #> │ └─vctrs 0.7.1 [new][bld][cmp]
    #> │   ├─cli
    #> │   ├─glue
    #> │   ├─lifecycle
    #> │   └─rlang
    #> ├─pkgconfig 2.0.3 [new][bld]
    #> ├─rlang
    #> └─vctrs
    #>
    #> Key:  [new] new | [bld] build | [cmp] compile

#### Explain dependencies

``` r
pak::pkg_deps_explain("tibble", "rlang")
```

    #> tibble -> lifecycle -> rlang
    #> tibble -> pillar -> lifecycle -> rlang
    #> tibble -> pillar -> rlang
    #> tibble -> pillar -> vctrs -> lifecycle -> rlang
    #> tibble -> pillar -> vctrs -> rlang
    #> tibble -> rlang
    #> tibble -> vctrs -> lifecycle -> rlang
    #> tibble -> vctrs -> rlang

#### Install a local package and its dependencies

``` r
pak::local_install("cli")
```

    #>
    #> → Will update 1 package.
    #> → The package (0 B) is cached.
    #> + cli 3.6.5 → 3.6.5 [bld][cmp]
    #>
    #> ℹ No downloads are needed, 1 pkg is cached
    #> ✔ Got cli 3.6.5 (source) (640.24 kB)
    #> ℹ Packaging cli 3.6.5
    #> ✔ Packaged cli 3.6.5 (1.5s)
    #> ℹ Building cli 3.6.5
    #> ✔ Built cli 3.6.5 (9.1s)
    #> ✔ Installed cli 3.6.5 (local) (36ms)
    #> ✔ 1 pkg: upd 1, dld 1 (640.24 kB) [11.3s]

## 🔗 Quick links (start here if in doubt!)

### How do I … ?

Start at [*Get Started with
pak*](https://pak.r-lib.org/reference/get-started.html) to solve
specific issues.

### FAQ

Check out the [list of frequently asked
questions](https://pak.r-lib.org/reference/faq.html).

### Reference

[The complete reference of pak
functions](https://pak.r-lib.org/dev/reference/) is the most complete
source of information about pak.

### I have a(nother) question

Don’t hesitate to ask at the [RStudio Community
forum](https://forum.posit.co/). Use the `pak` tag.

### I would like to report a bug

Head to the [pak issue tracker](https://github.com/r-lib/pak/issues).

## ✨ Features

⚡ Fast - parallel downloads and installation, caching, etc.

🦺 Safe - dependency solver, system dependency solver, etc.

🏪 Convenient - packages from multiple sources, time travel, etc.

See the [complete list of awesome
features](https://pak.r-lib.org/reference/features.html).

## [⬇️](https://github.com/r-lib/rig#%EF%B8%8F--installation) Installation

### Pre-built binaries

Install a binary build of pak from our repository on GitHub:

``` r
install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))
```

This is supported for the following systems:

| OS                 | CPU     | R version         |
|--------------------|---------|-------------------|
| Linux              | x86_64  | R 3.5.0 - R-devel |
| Linux              | aarch64 | R 3.5.0 - R-devel |
| macOS High Sierra+ | x86_64  | R 3.5.0 - R-devel |
| macOS Big Sur+     | aarch64 | R 4.1.0 - R-devel |
| Windows            | x86_64  | R 3.5.0 - R-devel |

For macOS we only support the official CRAN R build. Other builds, e.g.
Homebrew R, are not supported.

### Install from CRAN

Install the released version of the package from CRAN as usual:

``` r
install.packages("pak")
```

This potentially needs a C compiler on platforms CRAN does not have
binaries packages for.

### Other platforms and nightly builds

See the [installation
page](https://pak.r-lib.org/reference/install.html)!

## **📘** License

GPL-3 © RStudio
