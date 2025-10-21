# Draw the dependency tree of a package

Draw the dependency tree of a package

## Usage

``` r
pkg_deps_tree(pkg, upgrade = TRUE, dependencies = NA)
```

## Arguments

- pkg:

  Package names or package references. E.g.

  - `ggplot2`: package from CRAN, Bioconductor or a CRAN-like repository
    in general,

  - `tidyverse/ggplot2`: package from GitHub,

  - `tidyverse/ggplot2@v3.4.0`: package from GitHub tag or branch,

  - `https://examples.com/.../ggplot2_3.3.6.tar.gz`: package from URL,

  - `.`: package in the current working directory.

  See "[Package
  sources](https://pak.r-lib.org/dev/reference/pak_package_sources.md)"
  for more details.

- upgrade:

  Whether to use the most recent available package versions.

- dependencies:

  What kinds of dependencies to install. Most commonly one of the
  following values:

  - `NA`: only required (hard) dependencies,

  - `TRUE`: required dependencies plus optional and development
    dependencies,

  - `FALSE`: do not install any dependencies. (You might end up with a
    non-working package, and/or the installation might fail.) See
    [Package dependency
    types](https://pak.r-lib.org/dev/reference/package-dependency-types.md)
    for other possible values and more information about package
    dependencies.

## Value

The same data frame as
[`pkg_deps()`](https://pak.r-lib.org/dev/reference/pkg_deps.md),
invisibly.

## Examples

    pkg_deps_tree("dplyr")

    #> dplyr 1.0.10 ✨
    #> ├─generics 0.1.3 ✨
    #> ├─glue 1.6.2 ✨
    #> ├─lifecycle 1.0.3 ✨
    #> │ ├─cli 3.4.1 ✨
    #> │ ├─glue
    #> │ └─rlang 1.0.6 ✨
    #> ├─magrittr 2.0.3 ✨
    #> ├─R6 2.5.1 ✨
    #> ├─rlang
    #> ├─tibble 3.1.8 ✨
    #> │ ├─fansi 1.0.3 ✨
    #> │ ├─lifecycle
    #> │ ├─magrittr
    #> │ ├─pillar 1.8.1 ✨
    #> │ │ ├─cli
    #> │ │ ├─fansi
    #> │ │ ├─glue
    #> │ │ ├─lifecycle
    #> │ │ ├─rlang
    #> │ │ ├─utf8 1.2.2 ✨
    #> │ │ └─vctrs 0.5.1 ✨
    #> │ │   ├─cli
    #> │ │   ├─glue
    #> │ │   ├─lifecycle
    #> │ │   └─rlang
    #> │ ├─pkgconfig 2.0.3 ✨
    #> │ ├─rlang
    #> │ └─vctrs
    #> ├─tidyselect 1.2.0 ✨
    #> │ ├─cli
    #> │ ├─glue
    #> │ ├─lifecycle
    #> │ ├─rlang
    #> │ ├─vctrs
    #> │ └─withr 2.5.0 ✨
    #> ├─vctrs
    #> └─pillar
    #>
    #> Key:  ✨ new

    pkg_deps_tree("r-lib/usethis")

    #> r-lib/usethis 2.1.6.9000 ✨👷🏼🔧 
    #> ├─cli 3.4.1 ✨
    #> ├─clipr 0.8.0 ✨
    #> ├─crayon 1.5.2 ✨
    #> ├─curl 4.3.3 ✨
    #> ├─desc 1.4.2 ✨
    #> │ ├─cli
    #> │ ├─R6 2.5.1 ✨
    #> │ └─rprojroot 2.0.3 ✨
    #> ├─fs 1.5.2 ✨
    #> ├─gert 1.9.2 ✨ ⬇ (1.91 MB)
    #> │ ├─askpass 1.1 ✨
    #> │ │ └─sys 3.4.1 ✨
    #> │ ├─credentials 1.3.2 ✨ ⬇ (170.15 kB)
    #> │ │ ├─openssl 2.0.5 ✨
    #> │ │ │ └─askpass
    #> │ │ ├─sys
    #> │ │ ├─curl
    #> │ │ ├─jsonlite 1.8.4 ✨
    #> │ │ └─askpass
    #> │ ├─openssl
    #> │ ├─rstudioapi 0.14 ✨
    #> │ ├─sys
    #> │ └─zip 2.2.2 ✨
    #> ├─gh 1.3.1 ✨ ⬇ (95.20 kB)
    #> │ ├─cli
    #> │ ├─gitcreds 0.1.2 ✨ ⬇ (95.59 kB)
    #> │ ├─httr 1.4.4 ✨
    #> │ │ ├─curl
    #> │ │ ├─jsonlite
    #> │ │ ├─mime 0.12 ✨
    #> │ │ ├─openssl
    #> │ │ └─R6
    #> │ ├─ini 0.3.1 ✨ ⬇ (13.13 kB)
    #> │ └─jsonlite
    #> ├─glue 1.6.2 ✨
    #> ├─jsonlite
    #> ├─lifecycle 1.0.3 ✨
    #> │ ├─cli
    #> │ ├─glue
    #> │ └─rlang 1.0.6 ✨
    #> ├─purrr 0.3.5 ✨
    #> │ ├─magrittr 2.0.3 ✨
    #> │ └─rlang
    #> ├─rappdirs 0.3.3 ✨
    #> ├─rlang
    #> ├─rprojroot
    #> ├─rstudioapi
    #> ├─whisker 0.4.1 ✨ ⬇ (65.36 kB)
    #> ├─withr 2.5.0 ✨
    #> └─yaml 2.3.6 ✨
    #>
    #> Key:  ✨ new |  ⬇ download | 👷🏼 build | 🔧 compile

## See also

Other package functions:
[`lib_status()`](https://pak.r-lib.org/dev/reference/lib_status.md),
[`pak()`](https://pak.r-lib.org/dev/reference/pak.md),
[`pkg_deps()`](https://pak.r-lib.org/dev/reference/pkg_deps.md),
[`pkg_download()`](https://pak.r-lib.org/dev/reference/pkg_download.md),
[`pkg_install()`](https://pak.r-lib.org/dev/reference/pkg_install.md),
[`pkg_remove()`](https://pak.r-lib.org/dev/reference/pkg_remove.md),
[`pkg_status()`](https://pak.r-lib.org/dev/reference/pkg_status.md),
[`pkg_sysreqs()`](https://pak.r-lib.org/dev/reference/pkg_sysreqs.md)
