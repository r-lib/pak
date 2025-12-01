# Explain how a package depends on other packages

Extract dependency chains from `pkg` to `deps`.

## Usage

``` r
pkg_deps_explain(pkg, deps, upgrade = TRUE, dependencies = NA)
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
  sources](https://pak.r-lib.org/reference/pak_package_sources.md)" for
  more details.

- deps:

  Package names of the dependencies to explain.

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
    types](https://pak.r-lib.org/reference/package-dependency-types.md)
    for other possible values and more information about package
    dependencies.

## Value

A named list with a print method. First entries are the function
arguments: `pkg`, `deps`, `dependencies`, the last one is `paths` and it
contains the results in a named list, the names are the package names in
`deps`.

## Details

This function is similar to
[`pkg_deps_tree()`](https://pak.r-lib.org/reference/pkg_deps_tree.md),
but its output is easier to read if you are only interested is certain
packages (`deps`).

## Examples

How does dplyr depend on rlang?

    pkg_deps_explain("dplyr", "rlang")

    #> v Updated metadata database: 5.09 MB in 12 files.
    #> v Updating metadata database ... done
    #> dplyr -> lifecycle -> rlang
    #> dplyr -> rlang
    #> dplyr -> tibble -> lifecycle -> rlang
    #> dplyr -> tibble -> pillar -> lifecycle -> rlang
    #> dplyr -> tibble -> pillar -> rlang
    #> dplyr -> tibble -> pillar -> vctrs -> lifecycle -> rlang
    #> dplyr -> tibble -> pillar -> vctrs -> rlang
    #> dplyr -> tibble -> rlang
    #> dplyr -> tibble -> vctrs -> lifecycle -> rlang
    #> dplyr -> tibble -> vctrs -> rlang
    #> dplyr -> tidyselect -> lifecycle -> rlang
    #> dplyr -> tidyselect -> rlang
    #> dplyr -> tidyselect -> vctrs -> lifecycle -> rlang
    #> dplyr -> tidyselect -> vctrs -> rlang
    #> dplyr -> vctrs -> lifecycle -> rlang
    #> dplyr -> vctrs -> rlang
    #> dplyr -> pillar -> lifecycle -> rlang
    #> dplyr -> pillar -> rlang
    #> dplyr -> pillar -> vctrs -> lifecycle -> rlang
    #> dplyr -> pillar -> vctrs -> rlang

How does the GH version of usethis depend on cli and ps?

    pkg_deps_explain("r-lib/usethis", c("cli", "ps"))

    #> usethis -> cli
    #> usethis -> desc -> cli
    #> usethis -> gh -> cli
    #> usethis -> lifecycle -> cli
    #>
    #> x ps
