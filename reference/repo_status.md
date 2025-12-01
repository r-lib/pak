# Show the status of CRAN-like repositories

It checks the status of the configured or supplied repositories.

## Usage

``` r
repo_status(
  platforms = NULL,
  r_version = getRversion(),
  bioc = NULL,
  cran_mirror = NULL
)

repo_ping(
  platforms = NULL,
  r_version = getRversion(),
  bioc = NULL,
  cran_mirror = NULL
)
```

## Arguments

- platforms:

  Platforms to use, default is the current platform, plus source
  packages, via the
  [`pkg.platforms`](https://pak.r-lib.org/reference/pak-config.md)
  option.

- r_version:

  R version(s) to use, the default is the current R version, via
  [`getRversion()`](https://rdrr.io/r/base/numeric_version.html).

- bioc:

  Whether to add the Bioconductor repositories. If you already
  configured them via `options(repos)`, then you can set this to
  `FALSE`. Defaults to the
  [`pkg.use_bioconductor`](https://pak.r-lib.org/reference/pak-config.md)
  option.

- cran_mirror:

  The CRAN mirror to use. Defaults to the
  [`pkg.cran_mirror`](https://pak.r-lib.org/reference/pak-config.md)
  option.

## Value

A data frame that has a row for every repository, on every queried
platform and R version. It has these columns:

- `name`: the name of the repository. This comes from the names of the
  configured repositories in `options("repos")`, or added by pak. It is
  typically `CRAN` for CRAN, and the current Bioconductor repositories
  are `BioCsoft`, `BioCann`, `BioCexp`, `BioCworkflows`.

- `url`: base URL of the repository.

- `bioc_version`: Bioconductor version, or `NA` for non-Bioconductor
  repositories.

- `username`: Included if at least one repository is authenticated.
  `NA_character_` for repositories without authentication. See
  [`repo_auth()`](https://pak.r-lib.org/reference/repo_auth.md).

- `has_password`: `TRUE` is the function could retrieve the password for
  the authenticated repository. It is `NA` for repositories without
  authentication. This column is included only if at least one
  repository has authentication. See
  [`repo_auth()`](https://pak.r-lib.org/reference/repo_auth.md).

- `platform`: platform, possible values are `source`, `macos` and
  `windows` currently.

- `path`: the path to the packages within the base URL, for a given
  platform and R version.

- `r_version`: R version, one of the specified R versions.

- `ok`: Logical flag, whether the repository contains a metadata file
  for the given platform and R version.

- `ping`: HTTP response time of the repository in seconds. If the `ok`
  column is `FALSE`, then this columns in `NA`.

- `error`: the error object if the HTTP query failed for this
  repository, platform and R version.

## Details

`repo_ping()` is similar to `repo_status()` but also prints a short
summary of the data, and it returns its result invisibly.

## Examples

    repo_status()

    #> # A data frame: 12 × 10
    #>    name    url   type  bioc_version platform path  r_version ok     ping
    #>    <chr>   <chr> <chr> <chr>        <chr>    <chr> <chr>     <lgl> <dbl>
    #>  1 CRAN    http… cran  NA           source   src/… 4.4       TRUE  0.246
    #>  2 CRAN    http… cran  NA           aarch64… bin/… 4.4       TRUE  0.298
    #>  3 BioCso… http… bioc  3.20         source   src/… 4.4       TRUE  0.298
    #>  4 BioCso… http… bioc  3.20         aarch64… bin/… 4.4       TRUE  0.322
    #>  5 BioCann http… bioc  3.20         source   src/… 4.4       TRUE  0.530
    #>  6 BioCann http… bioc  3.20         aarch64… bin/… 4.4       TRUE  0.643
    #>  7 BioCexp http… bioc  3.20         source   src/… 4.4       TRUE  0.498
    #>  8 BioCexp http… bioc  3.20         aarch64… bin/… 4.4       TRUE  0.707
    #>  9 BioCwo… http… bioc  3.20         source   src/… 4.4       TRUE  0.569
    #> 10 BioCwo… http… bioc  3.20         aarch64… bin/… 4.4       TRUE  0.724
    #> 11 BioCbo… http… bioc  3.20         source   src/… 4.4       TRUE  0.662
    #> 12 BioCbo… http… bioc  3.20         aarch64… bin/… 4.4       TRUE  0.754
    #> # i 1 more variable: error <list>

    repo_status(
      platforms = c("windows", "macos"),
      r_version = c("4.0", "4.1")
    )

    #> # A data frame: 18 × 10
    #>    name   url   type  bioc_version platform r_version path  ok      ping
    #>    <chr>  <chr> <chr> <chr>        <chr>    <chr>     <chr> <lgl>  <dbl>
    #>  1 CRAN   http… cran  NA           i386+x8… 4.0       bin/… TRUE   0.175
    #>  2 CRAN   http… cran  NA           i386+x8… 4.1       bin/… TRUE   0.180
    #>  3 CRAN   http… cran  NA           aarch64… 4.1       bin/… FALSE NA
    #>  4 BioCs… http… bioc  3.12         i386+x8… 4.0       bin/… TRUE   0.539
    #>  5 BioCa… http… bioc  3.12         i386+x8… 4.0       bin/… TRUE   0.538
    #>  6 BioCe… http… bioc  3.12         i386+x8… 4.0       bin/… TRUE   1.04
    #>  7 BioCw… http… bioc  3.12         i386+x8… 4.0       bin/… TRUE   0.809
    #>  8 BioCb… http… bioc  3.12         i386+x8… 4.0       bin/… TRUE   0.610
    #>  9 BioCs… http… bioc  3.14         i386+x8… 4.1       bin/… TRUE   1.16
    #> 10 BioCs… http… bioc  3.14         aarch64… 4.1       bin/… FALSE NA
    #> 11 BioCa… http… bioc  3.14         i386+x8… 4.1       bin/… TRUE   0.659
    #> 12 BioCa… http… bioc  3.14         aarch64… 4.1       bin/… FALSE NA
    #> 13 BioCe… http… bioc  3.14         i386+x8… 4.1       bin/… TRUE   0.913
    #> 14 BioCe… http… bioc  3.14         aarch64… 4.1       bin/… FALSE NA
    #> 15 BioCw… http… bioc  3.14         i386+x8… 4.1       bin/… TRUE   0.811
    #> 16 BioCw… http… bioc  3.14         aarch64… 4.1       bin/… FALSE NA
    #> 17 BioCb… http… bioc  3.14         i386+x8… 4.1       bin/… TRUE   1.04
    #> 18 BioCb… http… bioc  3.14         aarch64… 4.1       bin/… FALSE NA
    #> # i 1 more variable: error <list>

    repo_ping()

    #> Repository summary:                   source aarch64-apple-darwin20          
    #> CRAN          @ cloud.r-project.org     v              v              (63ms )
    #> BioCsoft      @ bioconductor.org        v              v              (68ms )
    #> BioCann       @ bioconductor.org        v              v              (153ms)
    #> BioCexp       @ bioconductor.org        v              v              (186ms)
    #> BioCworkflows @ bioconductor.org        v              v              (112ms)
    #> BioCbooks     @ bioconductor.org        v              v              (193ms)

## See also

Other repository functions:
[`repo_add()`](https://pak.r-lib.org/reference/repo_add.md),
[`repo_get()`](https://pak.r-lib.org/reference/repo_get.md)
