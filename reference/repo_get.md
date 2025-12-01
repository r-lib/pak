# Query the currently configured CRAN-like repositories

pak uses the `repos` option, see
[`options()`](https://rdrr.io/r/base/options.html). It also
automatically adds a CRAN mirror if none is set up, and the correct
version of the Bioconductor repositories. See the `cran_mirror` and
`bioc` arguments.

## Usage

``` r
repo_get(r_version = getRversion(), bioc = NULL, cran_mirror = NULL)
```

## Arguments

- r_version:

  R version to use to determine the correct Bioconductor version, if
  `bioc = TRUE`.

- bioc:

  Whether to automatically add the Bioconductor repositories to the
  result.

- cran_mirror:

  CRAN mirror to use. Leave it at `NULL` to use the mirror in
  `getOption("repos")` or an automatically selected one.

## Value

`repo_get()` returns a data frame with columns:

- `name`: repository name. Names are informational only.

- `url`: repository URL.

- `type`: repository type. This is also informational, currently it can
  be `cran` for CRAN, `bioc` for a Bioconductor repository, and
  `cranlike`: for other repositories.

- `r_version`: R version that is supposed to be used with this
  repository. This is only set for Bioconductor repositories. It is `*`
  for others. This is also informational, and not used when retrieving
  the package metadata.

- `bioc_version`: Bioconductor version. Only set for Bioconductor
  repositories, and it is `NA` for others.

- `username`: user name, for authenticated repositories.

- `has_password`: whether `repo_get()` could find the password for this
  repository. Call
  [`repo_auth()`](https://pak.r-lib.org/reference/repo_auth.md) for more
  information if the credential lookup failed.

## Details

`repo_get()` returns the table of the currently configured repositories.

## Examples

    repo_get()

    #> # A data frame: 6 × 5
    #>   name          url                         type  r_version bioc_version
    #> * <chr>         <chr>                       <chr> <chr>     <chr>
    #> 1 CRAN          https://cloud.r-project.org cran  *         NA
    #> 2 BioCsoft      https://bioconductor.org/p… bioc  4.4.2     3.20
    #> 3 BioCann       https://bioconductor.org/p… bioc  4.4.2     3.20
    #> 4 BioCexp       https://bioconductor.org/p… bioc  4.4.2     3.20
    #> 5 BioCworkflows https://bioconductor.org/p… bioc  4.4.2     3.20
    #> 6 BioCbooks     https://bioconductor.org/p… bioc  4.4.2     3.20

## See also

Other repository functions:
[`repo_add()`](https://pak.r-lib.org/reference/repo_add.md),
[`repo_status()`](https://pak.r-lib.org/reference/repo_status.md)
