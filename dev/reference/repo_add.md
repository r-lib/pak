# Add a new CRAN-like repository

Add a new repository to the list of repositories that pak uses to look
for packages.

## Usage

``` r
repo_add(..., .list = NULL, username = NULL)

repo_resolve(spec, username = NULL)
```

## Arguments

- ...:

  Repository specifications, possibly named character vectors. See
  details below.

- .list:

  List or character vector of repository specifications. This argument
  is easier to use programmatically than `...`. See details below.

- username:

  User name to set, for authenticated repositories, see
  [`repo_auth()`](https://pak.r-lib.org/dev/reference/repo_auth.md).

- spec:

  Repository specification, a possibly named character scalar.

## Value

`repo_resolve()` returns a named character scalar, the URL of the
repository.

## Details

`repo_add()` adds new repositories. It resolves the specified
repositories using `repo_resolve()` and then modifies the `repos` global
option.

`repo_add()` only has an effect in the current R session. If you want to
keep your configuration between R sessions, then set the `repos` option
to the desired value in your user or project `.Rprofile` file.

## Repository specifications

The format of a repository specification is a named or unnamed character
scalar. If the name is missing, pak adds a name automatically. The
repository named `CRAN` is the main CRAN repository, but otherwise names
are informational.

Currently supported repository specifications:

- URL pointing to the root of the CRAN-like repository. Example:

      https://cloud.r-project.org

- `PPM@latest`, PPM (Posit Package Manager, formerly RStudio Package
  Manager), the latest snapshot.

- `PPM@<date>`, PPM (Posit Package Manager, formerly RStudio Package
  Manager) snapshot, at the specified date.

- `PPM@<package>-<version>` PPM snapshot, for the day after the release
  of `<version>` of `<package>`.

- `PPM@R-<version>` PPM snapshot, for the day after R `<version>` was
  released.

Still works for dates starting from 2017-10-10, but now deprecated,
because MRAN is discontinued:

- `MRAN@<date>`, MRAN (Microsoft R Application Network) snapshot, at the
  specified date.

- `MRAN@<package>-<version>` MRAN snapshot, for the day after the
  release of `<version>` of `<package>`.

- `MRAN@R-<version>` MRAN snapshot, for the day after R `<version>` was
  released.

Notes:

- See more about PPM at <https://packagemanager.posit.co/client/#/>.

- The `RSPM@` prefix is still supported and treated the same way as
  `PPM@`.

- The MRAN service is now retired, see
  `https://techcommunity.microsoft.com/blog/azuresqlblog/microsoft-r-application-network-retirement/3707161`
  for details.

- `MRAN@...` repository specifications now resolve to PPM, but note that
  PPM snapshots are only available from 2017-10-10. See more about this
  at
  <https://posit.co/blog/migrating-from-mran-to-posit-package-manager/>.

- All dates (or times) can be specified in the ISO 8601 format.

- If PPM does not have a snapshot available for a date, the next
  available date is used.

- Dates that are before the first, or after the last PPM snapshot will
  trigger an error.

- Unknown R or package versions will trigger an error.

## Examples

    repo_add(PPMdplyr100 = "PPM@dplyr-1.0.0")
    repo_get()

    #> # A data frame: 7 × 5
    #>   name          url                         type  r_version bioc_version
    #> * <chr>         <chr>                       <chr> <chr>     <chr>
    #> 1 CRAN          https://cloud.r-project.org cran  *         NA
    #> 2 PPMdplyr100   https://packagemanager.pos… cran… *         NA
    #> 3 BioCsoft      https://bioconductor.org/p… bioc  4.4.2     3.20
    #> 4 BioCann       https://bioconductor.org/p… bioc  4.4.2     3.20
    #> 5 BioCexp       https://bioconductor.org/p… bioc  4.4.2     3.20
    #> 6 BioCworkflows https://bioconductor.org/p… bioc  4.4.2     3.20
    #> 7 BioCbooks     https://bioconductor.org/p… bioc  4.4.2     3.20

    repo_resolve("PPM@2020-01-21")

    #>                                              CRAN
    #> "https://packagemanager.posit.co/cran/2020-01-21"

    repo_resolve("PPM@dplyr-1.0.0")

    #>                                              CRAN
    #> "https://packagemanager.posit.co/cran/2020-05-30"

    repo_resolve("PPM@R-4.0.0")

    #>                                              CRAN
    #> "https://packagemanager.posit.co/cran/2020-04-25"

## See also

Other repository functions:
[`repo_get()`](https://pak.r-lib.org/dev/reference/repo_get.md),
[`repo_status()`](https://pak.r-lib.org/dev/reference/repo_status.md)
