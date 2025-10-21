# Metadata cache utilities

Various utilities to inspect, update and clean the metadata cache. See
the pkgcache package if you need for control over the metadata cache.

## Usage

``` r
meta_summary()

meta_list(pkg = NULL)

meta_update()

meta_clean(force = FALSE)
```

## Arguments

- pkg:

  Package names, if specified then only entries for `pkg` are returned.

- force:

  If `FALSE`, then pak will ask for confirmation.

## Value

`meta_summary()` returns a list with entries:

- `cachepath`: absolute path of the metadata cache.

- `current_db`: the file that contains the current metadata database. It
  is currently an RDS file, but this might change in the future.

- `raw_files`: the files that are the downloaded `PACKAGES*` files.

- `db_files`: all metadata database files.

- `size`: total size of the metadata cache.

`meta_list()` returns a data frame of all available packages in the
configured repositories.

`meta_update()` returns nothing.

`meta_clean()` returns nothing

## Details

`meta_summary()` returns a summary of the metadata cache.

`meta_list()` lists all (or some) packages in the metadata database.

`meta_update()` updates the metadata database. You don't normally need
to call this function manually, because all pak functions (e.g.
[`pkg_install()`](https://pak.r-lib.org/dev/reference/pkg_install.md),
[`pkg_download()`](https://pak.r-lib.org/dev/reference/pkg_download.md),
etc.) call it automatically, to make sure that they use the latest
available metadata.

`meta_clean()` deletes the whole metadata DB.

## Examples

Metadata cache summary:

    meta_summary()
    #> $cachepath
    #> [1] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata"
    #>
    #> $current_db
    #> [1] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata/pkgs-34444e3072.rds"
    #>
    #> $raw_files
    #>  [1] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata/BioCann-59693086a0/bin/macosx/big-sur-arm64/contrib/4.2/PACKAGES.gz"
    #>  [2] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata/BioCann-59693086a0/src/contrib/PACKAGES.gz"
    #>  [3] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata/BioCexp-90d4a3978b/bin/macosx/big-sur-arm64/contrib/4.2/PACKAGES.gz"
    #>  [4] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata/BioCexp-90d4a3978b/src/contrib/PACKAGES.gz"
    #>  [5] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata/BioCsoft-2a43920999/bin/macosx/big-sur-arm64/contrib/4.2/PACKAGES.gz"
    #>  [6] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata/BioCsoft-2a43920999/src/contrib/PACKAGES.gz"
    #>  [7] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata/BioCworkflows-26330ba3ca/bin/macosx/big-sur-arm64/contrib/4.2/PACKAGES.gz"
    #>  [8] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata/BioCworkflows-26330ba3ca/src/contrib/PACKAGES.gz"
    #>  [9] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata/CRAN-075c426938/bin/macosx/big-sur-arm64/contrib/4.2/PACKAGES.gz"
    #> [10] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata/CRAN-075c426938/src/contrib/PACKAGES.gz"
    #>
    #> $db_files
    #> [1] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata/pkgs-34444e3072.rds"
    #> [2] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metadata/pkgs-ccacf1b389.rds"
    #>
    #> $size
    #> [1] 174848200

The current metadata DB:

    meta_list()

    #> v Loading metadata database ... done
    #> # A data frame: 45,279 × 32
    #>    package version depends sugge…¹ license imports linki…² archs enhan…³
    #>    <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr> <chr>
    #>  1 A3      1.0.0   R (>= … random… GPL (>… NA      NA      NA    NA
    #>  2 AATtoo… 0.0.2   R (>= … NA      GPL-3   magrit… NA      NA    NA
    #>  3 ABACUS  1.0.0   R (>= … rmarkd… GPL-3   ggplot… NA      NA    NA
    #>  4 ABC.RAP 0.9.0   R (>= … knitr,… GPL-3   graphi… NA      NA    NA
    #>  5 ABCana… 1.2.1   R (>= … NA      GPL-3   plotrix NA      NA    NA
    #>  6 ABCopt… 0.15.0  NA      testth… MIT + … Rcpp, … Rcpp    ABCo… NA
    #>  7 ABCp2   1.2     MASS    NA      GPL-2   NA      NA      NA    NA
    #>  8 ABHgen… 1.0.1   NA      knitr,… GPL-3   ggplot… NA      NA    NA
    #>  9 ABPS    0.3     NA      testth… GPL (>… kernlab NA      NA    NA
    #> 10 ACA     1.1     R (>= … NA      GPL     graphi… NA      NA    NA
    #> # … with 45,269 more rows, 23 more variables:
    #> #   license_restricts_use <chr>, os_type <chr>, priority <chr>,
    #> #   license_is_foss <chr>, repodir <chr>, rversion <chr>,
    #> #   platform <chr>, needscompilation <chr>, ref <chr>, type <chr>,
    #> #   direct <lgl>, status <chr>, target <chr>, mirror <chr>,
    #> #   sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>,
    #> #   built <chr>, published <dttm>, deps <list>, md5sum <chr>, …

Selected packages only:

    meta_list(pkg = c("shiny", "htmlwidgets"))

    #>   package  version depends sugge…¹ license imports linki…² archs enhan…³
    #> * <chr>    <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr> <chr>
    #> 1 htmlwid… 1.5.4   NA      "knitr… MIT + … "grDev… NA      NA    shiny …
    #> 2 shiny    1.7.3   R (>= … "datas… GPL-3 … "utils… NA      NA    NA
    #> 3 htmlwid… 1.6.0   NA      "testt… MIT + … "grDev… NA      NA    shiny …
    #> 4 shiny    1.7.4   R (>= … "datas… GPL-3 … "utils… NA      NA    NA
    #> # … with 23 more variables: license_restricts_use <chr>, os_type <chr>,
    #> #   priority <chr>, license_is_foss <chr>, repodir <chr>,
    #> #   rversion <chr>, platform <chr>, needscompilation <chr>, ref <chr>,
    #> #   type <chr>, direct <lgl>, status <chr>, target <chr>, mirror <chr>,
    #> #   sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>,
    #> #   built <chr>, published <dttm>, deps <list>, md5sum <chr>,
    #> #   path <chr>, and abbreviated variable names ¹​suggests, ²​linkingto, 
    #> …                                                                       

Update the metadata DB

    meta_update()

    #> v Updated metadata database: 1.23 MB in 1 file.
    #> v Updating metadata database ... done

Delete the metadata DB

    meta_clean()

    #> i Cleaning up cache directory /Users/gaborcsardi/Library/Caches/org.R-pr
    #> oject.R/R/pkgcache/_metadata.                                           
