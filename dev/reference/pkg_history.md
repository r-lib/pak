# Query the history of a CRAN package

Query the history of a CRAN package

## Usage

``` r
pkg_history(pkg)
```

## Arguments

- pkg:

  Package name.

## Value

A data frame, with one row per package version. The columns are the
entries of the `DESCRIPTION` files in the released package versions.

## Examples

    pkg_history("ggplot2")

    #> # A data frame: 44 × 30
    #>    Package Type    Title    Version Date  Author Maint…¹ Descr…² License
    #>  * <chr>   <chr>   <chr>    <chr>   <chr> <chr>  <chr>   <chr>   <chr>
    #>  1 ggplot2 Package An impl… 0.5     2007… Hadle… Hadley… "An im… GPL
    #>  2 ggplot2 Package An impl… 0.5.1   2007… Hadle… Hadley… "An im… GPL
    #>  3 ggplot2 Package An impl… 0.5.2   2007… Hadle… Hadley… "An im… GPL
    #>  4 ggplot2 Package An impl… 0.5.4   2007… Hadle… Hadley… "An im… GPL
    #>  5 ggplot2 Package An impl… 0.5.5   2007… Hadle… Hadley… "An im… GPL
    #>  6 ggplot2 Package An impl… 0.5.6   2007… Hadle… Hadley… "An im… GPL
    #>  7 ggplot2 Package An impl… 0.5.7   2007… Hadle… Hadley… "An im… GPL
    #>  8 ggplot2 Package An impl… 0.6     2008… Hadle… Hadley… "An im… GPL
    #>  9 ggplot2 Package An impl… 0.7     2008… Hadle… Hadley… "An im… GPL
    #> 10 ggplot2 Package An impl… 0.8     2008… Hadle… Hadley… "An im… GPL
    #> # … with 34 more rows, 21 more variables: SaveImage <chr>,
    #> #   LazyData <chr>, Packaged <chr>, crandb_file_date <chr>, date <chr>,
    #> #   dependencies <list>, URL <chr>, LazyLoad <chr>, Extends <chr>,
    #> #   Collate <chr>, Repository <chr>, `Date/Publication` <chr>,
    #> #   NeedsCompilation <chr>, VignetteBuilder <chr>, BugReports <chr>,
    #> #   `Authors@R` <chr>, RoxygenNote <chr>, Encoding <chr>, MD5sum <chr>,
    #> #   `Config/Needs/website` <chr>, `Config/testthat/edition` <chr>, …
