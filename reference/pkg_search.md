# Search CRAN packages

Search the indexed database of current CRAN packages. It uses the
pkgsearch package. See that package for more details and also
[`pkgsearch::pkg_search()`](https://r-hub.github.io/pkgsearch/reference/pkg_search.html)
for pagination, more advanced searching, etc.

## Usage

``` r
pkg_search(query, ...)
```

## Arguments

- query:

  Search query string.

- ...:

  Arguments passed on to
  [`pkgsearch::pkg_search`](https://r-hub.github.io/pkgsearch/reference/pkg_search.html)

  `from`

  :   Where to start listing the results, for pagination.

  `size`

  :   The number of results to list.

## Value

A data frame, that is also a `pak_search_result` object with a custom
print method. To see the underlying table, you can use `[]` to drop the
extra classes. See examples below.

## Examples

Simple search

    pkg_search("survival")

    #>
    #> 1 survival 3.4.0 -- by Terry M Therneau, 4 months ago
    #>   Survival Analysis
    #>
    #> 2 survminer 0.4.9 -- by Alboukadel Kassambara, 2 years ago
    #>   Drawing Survival Curves using 'ggplot2'
    #>
    #> 3 flexsurv 2.2 -- by Christopher Jackson, 6 months ago
    #>   Flexible Parametric Survival and Multi-State Models
    #>
    #> 4 muhaz 1.2.6.4 -- by David Winsemius, 2 years ago
    #>   Hazard Function Estimation in Survival Analysis
    #>
    #> 5 pec 2022.5.4 -- by Thomas A. Gerds, 8 months ago
    #>   Prediction Error Curves for Risk Prediction Models in Survival Analysi
    #> s
    #>
    #> 6 randomForestSRC 3.1.1 -- by Udaya B. Kogalur, 5 months ago
    #>   Fast Unified Random Forests for Survival, Regression, and Classificati
    #> on (RF-SRC)
    #>
    #> 7 relsurv 2.2.8 -- by Damjan Manevski, 4 months ago
    #>   Relative Survival
    #>
    #> 8 survRM2 1.0.4 -- by Hajime Uno, 6 months ago
    #>   Comparing Restricted Mean Survival Time
    #>
    #> 9 titanic 0.1.0 -- by Paul Hendricks, 7 years ago
    #>   Titanic Passenger Survival Data Set
    #>
    #> 10 KMsurv 0.1.5 -- by Jun Yan, 10 years ago
    #>   Data sets from Klein and Moeschberger (1997), Survival Analysis

See the underlying data frame

    psro <- pkg_search("ropensci")
    psro[]

    #> # A data frame: 10 × 15
    #>    score package    version    title descr…¹ date                maint…²
    #>    <dbl> <chr>      <pckg_vrs> <chr> <chr>   <dttm>              <chr>
    #>  1  538. webmockr   0.8.2      Stub… "Stubb… 2022-08-28 19:20:02 Scott …
    #>  2  520. RSelenium  1.7.9      R Bi… "Provi… 2022-09-02 07:10:11 Ju Yeo…
    #>  3  416. tracerer   2.2.2      Trac… "'BEAS… 2021-05-30 08:40:03 Richèl…
    #>  4  376. rfisheries 0.2        'Pro… "A pro… 2016-02-19 08:50:03 Karthi…
    #>  5  367. mcbette    1.15       Mode… "'BEAS… 2022-08-27 12:30:02 Richèl…
    #>  6  359. taxize     0.9.100    Taxo… "Inter… 2022-04-22 07:30:02 Zachar…
    #>  7  350. beastier   2.4.11     Call… "'BEAS… 2022-08-11 13:40:04 Richèl…
    #>  8  347. spocc      1.2.0      Inte… "A pro… 2021-01-05 19:50:03 Scott …
    #>  9  316. chromer    0.3        Inte… "A pro… 2022-10-27 22:45:36 Karl W…
    #> 10  315. visdat     0.5.3      Prel… "Creat… 2019-02-15 14:30:03 Nichol…
    #> # … with 8 more variables: maintainer_email <chr>, revdeps <int>,
    #> #   downloads_last_month <int>, license <chr>, url <chr>,
    #> #   bugreports <chr>, package_data <I<list>>, ago <chr>, and
    #> #   abbreviated variable names ¹​description, ²​maintainer_name
