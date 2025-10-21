# Package cache utilities

Various utilities to inspect and clean the package cache. See the
pkgcache package if you need for control over the package cache.

## Usage

``` r
cache_summary()

cache_list(...)

cache_delete(...)

cache_clean()
```

## Arguments

- ...:

  For `cache_list()` and `cache_delete()`, `...` may contain filters,
  where the argument name is the column name. E.g. `package`, `version`,
  etc. Call `cache_list()` without arguments to see the available column
  names. If you call `cache_delete()` without arguments, it will delete
  all cached files.

## Value

`cache_summary()` returns a list with elements:

- `cachepath`: absolute path to the package cache

- `files`: number of files (packages) in the cache

- `size`: total size of package cache in bytes

`cache_list()` returns a data frame with the data about the cache.

`cache_delete()` returns nothing.

`cache_clean()` returns nothing.

## Details

`cache_summary()` returns a summary of the package cache.

`cache_list()` lists all (by default), or a subset of packages in the
package cache.

`cache_delete()` deletes files from the cache.

`cache_clean()` deletes all files from the cache.

## Examples

    cache_summary()

    #> $cachepath
    #> [1] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/pkg"
    #>
    #> $files
    #> [1] 483
    #>
    #> $size
    #> [1] 654662486
    #>

    cache_list()

    #> # A data frame: 483 × 11
    #>    fullpath       path  package url   etag  sha256 version platf…¹ built
    #>    <chr>          <chr> <chr>   <chr> <chr> <chr>  <chr>   <chr>   <int>
    #>  1 /Users/gaborc… arch… NA      http… "\"1… 0c8f0… NA      NA         NA
    #>  2 /Users/gaborc… bin/… evalua… http… "\"1… 08a39… 0.17    aarch6…    NA
    #>  3 /Users/gaborc… bin/… crayon  http… "\"2… 1e6d5… 1.5.2   aarch6…    NA
    #>  4 /Users/gaborc… bin/… common… http… "\"4… 47b4a… 1.8.1   aarch6…    NA
    #>  5 /Users/gaborc… bin/… curl    http… "\"b… 7b8ba… 4.3.3   aarch6…    NA
    #>  6 /Users/gaborc… bin/… tinytex http… "\"2… 7e9ba… 0.42    aarch6…    NA
    #>  7 /Users/gaborc… bin/… jsonli… http… "\"1… 68e59… 1.8.2   aarch6…    NA
    #>  8 /Users/gaborc… bin/… lifecy… http… "\"1… 7ce27… 1.0.3   aarch6…    NA
    #>  9 /Users/gaborc… bin/… vctrs   http… "\"1… c3a69… 0.4.2   aarch6…    NA
    #> 10 /Users/gaborc… src/… pkgcac… NA     NA   9b70a… NA      NA          0
    #> # … with 473 more rows, 2 more variables: vignettes <int>,
    #> #   rversion <chr>, and abbreviated variable name ¹​platform

    cache_list(package = "recipes")

    #> # A data frame: 1 × 11
    #>   fullp…¹ path  package url   etag  sha256 version platf…² built vigne…³
    #>   <chr>   <chr> <chr>   <chr> <chr> <chr>  <chr>   <chr>   <int>   <int>
    #> 1 /Users… bin/… recipes http… "\"1… e281e… 1.0.2   aarch6…    NA      NA
    #> # … with 1 more variable: rversion <chr>, and abbreviated variable
    #> #   names ¹​fullpath, ²​platform, ³​vignettes

    cache_list(platform = "source")

    #> # A data frame: 69 × 11
    #>    fullpath       path  package url   etag  sha256 version platf…¹ built
    #>    <chr>          <chr> <chr>   <chr> <chr> <chr>  <chr>   <chr>   <int>
    #>  1 /Users/gaborc… src/… crayon  http… "\"9… 70a9a… 1.5.2   source     NA
    #>  2 /Users/gaborc… src/… zip     http… "\"1… 14873… 2.2.1   source     NA
    #>  3 /Users/gaborc… src/… curl    http… "\"a… 3567b… 4.3.3   source     NA
    #>  4 /Users/gaborc… src/… rlang   http… "\"b… e6973… 1.0.6   source     NA
    #>  5 /Users/gaborc… src/… openssl http… "\"1… 7cde9… 2.0.3   source     NA
    #>  6 /Users/gaborc… src/… tinytex http… "\"8… 205f7… 0.42    source     NA
    #>  7 /Users/gaborc… src/… evalua… http… "\"6… 49c74… 0.17    source     NA
    #>  8 /Users/gaborc… src/… Rcpp    http… "\"2… 807ce… 1.0.9   source     NA
    #>  9 /Users/gaborc… src/… knitr   http… "\"d… 9b8f9… 1.40    source     NA
    #> 10 /Users/gaborc… src/… lpSolve http… "\"7… f7258… 5.6.17  source     NA
    #> # … with 59 more rows, 2 more variables: vignettes <int>,
    #> #   rversion <chr>, and abbreviated variable name ¹​platform

    cache_delete(package = "knitr")
    cache_delete(platform = "macos")

    cache_clean()
