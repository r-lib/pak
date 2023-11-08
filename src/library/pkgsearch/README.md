# Search and Query CRAN R Packages

<!-- badges: start -->
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable-1)
[![R build status](https://github.com/r-hub/pkgsearch/workflows/R-CMD-check/badge.svg)](https://github.com/r-hub/pkgsearch/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/pkgsearch)](https://cran.r-project.org/package=pkgsearch)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/pkgsearch)](https://www.r-pkg.org/pkg/pkgsearch)
[![Coverage status](https://codecov.io/gh/r-hub/pkgsearch/branch/master/graph/badge.svg)](https://codecov.io/github/r-hub/pkgsearch?branch=master)
<!-- badges: end -->

`pkgsearch` uses R-hub web services that munge CRAN metadata and let you
access it through several lenses.

-   [Installation](#installation)
-   [Usage](#usage)
    -   [Search relevant packages](#search-relevant-packages)
    -   [Do it all *clicking*](#do-it-all-clicking)
    -   [Get package metadata](#get-package-metadata)
    -   [Discover packages](#discover-packages)
    -   [Keep up with CRAN](#keep-up-with-cran)
-   [Search features](#search-features)
    -   [More details](#more-details)
    -   [Pagination](#pagination)
    -   [Stemming](#stemming)
    -   [Ranking](#ranking)
    -   [Preferring Phrases](#preferring-phrases)
    -   [British vs American English](#british-vs-american-english)
    -   [Ascii Folding](#ascii-folding)
-   [More info](#more-info)
-   [License](#license)

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Installation

Install the latest pkgsearch release from CRAN:

``` r
install.packages("pkgsearch")
```

## Usage

### Search relevant packages

Do you need to find packages solving a particular problem, e.g.
“permutation test”?

``` r
library("pkgsearch")
library("pillar") # nicer data frame printing
pkg_search("permutation test")
```

    #> - "permutation test" ------------------------------------ 2236 packages in 0.01 seconds -
    #>   #     package        version by                      @ title                           
    #>   1 100 coin           1.4.2   Torsten Hothorn        4M Conditional Inference Procedu...
    #>   2  31 perm           1.0.0.2 Michael P. Fay         4M Exact or Asymptotic Permutati...
    #>   3  30 exactRankTests 0.8.34  Torsten Hothorn        4M Exact Distributions for Rank ...
    #>   4  29 flip           2.5.0   Livio Finos            3y Multivariate Permutation Tests  
    #>   5  22 jmuOutlier     2.2     Steven T. Garren       3y Permutation Tests for Nonpara...
    #>   6  19 wPerm          1.0.1   Neil A. Weiss          6y Permutation Tests               
    #>   7  16 cpt            1.0.2   Johann Gagnon-Bartsch  3y Classification Permutation Test 
    #>   8  16 GlobalDeviance 0.4     Frederike Fuhlbrueck   8y Global Deviance Permutation T...
    #>   9  16 permutes       2.3.2   Cesko C. Voeten        2M Permutation Tests for Time Se...
    #>  10  16 AUtests        0.99    Arjun Sondhi           1y Approximate Unconditional and...

pkgsearch uses an [R-hub](https://docs.r-hub.io) web service and a
careful ranking that puts popular packages before less frequently used
ones.

### Do it all *clicking*

For the search mentioned above, and other points of entry to CRAN
metadata, you can use pkgsearch RStudio add-in!

[![Addin
screencast](https://raw.githubusercontent.com/r-hub/pkgsearch/master/gifs/addin.gif)](https://vimeo.com/375618736)

Select the “CRAN package search” addin from the menu, or start it with
`pkg_search_addin()`.

### Get package metadata

Do you want to find the dependencies the first versions of `testthat`
had and when each of these versions was released?

``` r
cran_package_history("testthat")
```

    #> # A data frame: 35 × 29
    #>    Package  Type    Title     Version Author Maintainer Description URL   License LazyData
    #>  * <chr>    <chr>   <chr>     <chr>   <chr>  <chr>      <chr>       <chr> <chr>   <chr>   
    #>  1 testthat Package Tools fo… 0.1     Hadle… Hadley Wi… Test_that … http… GPL     true    
    #>  2 testthat Package Testthat… 0.1.1   Hadle… Hadley Wi… A testing … http… GPL     true    
    #>  3 testthat Package Testthat… 0.2     Hadle… Hadley Wi… A testing … http… GPL     true    
    #>  4 testthat Package Testthat… 0.3     Hadle… Hadley Wi… A testing … http… GPL     true    
    #>  5 testthat Package Testthat… 0.4     Hadle… Hadley Wi… A testing … http… GPL     true    
    #>  6 testthat Package Testthat… 0.5     Hadle… Hadley Wi… A testing … http… GPL     true    
    #>  7 testthat Package Testthat… 0.6     Hadle… Hadley Wi… A testing … http… GPL     true    
    #>  8 testthat Package Testthat… 0.7     Hadle… Hadley Wi… A testing … http… GPL     true    
    #>  9 testthat Package Testthat… 0.7.1   Hadle… Hadley Wi… A testing … http… GPL     true    
    #> 10 testthat Package Testthat… 0.8     Hadle… Hadley Wi… A testing … http… MIT + … true    
    #> # … with 25 more rows, and 19 more variables: Collate <chr>, Packaged <chr>,
    #> #   Repository <chr>, `Date/Publication` <chr>, crandb_file_date <chr>, date <chr>,
    #> #   dependencies <list>, NeedsCompilation <chr>, Roxygen <chr>, `Authors@R` <chr>,
    #> #   BugReports <chr>, RoxygenNote <chr>, VignetteBuilder <chr>, Encoding <chr>,
    #> #   MD5sum <chr>, `Config/testthat/edition` <chr>, `Config/testthat/parallel` <chr>,
    #> #   `Config/testthat/start-first` <chr>, `Config/Needs/website` <chr>

### Discover packages

Do you want to know what packages are trending on CRAN these days?
`pkgsearch` can help!

``` r
cran_trending()
```

    #> # A data frame: 100 × 2
    #>    package          score                
    #>    <chr>            <chr>                
    #>  1 spatstat.random  3084.3177189409368600
    #>  2 fstcore          2818.8976377952755900
    #>  3 collapse         1743.6553713049747700
    #>  4 r5r              916.4171779141104300 
    #>  5 RcppTOML         614.1354563348462200 
    #>  6 maditr           415.9413882397524200 
    #>  7 reproj           399.2964304190377700 
    #>  8 RGoogleAnalytics 382.0643996388805300 
    #>  9 crsmeta          371.6803643925141100 
    #> 10 gsalib           352.6695776231031900 
    #> # … with 90 more rows

``` r
cran_top_downloaded()
```

    #> # A data frame: 100 × 2
    #>    package     count 
    #>    <chr>       <chr> 
    #>  1 ggplot2     537944
    #>  2 rlang       510888
    #>  3 ragg        417993
    #>  4 textshaping 416529
    #>  5 dplyr       414192
    #>  6 sf          403120
    #>  7 jsonlite    401135
    #>  8 cli         383482
    #>  9 pillar      366216
    #> 10 lifecycle   362196
    #> # … with 90 more rows

### Keep up with CRAN

Are you curious about the latest releases or archivals?

``` r
cran_events()
```

    #> CRAN events (events)---------------------------------------------------------------------
    #>  . When     Package    Version Title                                                     
    #>  + 4 hours  bigsnpr    1.9.10  Analysis of Massive SNP Arrays                            
    #>  - 5 hours  report     0.5.0   Automated Reporting of Results and Statistical Models     
    #>  - 5 hours  miniCRAN   0.2.14  Create a Mini Version of CRAN Containing Only Selected ...
    #>  + 7 hours  RFishBC    0.2.4   Back-Calculation of Fish Length                           
    #>  + 7 hours  rEDM       1.10.2  Empirical Dynamic Modeling ('EDM')                        
    #>  + 8 hours  rsmatrix   0.2.1   Matrices for Repeat-Sales Price Indexes                   
    #>  + 10 hours wallace    1.1.2   A Modular Platform for Reproducible Modeling of Species...
    #>  + 10 hours fastmatrix 0.4     Fast Computation of some Matrices Useful in Statistics    
    #>  + 10 hours FSAdata    0.3.9   Data to Support Fish Stock Assessment ('FSA') Package     
    #>  + 10 hours ggdist     3.1.0   Visualizations of Distributions and Uncertainty

## Search features

### More details

By default it returns a short summary of the ten best search hits. Their
details can be printed by using the `format = "long"` option of
`pkg_search()`, or just calling `pkg_search()` again, without any
arguments, after a search:

``` r
library(pkgsearch)
pkg_search("C++")
```

    #> - "C++" ----------------------------------------------- 11837 packages in 0.006 seconds -
    #>   #     package      version  by                    @ title                              
    #>   1 100 Rcpp         1.0.8    Dirk Eddelbuettel    1M Seamless R and C++ Integration     
    #>   2  35 markdown     1.1      Yihui Xie            3y Render Markdown with the C Libra...
    #>   3  32 BH           1.78.0.0 Dirk Eddelbuettel    2M Boost C++ Header Files             
    #>   4  18 StanHeaders  2.21.0.7 Ben Goodrich         1y C++ Header Files for Stan          
    #>   5  13 RcppProgress 0.4.2    Karl Forner          2y An Interruptible Progress Bar wi...
    #>   6  12 cpp11        0.4.2    Romain François      3M A C++11 Interface for R's C Inte...
    #>   7  12 covr         3.5.1    Jim Hester           1y Test Coverage for Packages         
    #>   8  10 inline       0.3.19   Dirk Eddelbuettel    9M Functions to Inline C, C++, Fort...
    #>   9   9 SnowballC    0.7.0    Milan Bouchet-Valat  2y Snowball Stemmers Based on the C...
    #>  10   8 RcppThread   2.0.2    Thomas Nagler        7d R-Friendly Threading in C++

``` r
pkg_search()
```

    #> - "C++" ----------------------------------------------- 11837 packages in 0.006 seconds -
    #> 
    #> 1 Rcpp @ 1.0.8                                       Dirk Eddelbuettel, about a month ago
    #> --------------
    #>   # Seamless R and C++ Integration
    #>   The 'Rcpp' package provides R functions as well as C++ classes which offer a
    #>   seamless integration of R and C++. Many R data types and objects can be mapped
    #>   back and forth to C++ equivalents which facilitates both writing of new code
    #>   as well as easier integration of third-party libraries. Documentation about
    #>   'Rcpp' is provided by several vignettes included in this package, via the
    #>   'Rcpp Gallery' site at <https://gallery.rcpp.org>, the paper by Eddelbuettel
    #>   and Francois (2011, <doi:10.18637/jss.v040.i08>), the book by Eddelbuettel
    #>   (2013, <doi:10.1007/978-1-4614-6868-4>) and the paper by Eddelbuettel and
    #>   Balamuta (2018, <doi:10.1080/00031305.2017.1375990>); see 'citation("Rcpp")'
    #>   for details.
    #>   http://www.rcpp.org
    #>   https://dirk.eddelbuettel.com/code/rcpp.html
    #>   https://github.com/RcppCore/Rcpp
    #> 
    #> 2 markdown @ 1.1                                                   Yihui Xie, 3 years ago
    #> ----------------
    #>   # Render Markdown with the C Library 'Sundown'
    #>   Provides R bindings to the 'Sundown' Markdown rendering library
    #>   (<https://github.com/vmg/sundown>). Markdown is a plain-text formatting syntax
    #>   that can be converted to 'XHTML' or other formats. See
    #>   <http://en.wikipedia.org/wiki/Markdown> for more information about Markdown.
    #>   https://github.com/rstudio/markdown
    #> 
    #> 3 BH @ 1.78.0.0                                           Dirk Eddelbuettel, 2 months ago
    #> ---------------
    #>   # Boost C++ Header Files
    #>   Boost provides free peer-reviewed portable C++ source libraries.  A large part
    #>   of Boost is provided as C++ template code which is resolved entirely at
    #>   compile-time without linking.  This package aims to provide the most useful
    #>   subset of Boost libraries for template use among CRAN packages. By placing
    #>   these libraries in this package, we offer a more efficient distribution system
    #>   for CRAN as replication of this code in the sources of other packages is
    #>   avoided. As of release 1.78.0-0, the following Boost libraries are included:
    #>   'accumulators' 'algorithm' 'align' 'any' 'atomic' 'beast' 'bimap' 'bind'
    #>   'circular_buffer' 'compute' 'concept' 'config' 'container' 'date_time'
    #>   'detail' 'dynamic_bitset' 'exception' 'flyweight' 'foreach' 'functional'
    #>   'fusion' 'geometry' 'graph' 'heap' 'icl' 'integer' 'interprocess' 'intrusive'
    #>   'io' 'iostreams' 'iterator' 'lambda2' 'math' 'move' 'mp11' 'mpl'
    #>   'multiprecision' 'numeric' 'pending' 'phoenix' 'polygon' 'preprocessor'
    #>   'process' 'propery_tree' 'random' 'range' 'scope_exit' 'smart_ptr' 'sort'
    #>   'spirit' 'tuple' 'type_traits' 'typeof' 'unordered' 'utility' 'uuid'.
    #>   https://github.com/eddelbuettel/bh
    #>   https://dirk.eddelbuettel.com/code/bh.html
    #> 
    #> 4 StanHeaders @ 2.21.0.7                                   Ben Goodrich, about a year ago
    #> ------------------------
    #>   # C++ Header Files for Stan
    #>   The C++ header files of the Stan project are provided by this package, but it
    #>   contains little R code or documentation. The main reference is the vignette.
    #>   There is a shared object containing part of the 'CVODES' library, but its
    #>   functionality is not accessible from R. 'StanHeaders' is primarily useful for
    #>   developers who want to utilize the 'LinkingTo' directive of their package's
    #>   DESCRIPTION file to build on the Stan library without incurring unnecessary
    #>   dependencies. The Stan project develops a probabilistic programming language
    #>   that implements full or approximate Bayesian statistical inference via Markov
    #>   Chain Monte Carlo or 'variational' methods and implements (optionally
    #>   penalized) maximum likelihood estimation via optimization. The Stan library
    #>   includes an advanced automatic differentiation scheme, 'templated' statistical
    #>   and linear algebra functions that can handle the automatically
    #>   'differentiable' scalar types (and doubles, 'ints', etc.), and a parser for
    #>   the Stan language. The 'rstan' package provides user-facing R functions to
    #>   parse, compile, test, estimate, and analyze Stan models.
    #>   https://mc-stan.org/
    #> 
    #> 5 RcppProgress @ 0.4.2                                           Karl Forner, 2 years ago
    #> ----------------------
    #>   # An Interruptible Progress Bar with OpenMP Support for C++ in R Packages
    #>   Allows to display a progress bar in the R console for long running
    #>   computations taking place in c++ code, and support for interrupting those
    #>   computations even in multithreaded code, typically using OpenMP.
    #>   https://github.com/kforner/rcpp_progress
    #> 
    #> 6 cpp11 @ 0.4.2                                             Romain François, 3 months ago
    #> ---------------
    #>   # A C++11 Interface for R's C Interface
    #>   Provides a header only, C++11 interface to R's C interface.  Compared to other
    #>   approaches 'cpp11' strives to be safe against long jumps from the C API as
    #>   well as C++ exceptions, conform to normal R function semantics and supports
    #>   interaction with 'ALTREP' vectors.
    #>   https://cpp11.r-lib.org
    #>   https://github.com/r-lib/cpp11
    #> 
    #> 7 covr @ 3.5.1                                               Jim Hester, about a year ago
    #> --------------
    #>   # Test Coverage for Packages
    #>   Track and report code coverage for your package and (optionally) upload the
    #>   results to a coverage service like 'Codecov' <https://codecov.io> or
    #>   'Coveralls' <https://coveralls.io>. Code coverage is a measure of the amount
    #>   of code being exercised by a set of tests. It is an indirect measure of test
    #>   quality and completeness. This package is compatible with any testing
    #>   methodology or framework and tracks coverage of both R code and compiled
    #>   C/C++/FORTRAN code.
    #>   https://covr.r-lib.org
    #>   https://github.com/r-lib/covr
    #> 
    #> 8 inline @ 0.3.19                                         Dirk Eddelbuettel, 9 months ago
    #> -----------------
    #>   # Functions to Inline C, C++, Fortran Function Calls from R
    #>   Functionality to dynamically define R functions and S4 methods with 'inlined'
    #>   C, C++ or Fortran code supporting the .C and .Call calling conventions.
    #>   https://github.com/eddelbuettel/inline
    #>   https://dirk.eddelbuettel.com/code/inline.html
    #> 
    #> 9 SnowballC @ 0.7.0                                      Milan Bouchet-Valat, 2 years ago
    #> -------------------
    #>   # Snowball Stemmers Based on the C 'libstemmer' UTF-8 Library
    #>   An R interface to the C 'libstemmer' library that implements Porter's word
    #>   stemming algorithm for collapsing words to a common root to aid comparison of
    #>   vocabulary. Currently supported languages are Danish, Dutch, English, Finnish,
    #>   French, German, Hungarian, Italian, Norwegian, Portuguese, Romanian, Russian,
    #>   Spanish, Swedish and Turkish.
    #>   https://github.com/nalimilan/R.TeMiS
    #> 
    #> 10 RcppThread @ 2.0.2                                           Thomas Nagler, 7 days ago
    #> ---------------------
    #>   # R-Friendly Threading in C++
    #>   Provides a C++11-style thread class and thread pool that can safely be
    #>   interrupted from R. See Nagler (2021) <doi:10.18637/jss.v097.c01>.
    #>   https://github.com/tnagler/RcppThread

### Pagination

The `more()` function can be used to display the next batch of search
hits, batches contain ten packages by default. `ps()` is a shorter alias
to `pkg_search()`:

``` r
ps("google")
```

    #> - "google" ---------------------------------------------- 155 packages in 0.005 seconds -
    #>   #     package             version by               @ title                             
    #>   1 100 googledrive         2.0.0   Jennifer Bryan  7M An Interface to Google Drive      
    #>   2  93 googleVis           0.6.11  Markus Gesmann  1M R Interface to Google Charts      
    #>   3  93 googleAuthR         2.0.0   Mark Edmondson 16d Authenticate and Create Google ...
    #>   4  87 lubridate           1.8.0   Vitalie Spinu   4M Make Dealing with Dates a Littl...
    #>   5  83 gargle              1.2.0   Jennifer Bryan  8M Utilities for Working with Goog...
    #>   6  59 googleCloudStorageR 0.7.0   Mark Edmondson  2M Interface with Google Cloud Sto...
    #>   7  58 googlesheets4       1.0.0   Jennifer Bryan  7M Access Google Sheets using the ...
    #>   8  56 gsheet              0.4.5   Max Conway      2y Download Google Sheets Using Ju...
    #>   9  51 googlePolylines     0.8.2   David Cooley    1y Encoding Coordinates into 'Goog...
    #>  10  47 cld2                1.2.1   Jeroen Ooms     1y Google's Compact Language Detec...

``` r
more()
```

    #> - "google" ---------------------------------------------- 155 packages in 0.006 seconds -
    #>   #    package          version by                  @ title                              
    #>  11 46 bigrquery        1.4.0   Hadley Wickham     6M An Interface to Google's 'BigQue...
    #>  12 39 googleAnalyticsR 1.0.1   Mark Edmondson     4M Google Analytics API into R        
    #>  13 38 cld3             1.4.2   Jeroen Ooms        7M Google's Compact Language Detect...
    #>  14 37 plotKML          0.8.2   Tomislav Hengl     4M Visualization of Spatial and Spa...
    #>  15 33 bigQueryR        0.5.0   Mark Edmondson     2y Interface with Google BigQuery w...
    #>  16 32 ggmap            3.0.0   ORPHANED           3y Spatial Visualization with ggplot2 
    #>  17 31 V8               4.1.0   Jeroen Ooms        7d Embedded JavaScript and WebAssem...
    #>  18 30 googlesheets     0.3.0   Jennifer Bryan     4y Manage Google Spreadsheets from R  
    #>  19 30 googleway        2.7.6   David Cooley      20d Accesses Google Maps APIs to Ret...
    #>  20 27 tensorflow       2.8.0   Tomasz Kalinowski  4d R Interface to 'TensorFlow'

### Stemming

The search server uses the stems of the words in the indexed metadata,
and the search phrase. This means that “colour” and “colours” deliver
the exact same result. So do “coloring”, “colored”, etc. (Unless one is
happen to be an exact package name or match another non-stemmed field.)

``` r
ps("colour", size = 3)
```

    #> - "colour" ---------------------------------------------- 270 packages in 0.005 seconds -
    #>   #     package    version by              @ title                                       
    #>  1  100 crayon     1.4.2   Gábor Csárdi   4M Colored Terminal Output                     
    #>  2   62 colorspace 2.0.2   Achim Zeileis  8M A Toolbox for Manipulating and Assessing ...
    #>  3   59 viridis    0.6.2   Simon Garnier  4M Colorblind-Friendly Color Maps for R

``` r
ps("colours", size = 3)
```

    #> - "colours" --------------------------------------------- 268 packages in 0.006 seconds -
    #>   #     package    version by              @ title                                       
    #>  1  100 crayon     1.4.2   Gábor Csárdi   4M Colored Terminal Output                     
    #>  2   62 colorspace 2.0.2   Achim Zeileis  8M A Toolbox for Manipulating and Assessing ...
    #>  3   59 viridis    0.6.2   Simon Garnier  4M Colorblind-Friendly Color Maps for R

### Ranking

The most important feature of a search engine is the ranking of the
results. The best results should be listed first. pkgsearch uses
weighted scoring, where a match in the package title gets a higher score
than a match in the package description. It also uses the number of
reverse dependencies and the number of downloads to weight the scores:

``` r
ps("colour")[, c("score", "package", "revdeps", "downloads_last_month")]
```

    #> # A data frame: 10 × 4
    #>     score package      revdeps downloads_last_month
    #>     <dbl> <chr>          <int>                <int>
    #>  1 17254. crayon           346              1020155
    #>  2 10681. colorspace       179               621855
    #>  3 10096. viridis          160               425759
    #>  4  7653. pillar            66              1532564
    #>  5  6776. viridisLite       79               735427
    #>  6  6480. colourpicker      44                30185
    #>  7  4743. shape             34                65178
    #>  8  4327. RColorBrewer     569               615252
    #>  9  4109. colorRamps        19                 5761
    #> 10  3479. ggnewscale        18                 6126

### Preferring Phrases

The search engine prefers matching whole phrases over single words. E.g.
the search phrase “permutation test” will rank coin higher than
testthat, even though testthat is a much better result for the single
word “test”. (In fact, at the time of writing testthat is not even on
the first page of results.)

``` r
ps("permutation test")
```

    #> - "permutation test" ----------------------------------- 2236 packages in 0.009 seconds -
    #>   #     package        version by                      @ title                           
    #>   1 100 coin           1.4.2   Torsten Hothorn        4M Conditional Inference Procedu...
    #>   2  31 perm           1.0.0.2 Michael P. Fay         4M Exact or Asymptotic Permutati...
    #>   3  30 exactRankTests 0.8.34  Torsten Hothorn        4M Exact Distributions for Rank ...
    #>   4  29 flip           2.5.0   Livio Finos            3y Multivariate Permutation Tests  
    #>   5  22 jmuOutlier     2.2     Steven T. Garren       3y Permutation Tests for Nonpara...
    #>   6  19 wPerm          1.0.1   Neil A. Weiss          6y Permutation Tests               
    #>   7  16 cpt            1.0.2   Johann Gagnon-Bartsch  3y Classification Permutation Test 
    #>   8  16 GlobalDeviance 0.4     Frederike Fuhlbrueck   8y Global Deviance Permutation T...
    #>   9  16 permutes       2.3.2   Cesko C. Voeten        2M Permutation Tests for Time Se...
    #>  10  16 AUtests        0.99    Arjun Sondhi           1y Approximate Unconditional and...

If the whole phrase does not match, pkgsearch falls back to individual
matching words. For example, a match from either words is enough here,
to get on the first page of results:

``` r
ps("test http")
```

    #> - "test http" ------------------------------------------- 6366 packages in 0.01 seconds -
    #>   #     package   version by                  @ title                                    
    #>   1 100 httptest  4.1.0   Neal Richardson    5M A Test Environment for HTTP Requests     
    #>   2  77 covr      3.5.1   Jim Hester         1y Test Coverage for Packages               
    #>   3  35 webfakes  1.1.3   Gábor Csárdi      10M Fake Web Apps for HTTP Testing           
    #>   4  15 testthat  3.1.2   Hadley Wickham    24d Unit Testing for R                       
    #>   5  14 vcr       1.0.2   Scott Chamberlain  9M Record 'HTTP' Calls to Disk              
    #>   6  13 psych     2.1.9   William Revelle    5M Procedures for Psychological, Psychome...
    #>   7   9 webmockr  0.8.0   Scott Chamberlain  1y Stubbing and Setting Expectations on '...
    #>   8   8 httr      1.4.2   Hadley Wickham     2y Tools for Working with URLs and HTTP     
    #>   9   6 bnlearn   4.7     Marco Scutari      5M Bayesian Network Structure Learning, P...
    #>  10   5 rmarkdown 2.11    Yihui Xie          5M Dynamic Documents for R

### British vs American English

The search engine uses a dictionary to make sure that package metadata
and queries given in British and American English yield the same
results. E.g. note the spelling of colour/color in the results:

``` r
ps("colour")
```

    #> - "colour" ---------------------------------------------- 270 packages in 0.005 seconds -
    #>   #     package      version by                 @ title                                  
    #>   1 100 crayon       1.4.2   Gábor Csárdi      4M Colored Terminal Output                
    #>   2  62 colorspace   2.0.2   Achim Zeileis     8M A Toolbox for Manipulating and Asses...
    #>   3  59 viridis      0.6.2   Simon Garnier     4M Colorblind-Friendly Color Maps for R   
    #>   4  44 pillar       1.7.0   Kirill Müller    12d Coloured Formatting for Columns        
    #>   5  39 viridisLite  0.4.0   Simon Garnier    10M Colorblind-Friendly Color Maps (Lite...
    #>   6  38 colourpicker 1.1.1   Dean Attali       4M A Colour Picker Tool for Shiny and f...
    #>   7  27 shape        1.4.6   Karline Soetaert  9M Functions for Plotting Graphical Sha...
    #>   8  25 RColorBrewer 1.1.2   Erich Neuwirth    7y ColorBrewer Palettes                   
    #>   9  24 colorRamps   2.3     Tim Keitt         9y Builds color tables                    
    #>  10  20 ggnewscale   0.4.5   Elio Campitelli   1y Multiple Fill and Colour Scales in '...

``` r
ps("color")
```

    #> - "color" ----------------------------------------------- 269 packages in 0.009 seconds -
    #>   #     package      version by                 @ title                                  
    #>   1 100 crayon       1.4.2   Gábor Csárdi      4M Colored Terminal Output                
    #>   2  62 colorspace   2.0.2   Achim Zeileis     8M A Toolbox for Manipulating and Asses...
    #>   3  59 viridis      0.6.2   Simon Garnier     4M Colorblind-Friendly Color Maps for R   
    #>   4  44 pillar       1.7.0   Kirill Müller    12d Coloured Formatting for Columns        
    #>   5  39 viridisLite  0.4.0   Simon Garnier    10M Colorblind-Friendly Color Maps (Lite...
    #>   6  38 colourpicker 1.1.1   Dean Attali       4M A Colour Picker Tool for Shiny and f...
    #>   7  27 shape        1.4.6   Karline Soetaert  9M Functions for Plotting Graphical Sha...
    #>   8  25 RColorBrewer 1.1.2   Erich Neuwirth    7y ColorBrewer Palettes                   
    #>   9  24 colorRamps   2.3     Tim Keitt         9y Builds color tables                    
    #>  10  20 ggnewscale   0.4.5   Elio Campitelli   1y Multiple Fill and Colour Scales in '...

### Ascii Folding

Especially when searching for package maintainer names, it is convenient
to use the corresponding ASCII letters for non-ASCII characters in
search phrases. E.g. the following two queries yield the same results.
Note that case is also ignored.

``` r
ps("gabor", size = 5)
```

    #> - "gabor" ----------------------------------------------- 101 packages in 0.005 seconds -
    #>   #     package  version by              @ title                                         
    #>  1  100 crayon   1.4.2   Gábor Csárdi   4M Colored Terminal Output                       
    #>  2   84 cli      3.1.1   Gábor Csárdi  24d Helpers for Developing Command Line Interfaces
    #>  3   77 progress 1.2.2   Gábor Csárdi   3y Terminal Progress Bars                        
    #>  4   59 zoo      1.8.9   Achim Zeileis  1y S3 Infrastructure for Regular and Irregular...
    #>  5   59 fs       1.5.2   Gábor Csárdi   2M Cross-Platform File System Operations Based...

``` r
ps("Gábor", size = 5)
```

    #> - "Gábor" ----------------------------------------------- 101 packages in 0.006 seconds -
    #>   #     package  version by              @ title                                         
    #>  1  100 crayon   1.4.2   Gábor Csárdi   4M Colored Terminal Output                       
    #>  2   84 cli      3.1.1   Gábor Csárdi  24d Helpers for Developing Command Line Interfaces
    #>  3   77 progress 1.2.2   Gábor Csárdi   3y Terminal Progress Bars                        
    #>  4   59 zoo      1.8.9   Achim Zeileis  1y S3 Infrastructure for Regular and Irregular...
    #>  5   59 fs       1.5.2   Gábor Csárdi   2M Cross-Platform File System Operations Based...

## More info

See the [complete documentation](https://r-hub.github.io/pkgsearch/).

## License

MIT @ [Gábor Csárdi](https://github.com/gaborcsardi),
[RStudio](https://github.com/rstudio), [R
Consortium](https://www.r-consortium.org/).
