# Search and Query CRAN R Packages

<!-- badges: start -->
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable-1)
[![R-CMD-check](https://github.com/r-hub/pkgsearch/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-hub/pkgsearch/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/pkgsearch)](https://cran.r-project.org/package=pkgsearch)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/pkgsearch)](https://www.r-pkg.org/pkg/pkgsearch)
[![Codecov test coverage](https://codecov.io/gh/r-hub/pkgsearch/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-hub/pkgsearch?branch=main)
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

The development version is on GitHub:

``` r
pak::pak("r-hub/pkgsearch")
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

    #> - "permutation test" ------------------------------------ 2441 packages in 0.02 seconds -
    #>   #     package        version by                      @ title                           
    #>   1 100 coin           1.4.3   Torsten Hothorn        2M Conditional Inference Procedu...
    #>   2  50 perm           1.0.0.4 Michael P. Fay         3M Exact or Asymptotic Permutati...
    #>   3  48 exactRankTests 0.8.35  Torsten Hothorn        2y Exact Distributions for Rank ...
    #>   4  35 jmuOutlier     2.2     Steven T. Garren       4y Permutation Tests for Nonpara...
    #>   5  35 nptest         1.1     Nathaniel E. Helwig    7M Nonparametric Bootstrap and P...
    #>   6  33 lmPerm         2.1.0   Marco Torchiano        7y Permutation Tests for Linear ...
    #>   7  29 wPerm          1.0.1   Neil A. Weiss          8y Permutation Tests               
    #>   8  27 flip           2.5.0   Livio Finos            5y Multivariate Permutation Tests  
    #>   9  25 cpt            1.0.2   Johann Gagnon-Bartsch  5y Classification Permutation Test 
    #>  10  25 AUtests        0.99    Arjun Sondhi           3y Approximate Unconditional and...

pkgsearch uses an [R-hub](https://docs.r-hub.io) web service and a
careful ranking that puts popular packages before less frequently used
ones.

### Do it all *clicking*

For the search mentioned above, and other points of entry to CRAN
metadata, you can use pkgsearch RStudio add-in!

[![Addin
screencast](https://raw.githubusercontent.com/r-hub/pkgsearch/main/gifs/addin.gif)](https://vimeo.com/375618736)

Select the “CRAN package search” addin from the menu, or start it with
`pkg_search_addin()`.

### Get package metadata

Do you want to find the dependencies the first versions of `testthat`
had and when each of these versions was released?

``` r
cran_package_history("testthat")
```

    #> # A data frame: 44 × 29
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
    #> # ℹ 34 more rows
    #> # ℹ 19 more variables: Collate <chr>, Packaged <chr>, Repository <chr>,
    #> #   `Date/Publication` <chr>, crandb_file_date <chr>, date <chr>, dependencies <list>,
    #> #   NeedsCompilation <chr>, Roxygen <chr>, `Authors@R` <chr>, BugReports <chr>,
    #> #   RoxygenNote <chr>, VignetteBuilder <chr>, Encoding <chr>, MD5sum <chr>,
    #> #   `Config/testthat/edition` <chr>, `Config/testthat/parallel` <chr>,
    #> #   `Config/testthat/start-first` <chr>, `Config/Needs/website` <chr>

### Discover packages

Do you want to know what packages are trending on CRAN these days?
`pkgsearch` can help!

``` r
cran_trending()
```

    #> # A data frame: 100 × 2
    #>    package      score                
    #>    <chr>        <chr>                
    #>  1 pcalg        1203.6334096447825100
    #>  2 nlraa        970.2647657841140500 
    #>  3 sur          843.7564499484004100 
    #>  4 dynr         501.1116481391976800 
    #>  5 imbalance    448.0396203054065200 
    #>  6 DoE.base     423.4324791958474100 
    #>  7 precrec      386.7902114516512200 
    #>  8 Sejong       334.8732815860843500 
    #>  9 apcluster    325.1561461794019900 
    #> 10 multilinguer 313.1972588608061300 
    #> # ℹ 90 more rows

``` r
cran_top_downloaded()
```

    #> # A data frame: 100 × 2
    #>    package     count 
    #>    <chr>       <chr> 
    #>  1 ragg        840026
    #>  2 textshaping 826270
    #>  3 ggplot2     698736
    #>  4 devtools    534494
    #>  5 pkgdown     514648
    #>  6 rlang       500676
    #>  7 rgl         489969
    #>  8 sf          438924
    #>  9 dplyr       416520
    #> 10 lifecycle   389821
    #> # ℹ 90 more rows

### Keep up with CRAN

Are you curious about the latest releases or archivals?

``` r
cran_events()
```

    #> CRAN events (events)---------------------------------------------------------------------
    #>  . When     Package      Version Title                                               
    #>  + 3 hours  TestAnaAPP   0.1.4   The 'shiny' App for Test Analysis and Visualization 
    #>  + 3 hours  NMcalc       0.0.2   Basic Calculations for PK/PD Modeling               
    #>  + 7 hours  CASMI        1.2.0   'CASMI'-Based Functions                             
    #>  + 8 hours  cobalt       4.5.2   Covariate Balance Tables and Plots                  
    #>  + 9 hours  raqs         1.0.2   Interface to the US EPA Air Quality System (AQS) API
    #>  + 9 hours  str2str      1.0.0   Convert R Objects from One Structure to Another     
    #>  + 9 hours  hemispheR    1.1.0   Processing Hemispherical Canopy Images              
    #>  + 10 hours tidyfit      0.6.5   Regularized Linear Modeling with Tidy Data          
    #>  + 11 hours FlexVarJM    0.1.0   Estimate Joint Models with Subject-Specific Variance
    #>  + 11 hours ggScatRidges 0.1.0   Scatter Plot Combined with Ridgelines in 'ggplot2'

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

    #> - "C++" ----------------------------------------------- 15310 packages in 0.016 seconds -
    #>   #     package      version  by                    @ title                              
    #>   1 100 Rcpp         1.0.11   Dirk Eddelbuettel    5M Seamless R and C++ Integration     
    #>   2  35 BH           1.81.0.1 Dirk Eddelbuettel   10M Boost C++ Header Files             
    #>   3  18 cpp11        0.4.6    Davis Vaughan        3M A C++11 Interface for R's C Inte...
    #>   4  13 RcppProgress 0.4.2    Karl Forner          4y An Interruptible Progress Bar wi...
    #>   5  10 inline       0.3.19   Dirk Eddelbuettel    2y Functions to Inline C, C++, Fort...
    #>   6   9 SnowballC    0.7.1    Milan Bouchet-Valat  7M Snowball Stemmers Based on the C...
    #>   7   7 RNifti       1.5.0    Jon Clayden          6M Fast R and C++ Access to NIfTI I...
    #>   8   6 xml2         1.3.5    Hadley Wickham       5M Parse XML                          
    #>   9   6 LiblineaR    2.10.22  Thibault Helleputte  1y Linear Predictive Models Based o...
    #>  10   6 readxl       1.4.3    Jennifer Bryan       5M Read Excel Files

``` r
pkg_search()
```

    #> - "C++" ----------------------------------------------- 15310 packages in 0.016 seconds -
    #> 
    #> 1 Rcpp @ 1.0.11                                           Dirk Eddelbuettel, 5 months ago
    #> ---------------
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
    #>   https://www.rcpp.org
    #>   https://dirk.eddelbuettel.com/code/rcpp.html
    #>   https://github.com/RcppCore/Rcpp
    #> 
    #> 2 BH @ 1.81.0.1                                          Dirk Eddelbuettel, 10 months ago
    #> ---------------
    #>   # Boost C++ Header Files
    #>   Boost provides free peer-reviewed portable C++ source libraries.  A large part
    #>   of Boost is provided as C++ template code which is resolved entirely at
    #>   compile-time without linking.  This package aims to provide the most useful
    #>   subset of Boost libraries for template use among CRAN packages. By placing
    #>   these libraries in this package, we offer a more efficient distribution system
    #>   for CRAN as replication of this code in the sources of other packages is
    #>   avoided. As of release 1.81.0-0, the following Boost libraries are included:
    #>   'accumulators' 'algorithm' 'align' 'any' 'atomic' 'beast' 'bimap' 'bind'
    #>   'circular_buffer' 'compute' 'concept' 'config' 'container' 'date_time'
    #>   'detail' 'dynamic_bitset' 'exception' 'flyweight' 'foreach' 'functional'
    #>   'fusion' 'geometry' 'graph' 'heap' 'icl' 'integer' 'interprocess' 'intrusive'
    #>   'io' 'iostreams' 'iterator' 'lambda2' 'math' 'move' 'mp11' 'mpl'
    #>   'multiprecision' 'numeric' 'pending' 'phoenix' 'polygon' 'preprocessor'
    #>   'process' 'propery_tree' 'random' 'range' 'scope_exit' 'smart_ptr' 'sort'
    #>   'spirit' 'tuple' 'type_traits' 'typeof' 'unordered' 'url' 'utility' 'uuid'.
    #>   https://github.com/eddelbuettel/bh
    #>   https://dirk.eddelbuettel.com/code/bh.html
    #> 
    #> 3 cpp11 @ 0.4.6                                               Davis Vaughan, 3 months ago
    #> ---------------
    #>   # A C++11 Interface for R's C Interface
    #>   Provides a header only, C++11 interface to R's C interface.  Compared to other
    #>   approaches 'cpp11' strives to be safe against long jumps from the C API as
    #>   well as C++ exceptions, conform to normal R function semantics and supports
    #>   interaction with 'ALTREP' vectors.
    #>   https://cpp11.r-lib.org
    #>   https://github.com/r-lib/cpp11
    #> 
    #> 4 RcppProgress @ 0.4.2                                           Karl Forner, 4 years ago
    #> ----------------------
    #>   # An Interruptible Progress Bar with OpenMP Support for C++ in R Packages
    #>   Allows to display a progress bar in the R console for long running
    #>   computations taking place in c++ code, and support for interrupting those
    #>   computations even in multithreaded code, typically using OpenMP.
    #>   https://github.com/kforner/rcpp_progress
    #> 
    #> 5 inline @ 0.3.19                                          Dirk Eddelbuettel, 2 years ago
    #> -----------------
    #>   # Functions to Inline C, C++, Fortran Function Calls from R
    #>   Functionality to dynamically define R functions and S4 methods with 'inlined'
    #>   C, C++ or Fortran code supporting the .C and .Call calling conventions.
    #>   https://github.com/eddelbuettel/inline
    #>   https://dirk.eddelbuettel.com/code/inline.html
    #> 
    #> 6 SnowballC @ 0.7.1                                     Milan Bouchet-Valat, 7 months ago
    #> -------------------
    #>   # Snowball Stemmers Based on the C 'libstemmer' UTF-8 Library
    #>   An R interface to the C 'libstemmer' library that implements Porter's word
    #>   stemming algorithm for collapsing words to a common root to aid comparison of
    #>   vocabulary. Currently supported languages are Arabic, Basque, Catalan, Danish,
    #>   Dutch, English, Finnish, French, German, Greek, Hindi, Hungarian, Indonesian,
    #>   Irish, Italian, Lithuanian, Nepali, Norwegian, Portuguese, Romanian, Russian,
    #>   Spanish, Swedish, Tamil and Turkish.
    #>   https://github.com/nalimilan/R.TeMiS
    #> 
    #> 7 RNifti @ 1.5.0                                                Jon Clayden, 6 months ago
    #> ----------------
    #>   # Fast R and C++ Access to NIfTI Images
    #>   Provides very fast read and write access to images stored in the NIfTI-1,
    #>   NIfTI-2 and ANALYZE-7.5 formats, with seamless synchronisation of in-memory
    #>   image objects between compiled C and interpreted R code. Also provides a
    #>   simple image viewer, and a C/C++ API that can be used by other packages. Not
    #>   to be confused with 'RNiftyReg', which performs image registration and applies
    #>   spatial transformations.
    #>   https://github.com/jonclayden/RNifti
    #> 
    #> 8 xml2 @ 1.3.5                                               Hadley Wickham, 5 months ago
    #> --------------
    #>   # Parse XML
    #>   Work with XML files using a simple, consistent interface. Built on top of the
    #>   'libxml2' C library.
    #>   https://xml2.r-lib.org/
    #>   https://github.com/r-lib/xml2
    #> 
    #> 9 LiblineaR @ 2.10.22                               Thibault Helleputte, about a year ago
    #> ---------------------
    #>   # Linear Predictive Models Based on the LIBLINEAR C/C++ Library
    #>   A wrapper around the LIBLINEAR C/C++ library for machine learning (available
    #>   at <https://www.csie.ntu.edu.tw/~cjlin/liblinear/>). LIBLINEAR is a simple
    #>   library for solving large-scale regularized linear classification and
    #>   regression. It currently supports L2-regularized classification (such as
    #>   logistic regression, L2-loss linear SVM and L1-loss linear SVM) as well as
    #>   L1-regularized classification (such as L2-loss linear SVM and logistic
    #>   regression) and L2-regularized support vector regression (with L1- or
    #>   L2-loss). The main features of LiblineaR include multi-class classification
    #>   (one-vs-the rest, and Crammer & Singer method), cross validation for model
    #>   selection, probability estimates (logistic regression only) or weights for
    #>   unbalanced data. The estimation of the models is particularly fast as compared
    #>   to other libraries.
    #>   <https://dnalytics.com/software/liblinear/>
    #> 
    #> 10 readxl @ 1.4.3                                            Jennifer Bryan, 5 months ago
    #> -----------------
    #>   # Read Excel Files
    #>   Import excel files into R. Supports '.xls' via the embedded 'libxls' C library
    #>   <https://github.com/libxls/libxls> and '.xlsx' via the embedded 'RapidXML' C++
    #>   library <https://rapidxml.sourceforge.net/>. Works on Windows, Mac and Linux
    #>   without external dependencies.
    #>   https://readxl.tidyverse.org
    #>   https://github.com/tidyverse/readxl

### Pagination

The `more()` function can be used to display the next batch of search
hits, batches contain ten packages by default. `ps()` is a shorter alias
to `pkg_search()`:

``` r
ps("google")
```

    #> - "google" ----------------------------------------------- 168 packages in 0.01 seconds -
    #>   #     package             version by               @ title                             
    #>   1 100 googledrive         2.1.1   Jennifer Bryan  5M An Interface to Google Drive      
    #>   2  84 googleVis           0.7.1   Markus Gesmann  9M R Interface to Google Charts      
    #>   3  81 gargle              1.5.2   Jennifer Bryan  4M Utilities for Working with Goog...
    #>   4  79 googleAuthR         2.0.1   Mark Edmondson  7M Authenticate and Create Google ...
    #>   5  64 googlesheets4       1.1.1   Jennifer Bryan  5M Access Google Sheets using the ...
    #>   6  60 googleCloudStorageR 0.7.0   Mark Edmondson  2y Interface with Google Cloud Sto...
    #>   7  55 bigrquery           1.4.2   Hadley Wickham  7M An Interface to Google's 'BigQu...
    #>   8  51 gsheet              0.4.5   Max Conway      4y Download Google Sheets Using Ju...
    #>   9  47 cld2                1.2.4   Jeroen Ooms     1y Google's Compact Language Detec...
    #>  10  34 cld3                1.6.0   Jeroen Ooms     2M Google's Compact Language Detec...

``` r
more()
```

    #> - "google" ---------------------------------------------- 168 packages in 0.011 seconds -
    #>   #    package          version by                    @ title                            
    #>  11 33 gtrendsR         1.5.1   Philippe Massicotte  1y Perform and Display Google Tre...
    #>  12 32 tensorflow       2.14.0  Tomasz Kalinowski    2M R Interface to 'TensorFlow'      
    #>  13 29 gfonts           0.2.0   Victor Perrier      11M Offline 'Google' Fonts for 'Ma...
    #>  14 29 googleAnalyticsR 1.1.0   Mark Edmondson       1y Google Analytics API into R      
    #>  15 29 bigQueryR        0.5.0   Mark Edmondson       4y Interface with Google BigQuery...
    #>  16 27 re2              0.1.2   Girish Palya         2y R Interface to Google RE2 (C++...
    #>  17 27 googleway        2.7.8   David Cooley         3M Accesses Google Maps APIs to R...
    #>  18 27 scholar          0.2.4   Guangchuang Yu       1y Analyse Citation Data from Goo...
    #>  19 25 googletraffic    0.1.4   Robert Marty         5M Google Traffic                   
    #>  20 25 rgoogleclassroom 0.9.1   Candace Savonen      3M API Wrapper for Google Classro...

### Stemming

The search server uses the stems of the words in the indexed metadata,
and the search phrase. This means that “colour” and “colours” deliver
the exact same result. So do “coloring”, “colored”, etc. (Unless one is
happen to be an exact package name or match another non-stemmed field.)

``` r
ps("colour", size = 3)
```

    #> - "colour" ---------------------------------------------- 318 packages in 0.021 seconds -
    #>   #     package    version by              @ title                                       
    #>  1  100 crayon     1.5.2   Gábor Csárdi   1y Colored Terminal Output                     
    #>  2   62 viridis    0.6.4   Simon Garnier  4M Colorblind-Friendly Color Maps for R        
    #>  3   59 colorspace 2.1.0   Achim Zeileis 10M A Toolbox for Manipulating and Assessing ...

``` r
ps("colours", size = 3)
```

    #> - "colours" ---------------------------------------------- 316 packages in 0.01 seconds -
    #>   #     package    version by              @ title                                       
    #>  1  100 crayon     1.5.2   Gábor Csárdi   1y Colored Terminal Output                     
    #>  2   62 viridis    0.6.4   Simon Garnier  4M Colorblind-Friendly Color Maps for R        
    #>  3   59 colorspace 2.1.0   Achim Zeileis 10M A Toolbox for Manipulating and Assessing ...

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
    #>  1 18223. crayon           375               715661
    #>  2 11222. viridis          200               288752
    #>  3 10806. colorspace       191               742038
    #>  4  9907. pillar           112              1208927
    #>  5  7338. viridisLite       95               750543
    #>  6  6722. colourpicker      47                35221
    #>  7  4652. ggnewscale        33                21712
    #>  8  4629. RColorBrewer     598               683668
    #>  9  4471. shape             31               177749
    #> 10  4229. colorRamps        21                 4917

### Preferring Phrases

The search engine prefers matching whole phrases over single words. E.g.
the search phrase “permutation test” will rank coin higher than
testthat, even though testthat is a much better result for the single
word “test”. (In fact, at the time of writing testthat is not even on
the first page of results.)

``` r
ps("permutation test")
```

    #> - "permutation test" ------------------------------------ 2441 packages in 0.02 seconds -
    #>   #     package        version by                      @ title                           
    #>   1 100 coin           1.4.3   Torsten Hothorn        2M Conditional Inference Procedu...
    #>   2  50 perm           1.0.0.4 Michael P. Fay         3M Exact or Asymptotic Permutati...
    #>   3  48 exactRankTests 0.8.35  Torsten Hothorn        2y Exact Distributions for Rank ...
    #>   4  35 jmuOutlier     2.2     Steven T. Garren       4y Permutation Tests for Nonpara...
    #>   5  35 nptest         1.1     Nathaniel E. Helwig    7M Nonparametric Bootstrap and P...
    #>   6  33 lmPerm         2.1.0   Marco Torchiano        7y Permutation Tests for Linear ...
    #>   7  29 wPerm          1.0.1   Neil A. Weiss          8y Permutation Tests               
    #>   8  27 flip           2.5.0   Livio Finos            5y Multivariate Permutation Tests  
    #>   9  25 cpt            1.0.2   Johann Gagnon-Bartsch  5y Classification Permutation Test 
    #>  10  25 AUtests        0.99    Arjun Sondhi           3y Approximate Unconditional and...

If the whole phrase does not match, pkgsearch falls back to individual
matching words. For example, a match from either words is enough here,
to get on the first page of results:

``` r
ps("test http")
```

    #> - "test http" ------------------------------------------ 6786 packages in 0.022 seconds -
    #>   #     package      version by                  @ title                                 
    #>   1 100 httptest     4.2.1   Neal Richardson    5M A Test Environment for HTTP Requests  
    #>   2  13 webfakes     1.2.1   Gábor Csárdi       2M Fake Web Apps for HTTP Testing        
    #>   3  13 vcr          1.2.2   Scott Chamberlain  5M Record 'HTTP' Calls to Disk           
    #>   4  13 psych        2.3.9   William Revelle    2M Procedures for Psychological, Psych...
    #>   5   8 httr         1.4.7   Hadley Wickham     3M Tools for Working with URLs and HTTP  
    #>   6   8 webmockr     0.9.0   Scott Chamberlain  9M Stubbing and Setting Expectations o...
    #>   7   5 bnlearn      4.9     Marco Scutari      2M Bayesian Network Structure Learning...
    #>   8   5 clubSandwich 0.5.10  James Pustejovsky  4M Cluster-Robust (Sandwich) Variance ...
    #>   9   4 oompaBase    3.2.9   Kevin R. Coombes   4y Class Unions, Matrix Operations, an...
    #>  10   4 testthat     3.2.0   Hadley Wickham     2M Unit Testing for R

### British vs American English

The search engine uses a dictionary to make sure that package metadata
and queries given in British and American English yield the same
results. E.g. note the spelling of colour/color in the results:

``` r
ps("colour")
```

    #> - "colour" ---------------------------------------------- 318 packages in 0.009 seconds -
    #>   #     package      version by                 @ title                                  
    #>   1 100 crayon       1.5.2   Gábor Csárdi      1y Colored Terminal Output                
    #>   2  62 viridis      0.6.4   Simon Garnier     4M Colorblind-Friendly Color Maps for R   
    #>   3  59 colorspace   2.1.0   Achim Zeileis    10M A Toolbox for Manipulating and Asses...
    #>   4  54 pillar       1.9.0   Kirill Müller     8M Coloured Formatting for Columns        
    #>   5  40 viridisLite  0.4.2   Simon Garnier     7M Colorblind-Friendly Color Maps (Lite...
    #>   6  37 colourpicker 1.3.0   Dean Attali       3M A Colour Picker Tool for Shiny and f...
    #>   7  26 ggnewscale   0.4.9   Elio Campitelli   6M Multiple Fill and Colour Scales in '...
    #>   8  25 RColorBrewer 1.1.3   Erich Neuwirth    2y ColorBrewer Palettes                   
    #>   9  25 shape        1.4.6   Karline Soetaert  3y Functions for Plotting Graphical Sha...
    #>  10  23 colorRamps   2.3.1   Tim Keitt         2y Builds Color Tables

``` r
ps("color")
```

    #> - "color" ----------------------------------------------- 317 packages in 0.011 seconds -
    #>   #     package      version by                 @ title                                  
    #>   1 100 crayon       1.5.2   Gábor Csárdi      1y Colored Terminal Output                
    #>   2  62 viridis      0.6.4   Simon Garnier     4M Colorblind-Friendly Color Maps for R   
    #>   3  59 colorspace   2.1.0   Achim Zeileis    10M A Toolbox for Manipulating and Asses...
    #>   4  54 pillar       1.9.0   Kirill Müller     8M Coloured Formatting for Columns        
    #>   5  40 viridisLite  0.4.2   Simon Garnier     7M Colorblind-Friendly Color Maps (Lite...
    #>   6  37 colourpicker 1.3.0   Dean Attali       3M A Colour Picker Tool for Shiny and f...
    #>   7  26 ggnewscale   0.4.9   Elio Campitelli   6M Multiple Fill and Colour Scales in '...
    #>   8  25 RColorBrewer 1.1.3   Erich Neuwirth    2y ColorBrewer Palettes                   
    #>   9  25 shape        1.4.6   Karline Soetaert  3y Functions for Plotting Graphical Sha...
    #>  10  23 colorRamps   2.3.1   Tim Keitt         2y Builds Color Tables

### Ascii Folding

Especially when searching for package maintainer names, it is convenient
to use the corresponding ASCII letters for non-ASCII characters in
search phrases. E.g. the following two queries yield the same results.
Note that case is also ignored.

``` r
ps("gabor", size = 5)
```

    #> - "gabor" ----------------------------------------------- 105 packages in 0.009 seconds -
    #>   #     package  version by              @ title                                         
    #>  1  100 cli      3.6.1   Gábor Csárdi   8M Helpers for Developing Command Line Interfaces
    #>  2   83 crayon   1.5.2   Gábor Csárdi   1y Colored Terminal Output                       
    #>  3   66 progress 1.2.2   Gábor Csárdi   5y Terminal Progress Bars                        
    #>  4   61 fs       1.6.3   Gábor Csárdi   4M Cross-Platform File System Operations Based...
    #>  5   54 zoo      1.8.12  Achim Zeileis  7M S3 Infrastructure for Regular and Irregular...

``` r
ps("Gábor", size = 5)
```

    #> - "Gábor" ----------------------------------------------- 105 packages in 0.009 seconds -
    #>   #     package  version by              @ title                                         
    #>  1  100 cli      3.6.1   Gábor Csárdi   8M Helpers for Developing Command Line Interfaces
    #>  2   83 crayon   1.5.2   Gábor Csárdi   1y Colored Terminal Output                       
    #>  3   66 progress 1.2.2   Gábor Csárdi   5y Terminal Progress Bars                        
    #>  4   61 fs       1.6.3   Gábor Csárdi   4M Cross-Platform File System Operations Based...
    #>  5   54 zoo      1.8.12  Achim Zeileis  7M S3 Infrastructure for Regular and Irregular...

## More info

See the [complete documentation](https://r-hub.github.io/pkgsearch/).

## License

MIT @ [Gábor Csárdi](https://github.com/gaborcsardi),
[RStudio](https://github.com/rstudio), [R
Consortium](https://www.r-consortium.org/).
