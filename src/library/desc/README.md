
# desc

> Parse DESCRIPTION files

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/r-lib/desc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/desc/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/r-lib/desc/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/desc?branch=main)
[![](https://www.r-pkg.org/badges/version/desc)](https://www.r-pkg.org/pkg/desc)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/desc)](https://www.r-pkg.org/pkg/desc)
<!-- badges: end -->

Parse, manipulate and reformat DESCRIPTION files. The package provides
two APIs, one is object oriented, the other one is procedural and
manipulates the files *in place*.

------------------------------------------------------------------------

-   [Installation](#installation)
-   [The object oriented API](#the-oo-api)
    -   [Introduction](#introduction)
    -   [Loading or creating new `DESCRIPTION`
        files](#loading-or-creating-new-description-files)
    -   [Normalizing `DESCRIPTION`
        files](#normalizing-description-files)
    -   [Querying, changing and removing
        fields](#querying-changing-and-removing-fields)
    -   [Dependencies](#dependencies)
    -   [Collate fields](#collate-fields)
    -   [Authors](#authors)
-   [The procedural API](#the-procedural-api)
-   [License](#license)

## Installation

``` r
install.packages("desc")
```

## The object oriented API

``` r
library(desc)
```

### Introduction

The object oriented API uses [R6](https://github.com/r-lib/R6) classes.

### Loading or creating new `DESCRIPTION` files

A new `description` object can be created by reading a `DESCRIPTION`
file form the disk. By default the `DESCRIPTION` file in the current
directory is read:

``` r
desc <- description$new()
```

``` r
desc
```

    #> Package: desc
    #> Title: Manipulate DESCRIPTION Files
    #> Version: 1.0.0
    #> Author: Gábor Csárdi
    #> Maintainer: Gábor Csárdi <csardi.gabor@gmail.com>
    #> Description: Tools to read, write, create, and manipulate DESCRIPTION
    #>     files.  It is intented for packages that create or manipulate other
    #>     packages.
    #> License: MIT + file LICENSE
    #> URL: https://github.com/r-lib/desc
    #> BugReports: https://github.com/r-lib/desc/issues
    #> Imports:
    #>     R6
    #> Suggests:
    #>     newpackage,
    #>     testthat,
    #>     whoami
    #> Encoding: UTF-8
    #> LazyData: true
    #> RoxygenNote: 5.0.0

A new object can also be created from scratch:

``` r
desc2 <- description$new("!new")
desc2
```

    #> Package: {{ Package }}
    #> Title: {{ Title }}
    #> Version: 1.0.0
    #> Authors@R (parsed):
    #>     * Jo Doe <jodoe@dom.ain> [aut, cre]
    #> Maintainer: {{ Maintainer }}
    #> Description: {{ Description }}
    #> License: {{ License }}
    #> URL: {{ URL }}
    #> BugReports: {{ BugReports }}
    #> Encoding: UTF-8

### Normalizing `DESCRIPTION` files

Most `DESCRIPTION` fields may be formatted in multiple equivalent ways.
`desc` does not reformat fields, unless they are updated or reformatting
is explicitly requested via a call to the `normalize()` method or using
the `normalize` argument of the `write()` method.

### Querying, changing and removing fields

`get()` and `set()` queries or updates a field:

``` r
desc$set("Package", "foo")
desc$get("Package")
```

    #> Package 
    #>   "foo"

They work with multiple fields as well:

``` r
desc$set(Package = "bar", Title = "Bar Package")
desc$get(c("Package", "Title"))
```

    #>       Package         Title 
    #>         "bar" "Bar Package"

### Dependencies

Package dependencies can be set and updated via an easier API:

``` r
desc$get_deps()
```

    #>       type    package version
    #> 1 Suggests   testthat       *
    #> 2 Suggests     whoami       *
    #> 3 Suggests newpackage       *
    #> 4  Imports         R6       *

``` r
desc$set_dep("mvtnorm")
desc$set_dep("Rcpp", "LinkingTo")
desc$get_deps()
```

    #>        type    package version
    #> 1  Suggests   testthat       *
    #> 2  Suggests     whoami       *
    #> 3  Suggests newpackage       *
    #> 4   Imports    mvtnorm       *
    #> 5   Imports         R6       *
    #> 6 LinkingTo       Rcpp       *

``` r
desc
```

    #> Package: bar
    #> Title: Bar Package
    #> Version: 1.0.0
    #> Author: Gábor Csárdi
    #> Maintainer: Gábor Csárdi <csardi.gabor@gmail.com>
    #> Description: Tools to read, write, create, and manipulate DESCRIPTION
    #>     files.  It is intented for packages that create or manipulate other
    #>     packages.
    #> License: MIT + file LICENSE
    #> URL: https://github.com/r-lib/desc
    #> BugReports: https://github.com/r-lib/desc/issues
    #> Imports:
    #>     mvtnorm,
    #>     R6
    #> Suggests:
    #>     newpackage,
    #>     testthat,
    #>     whoami
    #> LinkingTo:
    #>     Rcpp
    #> Encoding: UTF-8
    #> LazyData: true
    #> RoxygenNote: 5.0.0

### Collate fields

Collate fields can be queried and set using simple character vectors of
file names:

``` r
desc$set_collate(list.files("../R"))
```

    #> Warning in idesc_set_collate(self, private, files, match.arg(which)): No files
    #> in 'Collate' field

``` r
desc$get_collate()
```

    #> character(0)

### Authors

Authors information, when specified via the `Authors@R` field, also has
a simplified API:

``` r
desc <- description$new("tools/pkg2")
desc$get_authors()
```

    #> [1] "Hadley Wickham <h.wickham@gmail.com> [aut, cre, cph]"
    #> [2] "Peter Danenberg <pcd@roxygen.org> [aut, cph]"        
    #> [3] "Manuel Eugster [aut, cph]"                           
    #> [4] "RStudio [cph]"

``` r
desc$add_author("Bugs", "Bunny", email = "bb@acme.com")
desc$add_me()
desc$add_author_gh("jeroen")
desc$get_authors()
```

    #> [1] "Hadley Wickham <h.wickham@gmail.com> [aut, cre, cph]"
    #> [2] "Peter Danenberg <pcd@roxygen.org> [aut, cph]"        
    #> [3] "Manuel Eugster [aut, cph]"                           
    #> [4] "RStudio [cph]"                                       
    #> [5] "Bugs Bunny <bb@acme.com>"                            
    #> [6] "First Last <first.last@dom.com> [ctb]"               
    #> [7] "Jeroen Ooms <jeroen@berkeley.edu> [ctb]"

If the `Author` field is specified, it can be changed to a `Authors@R`
field using `coerce_authors_at_r()`, incorporating the `Maintainer`
information if necessary:

``` r
desc <- description$new("!new")
desc$del("Authors@R")
desc$del("Maintainer")
desc$set(Author = "Gábor Csárdi <csardi.gabor@gmail.com>")
desc$get_authors()
```

    #> Error in ensure_authors_at_r(self): No 'Authors@R' field!
    #> You can create one with $add_author.
    #> You can also use $coerce_authors_at_r() to change Author fields

``` r
desc$coerce_authors_at_r()
desc$get_authors()
```

    #> [1] "Gábor Csárdi <csardi.gabor@gmail.com> [aut]"

## The procedural API

The procedural API is simpler to use for one-off `DESCRIPTION`
manipulation, since it does not require dealing with `description`
objects. Each object oriented method has a procedural counterpart that
works on a file, and potentially writes its result back to the same
file.

For example, adding a new dependency to `DESCRIPTION` in the current
working directory can be done with

``` r
desc_set_dep("newpackage", "Suggests")
```

    #> Package: desc
    #> Title: Manipulate DESCRIPTION Files
    #> Version: 1.4.1.9000
    #> Authors@R (parsed):
    #>     * Gábor Csárdi <csardi.gabor@gmail.com> [aut, cre]
    #>     * Kirill Müller [aut]
    #>     * Jim Hester <james.f.hester@gmail.com> [aut]
    #>     * Maëlle Salmon [ctb] (<https://orcid.org/0000-0002-2815-0399>)
    #>     * RStudio [cph, fnd]
    #> Maintainer: Gábor Csárdi <csardi.gabor@gmail.com>
    #> Description: Tools to read, write, create, and manipulate DESCRIPTION
    #>     files.  It is intended for packages that create or manipulate other
    #>     packages.
    #> License: MIT + file LICENSE
    #> URL: https://github.com/r-lib/desc#readme, https://r-lib.github.io/desc/
    #> BugReports: https://github.com/r-lib/desc/issues
    #> Depends:
    #>     R (>= 3.4)
    #> Imports:
    #>     cli,
    #>     R6,
    #>     rprojroot,
    #>     utils
    #> Suggests:
    #>     callr,
    #>     covr,
    #>     gh,
    #>     newpackage,
    #>     spelling,
    #>     testthat,
    #>     whoami,
    #>     withr
    #> Config/Needs/website: tidyverse/tidytemplate
    #> Config/testthat/edition: 3
    #> Encoding: UTF-8
    #> Language: en-US
    #> Roxygen: list(r6 = FALSE, load = "installed", markdown = TRUE)
    #> RoxygenNote: 7.2.1.9000
    #> Collate:
    #>     'assertions.R'
    #>     'authors-at-r.R'
    #>     'built.R'
    #>     'classes.R'
    #>     'collate.R'
    #>     'constants.R'
    #>     'deps.R'
    #>     'desc-package.R'
    #>     'description.R'
    #>     'encoding.R'
    #>     'latex.R'
    #>     'non-oo-api.R'
    #>     'package-archives.R'
    #>     'read.R'
    #>     'remotes.R'
    #>     'str.R'
    #>     'syntax_checks.R'
    #>     'urls.R'
    #>     'utils.R'
    #>     'validate.R'
    #>     'version.R'

This added `newpackage` to the `Suggests` field:

``` r
desc_get("Suggests")
```

    #>                                                                                                  Suggests 
    #> "\n    callr,\n    covr,\n    gh,\n    newpackage,\n    spelling,\n    testthat,\n    whoami,\n    withr"

So the full list of dependencies are now

``` r
desc_get_deps()
```

    #>        type    package version
    #> 1   Depends          R  >= 3.4
    #> 2   Imports        cli       *
    #> 3   Imports         R6       *
    #> 4   Imports  rprojroot       *
    #> 5   Imports      utils       *
    #> 6  Suggests      callr       *
    #> 7  Suggests       covr       *
    #> 8  Suggests         gh       *
    #> 9  Suggests newpackage       *
    #> 10 Suggests   spelling       *
    #> 11 Suggests   testthat       *
    #> 12 Suggests     whoami       *
    #> 13 Suggests      withr       *

## Code of Conduct

Please note that the desc project is released with a [Contributor Code
of Conduct](https://r-lib.github.io/desc/dev/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## License

MIT © [Gábor Csárdi](https://github.com/gaborcsardi), [RStudio
Inc](https://github.com/rstudio)
