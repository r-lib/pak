# Simplified manual. Start here!

You don't need to read long manual pages for a simple task. This manual
page collects the most common pak use cases.

## Package installation

### Install a package from CRAN or Bioconductor

    pak::pkg_install("tibble")

![](figures/tldr-cran.svg)

pak automatically sets a CRAN repository and the Bioconductor
repositories that corresponds to the current R version.

### Install a package from GitHub

    pak::pkg_install("tidyverse/tibble")

    #>
    #> → Will update 2 packages.
    #> → All 2 packages (0 B) are cached.
    #> + tibble 3.1.8 → 3.1.8.9002 👷🏻🔧 (GitHub: 37ec86a)
    #> + vctrs  0.5.1 → 0.5.1.9000 👷🏾‍♀️🔧 (GitHub: 2d7de76)
    #> i No downloads are needed, 2 pkgs are cached
    #> i Packaging vctrs 0.5.1.9000
    #> v Packaged vctrs 0.5.1.9000 (1.4s)
    #> i Building vctrs 0.5.1.9000
    #> v Built vctrs 0.5.1.9000 (11.2s)
    #> v Installed vctrs 0.5.1.9000 (github::r-lib/vctrs@2d7de76) (34ms)
    #> i Packaging tibble 3.1.8.9002
    #> v Packaged tibble 3.1.8.9002 (502ms)
    #> i Building tibble 3.1.8.9002
    #> v Built tibble 3.1.8.9002 (2.7s)
    #> v Installed tibble 3.1.8.9002 (github::tidyverse/tibble@37ec86a) (28ms)
    #> v 1 pkg + 10 deps: kept 9, upd 2 [17.5s]

Use the `user/repo` form. You can specify a branch or tag:
`user/repo@branch` or `user/repo@tag`.

### Install a package from a URL

    pak::pkg_install(
      "url::https://cran.r-project.org/src/contrib/Archive/tibble/tibble_3.1.7.tar.gz"
    )

    #>
    #> → Will install 1 package.
    #> → Will update 1 package.
    #> → All 2 packages (38.65 kB) are cached.
    #> + ellipsis              0.3.2
    #> + tibble   3.1.8.9002 → 3.1.7 👷🏻‍♀️🔧
    #> i No downloads are needed, 2 pkgs (38.65 kB) are cached
    #> v Installed ellipsis 0.3.2  (18ms)
    #> i Building tibble 3.1.7
    #> v Built tibble 3.1.7 (2.5s)
    #> v Installed tibble 3.1.7  (31ms)
    #> v 1 pkg + 11 deps: kept 10, upd 1, added 1 [3.3s]

The URL may point to an R package file, made with `R CMD build`, or a
`.tar.gz` or `.zip` archive of a package tree.

## Package updates

### Update a package

    pak::pkg_install("tibble")

    #>
    #> → Will update 1 package.
    #> → The package (724.32 kB) is cached.
    #> + tibble 3.1.7 → 3.1.8
    #> i No downloads are needed, 1 pkg (724.32 kB) is cached
    #> v Installed tibble 3.1.8  (36ms)
    #> v 1 pkg + 10 deps: kept 10, upd 1 [368ms]

[`pak::pkg_install()`](https://pak.r-lib.org/reference/pkg_install.md)
automatically updates the package.

### Update all dependencies of a package

    pak::pkg_install("tibble", upgrade = TRUE)

    #>
    #> i No downloads are needed
    #> v 1 pkg + 10 deps: kept 11 [278ms]

`upgrade = TRUE` updates the package itself and all of its dependencies,
if necessary.

### Reinstall a package

Add `?reinstall` to the package name or package reference in general:

    pak::pkg_install("tibble?reinstall")

    #>
    #> → Will install 1 package.
    #> → The package (724.32 kB) is cached.
    #> + tibble   3.1.8
    #> i No downloads are needed, 1 pkg (724.32 kB) is cached
    #> v Installed tibble 3.1.8  (60ms)
    #> v 1 pkg + 10 deps: kept 10, added 1 [340ms]

## Dependency lookup

### Dependencies of a CRAN or Bioconductor package

    pak::pkg_deps("tibble")

    #> # A data frame: 11 × 32
    #>    ref       type  direct direc…¹ status package version license needs…²
    #>    <chr>     <chr> <lgl>  <lgl>   <chr>  <chr>   <chr>   <chr>   <lgl>
    #>  1 cli       stan… FALSE  FALSE   OK     cli     3.4.1   MIT + … FALSE
    #>  2 fansi     stan… FALSE  FALSE   OK     fansi   1.0.3   GPL-2 … FALSE
    #>  3 glue      stan… FALSE  FALSE   OK     glue    1.6.2   MIT + … FALSE
    #>  4 lifecycle stan… FALSE  FALSE   OK     lifecy… 1.0.3   MIT + … FALSE
    #>  5 magrittr  stan… FALSE  FALSE   OK     magrit… 2.0.3   MIT + … FALSE
    #>  6 pillar    stan… FALSE  FALSE   OK     pillar  1.8.1   MIT + … FALSE
    #>  7 pkgconfig stan… FALSE  FALSE   OK     pkgcon… 2.0.3   MIT + … FALSE
    #>  8 rlang     stan… FALSE  FALSE   OK     rlang   1.0.6   MIT + … FALSE
    #>  9 tibble    stan… TRUE   TRUE    OK     tibble  3.1.8   MIT + … FALSE
    #> 10 utf8      stan… FALSE  FALSE   OK     utf8    1.2.2   Apache… FALSE
    #> 11 vctrs     stan… FALSE  FALSE   OK     vctrs   0.5.1   MIT + … FALSE
    #> # … with 23 more variables: priority <chr>, md5sum <chr>, sha256 <chr>,
    #> #   filesize <int>, built <chr>, platform <chr>, rversion <chr>,
    #> #   repotype <chr>, repodir <chr>, target <chr>, deps <list>,
    #> #   mirror <chr>, sources <list>, remote <list>, error <list>,
    #> #   metadata <list>, dep_types <list>, params <list>, sysreqs <chr>,
    #> #   cache_status <chr>, lib_status <chr>, old_version <chr>,
    #> #   new_version <chr>, and abbreviated variable names ¹​directpkg, …

The results are returned in a data frame.

### Dependency tree of a CRAN / Bioconductor package

    pak::pkg_deps_tree("tibble")

    #> tibble 3.1.8 ✨
    #> ├─fansi 1.0.3 ✨
    #> ├─lifecycle 1.0.3 ✨
    #> │ ├─cli 3.4.1 ✨
    #> │ ├─glue 1.6.2 ✨
    #> │ └─rlang 1.0.6 ✨
    #> ├─magrittr 2.0.3 ✨
    #> ├─pillar 1.8.1 ✨
    #> │ ├─cli
    #> │ ├─fansi
    #> │ ├─glue
    #> │ ├─lifecycle
    #> │ ├─rlang
    #> │ ├─utf8 1.2.2 ✨
    #> │ └─vctrs 0.5.1 ✨
    #> │   ├─cli
    #> │   ├─glue
    #> │   ├─lifecycle
    #> │   └─rlang
    #> ├─pkgconfig 2.0.3 ✨
    #> ├─rlang
    #> └─vctrs
    #>
    #> Key:  ✨ new

The results are also silently returned in a data frame.

### Dependency tree of a package on GitHub

    pak::pkg_deps_tree("tidyverse/tibble")

    #> tidyverse/tibble 3.1.8.9002 ✨👷🏼🔧 
    #> ├─fansi 1.0.3 ✨
    #> ├─lifecycle 1.0.3 ✨
    #> │ ├─cli 3.4.1 ✨
    #> │ ├─glue 1.6.2 ✨
    #> │ └─rlang 1.0.6 ✨
    #> ├─magrittr 2.0.3 ✨
    #> ├─pillar 1.8.1 ✨
    #> │ ├─cli
    #> │ ├─fansi
    #> │ ├─glue
    #> │ ├─lifecycle
    #> │ ├─rlang
    #> │ ├─utf8 1.2.2 ✨
    #> │ └─r-lib/vctrs 0.5.1.9000 ✨👷🏼🔧 
    #> │   ├─cli
    #> │   ├─glue
    #> │   ├─lifecycle
    #> │   └─rlang
    #> ├─pkgconfig 2.0.3 ✨
    #> ├─rlang
    #> └─r-lib/vctrs
    #>
    #> Key:  ✨ new | 👷🏼 build | 🔧 compile

Use the `user/repo` form. As usual, you can also select a branch, tag,
or sha, with the `user/repo@branch`, `user/repo@tag` or `user/repo@sha`
forms.

### Dependency tree of the package in the current directory

    pak::local_deps_tree("tibble")

    #> local::tibble 3.1.8 ✨👷🏻‍♀️🔧    
    #> ├─fansi 1.0.3 ✨
    #> ├─lifecycle 1.0.3 ✨
    #> │ ├─cli 3.4.1 ✨
    #> │ ├─glue 1.6.2 ✨
    #> │ └─rlang 1.0.6 ✨
    #> ├─magrittr 2.0.3 ✨
    #> ├─pillar 1.8.1 ✨
    #> │ ├─cli
    #> │ ├─fansi
    #> │ ├─glue
    #> │ ├─lifecycle
    #> │ ├─rlang
    #> │ ├─utf8 1.2.2 ✨
    #> │ └─vctrs 0.5.1 ✨
    #> │   ├─cli
    #> │   ├─glue
    #> │   ├─lifecycle
    #> │   └─rlang
    #> ├─pkgconfig 2.0.3 ✨
    #> ├─rlang
    #> └─vctrs
    #>
    #> Key:  ✨ new | 👷🏻‍♀️ build | 🔧 compile

Assuming package is in directory `tibble`.

### Explain a recursive dependency

How does tibble depend on rlang?

    pak::pkg_deps_explain("tibble", "rlang")

    #> tibble -> lifecycle -> rlang
    #> tibble -> pillar -> lifecycle -> rlang
    #> tibble -> pillar -> rlang
    #> tibble -> pillar -> vctrs -> lifecycle -> rlang
    #> tibble -> pillar -> vctrs -> rlang
    #> tibble -> rlang
    #> tibble -> vctrs -> lifecycle -> rlang
    #> tibble -> vctrs -> rlang

Use can also use the `user/repo` form for packages from GitHub,
`url::...` for packages at URLs, etc.

## Package development

### Install dependencies of local package

    pak::local_install_deps()

    #> v Loading metadata database ... done
    #>
    #> → The package (0 B) is cached.
    #> i No downloads are needed
    #> v 10 deps: kept 10 [3.2s]

### Install local package

    pak::local_install()

    #>
    #> → Will update 1 package.
    #> → The package (0 B) is cached.
    #> + tibble 3.1.8 → 3.1.8 👷🏻‍♂️🔧
    #> i No downloads are needed, 1 pkg is cached
    #> v Got tibble 3.1.8 (source) (96 B)
    #> i Packaging tibble 3.1.8
    #> v Packaged tibble 3.1.8 (864ms)
    #> i Building tibble 3.1.8
    #> v Built tibble 3.1.8 (2.4s)
    #> v Installed tibble 3.1.8 (local) (38ms)
    #> v 1 pkg + 10 deps: kept 10, upd 1, dld 1 (NA B) [4.2s]

### Install all dependencies of local package

    pak::local_install_dev_deps()

    #>
    #> → Will install 86 packages.
    #> → Will update 2 packages.
    #> → All 89 packages (100.53 MB) are cached.
    #> + askpass                1.1
    #> + base64enc              0.1-3
    #> + bench                  1.1.2
    #> + bit                    4.0.5
    #> + bit64                  4.0.5
    #> + blob                   1.2.3
    #> + brio                   1.1.3
    #> + bslib                  0.4.1
    #> + cachem                 1.0.6
    #> + callr                  3.7.3
    #> + clipr                  0.8.0
    #> + colorspace             2.0-3
    #> + covr                   3.6.1
    #> + crayon                 1.5.2
    #> + curl                   4.3.3
    #> + desc                   1.4.2
    #> + DiagrammeR             1.0.9
    #> + diffobj                0.3.5
    #> + digest                 0.6.31
    #> + downloader             0.4
    #> + dplyr                  1.0.10
    #> + evaluate               0.19    👷🏿‍♂️
    #> + farver                 2.1.1
    #> + fastmap                1.1.0
    #> + formattable            0.2.1
    #> + fs                     1.5.2
    #> + generics               0.1.3
    #> + ggplot2                3.4.0
    #> + gridExtra              2.3
    #> + gtable                 0.3.1
    #> + highr                  0.9
    #> + hms                    1.1.2
    #> + htmltools              0.5.4
    #> + htmlwidgets            1.6.0   👷🏾‍♂️
    #> + httr                   1.4.4
    #> + igraph                 1.3.5
    #> + influenceR             0.1.0.1
    #> + isoband                0.2.6
    #> + jquerylib              0.1.4
    #> + jsonlite               1.8.4
    #> + knitr                  1.41
    #> + labeling               0.4.2
    #> + lazyeval               0.2.2
    #> + lubridate              1.9.0
    #> + Matrix       1.5-1   → 1.5-3
    #> + memoise                2.0.1
    #> + mime                   0.12
    #> + mockr                  0.2.0
    #> + munsell                0.5.0
    #> + nlme         3.1-160 → 3.1-161 👷‍♂️🔧
    #> + nycflights13           1.0.2
    #> + openssl                2.0.5
    #> + pkgbuild               1.4.0
    #> + pkgload                1.3.2
    #> + praise                 1.0.0
    #> + prettyunits            1.1.1
    #> + processx               3.8.0
    #> + profmem                0.6.0
    #> + ps                     1.7.2
    #> + purrr                  0.3.5
    #> + R6                     2.5.1
    #> + rappdirs               0.3.3
    #> + RColorBrewer           1.1-3
    #> + readr                  2.1.3
    #> + rematch2               2.1.2
    #> + rex                    1.2.1
    #> + rmarkdown              2.19    👷‍♂️
    #> + rprojroot              2.0.3
    #> + rstudioapi             0.14
    #> + sass                   0.4.4
    #> + scales                 1.2.1
    #> + stringi                1.7.8
    #> + stringr                1.5.0
    #> + sys                    3.4.1
    #> + testthat               3.1.6
    #> + tidyr                  1.2.1
    #> + tidyselect             1.2.0
    #> + timechange             0.1.1
    #> + tinytex                0.43    👷🏻‍♂️
    #> + tzdb                   0.3.0
    #> + viridis                0.6.2
    #> + viridisLite            0.4.1
    #> + visNetwork             2.1.2
    #> + vroom                  1.6.0
    #> + waldo                  0.4.0
    #> + withr                  2.5.0
    #> + xfun                   0.35
    #> + yaml                   2.3.6
    #> i No downloads are needed, 88 pkgs (100.53 MB) are cached
    #> i Packaging tibble 3.1.8
    #> i Building evaluate 0.19
    #> i Building nlme 3.1-161
    #> v Installed R6 2.5.1  (31ms)
    #> v Installed DiagrammeR 1.0.9  (76ms)
    #> v Installed RColorBrewer 1.1-3  (72ms)
    #> v Installed askpass 1.1  (79ms)
    #> v Installed Matrix 1.5-3  (137ms)
    #> v Installed base64enc 0.1-3  (125ms)
    #> v Installed bench 1.1.2  (90ms)
    #> v Installed bit64 4.0.5  (44ms)
    #> v Installed bit 4.0.5  (43ms)
    #> v Installed blob 1.2.3  (41ms)
    #> v Installed brio 1.1.3  (40ms)
    #> v Installed cachem 1.0.6  (31ms)
    #> v Installed callr 3.7.3  (53ms)
    #> v Installed clipr 0.8.0  (89ms)
    #> v Installed colorspace 2.0-3  (99ms)
    #> v Installed covr 3.6.1  (58ms)
    #> v Installed crayon 1.5.2  (75ms)
    #> v Installed curl 4.3.3  (83ms)
    #> v Packaged tibble 3.1.8 (684ms)
    #> v Installed bslib 0.4.1  (315ms)
    #> v Installed desc 1.4.2  (77ms)
    #> v Installed diffobj 0.3.5  (68ms)
    #> v Installed digest 0.6.31  (60ms)
    #> v Installed downloader 0.4  (39ms)
    #> v Installed dplyr 1.0.10  (39ms)
    #> v Installed farver 2.1.1  (41ms)
    #> v Installed fastmap 1.1.0  (38ms)
    #> v Installed formattable 0.2.1  (43ms)
    #> v Built evaluate 0.19 (903ms)
    #> v Installed fs 1.5.2  (49ms)
    #> v Installed generics 0.1.3  (46ms)
    #> v Installed ggplot2 3.4.0  (65ms)
    #> v Installed gridExtra 2.3  (43ms)
    #> v Installed gtable 0.3.1  (38ms)
    #> v Installed highr 0.9  (37ms)
    #> v Installed hms 1.1.2  (39ms)
    #> v Installed htmltools 0.5.4  (40ms)
    #> v Installed httr 1.4.4  (40ms)
    #> v Installed influenceR 0.1.0.1  (17ms)
    #> v Installed igraph 1.3.5  (96ms)
    #> v Installed isoband 0.2.6  (68ms)
    #> v Installed jquerylib 0.1.4  (38ms)
    #> v Installed jsonlite 1.8.4  (37ms)
    #> v Installed labeling 0.4.2  (14ms)
    #> v Installed knitr 1.41  (73ms)
    #> v Installed lazyeval 0.2.2  (43ms)
    #> v Installed lubridate 1.9.0  (38ms)
    #> v Installed memoise 2.0.1  (39ms)
    #> v Installed mime 0.12  (58ms)
    #> v Installed mockr 0.2.0  (38ms)
    #> v Installed munsell 0.5.0  (36ms)
    #> v Installed nycflights13 1.0.2  (37ms)
    #> v Installed openssl 2.0.5  (41ms)
    #> v Installed pkgbuild 1.4.0  (39ms)
    #> v Installed pkgload 1.3.2  (37ms)
    #> v Installed praise 1.0.0  (35ms)
    #> v Installed prettyunits 1.1.1  (56ms)
    #> v Installed processx 3.8.0  (37ms)
    #> v Installed profmem 0.6.0  (37ms)
    #> v Installed ps 1.7.2  (37ms)
    #> v Installed purrr 0.3.5  (38ms)
    #> v Installed rappdirs 0.3.3  (37ms)
    #> v Installed readr 2.1.3  (42ms)
    #> v Installed rematch2 2.1.2  (41ms)
    #> v Installed rex 1.2.1  (58ms)
    #> v Installed rprojroot 2.0.3  (58ms)
    #> v Installed rstudioapi 0.14  (40ms)
    #> v Installed sass 0.4.4  (42ms)
    #> v Installed scales 1.2.1  (39ms)
    #> v Installed stringr 1.5.0  (33ms)
    #> v Installed sys 3.4.1  (49ms)
    #> v Installed testthat 3.1.6  (88ms)
    #> v Installed tidyr 1.2.1  (77ms)
    #> v Installed stringi 1.7.8  (195ms)
    #> v Installed tidyselect 1.2.0  (113ms)
    #> v Installed timechange 0.1.1  (55ms)
    #> v Installed tzdb 0.3.0  (40ms)
    #> v Installed viridisLite 0.4.1  (39ms)
    #> v Installed viridis 0.6.2  (39ms)
    #> v Installed visNetwork 2.1.2  (77ms)
    #> v Installed vroom 1.6.0  (77ms)
    #> v Installed waldo 0.4.0  (62ms)
    #> v Installed withr 2.5.0  (40ms)
    #> v Installed xfun 0.35  (39ms)
    #> i Building tinytex 0.43
    #> v Installed yaml 2.3.6  (43ms)
    #> v Installed evaluate 0.19  (15ms)
    #> v Built tinytex 0.43 (1.1s)
    #> v Installed tinytex 0.43  (15ms)
    #> i Building rmarkdown 2.19
    #> v Built rmarkdown 2.19 (3.9s)
    #> v Installed rmarkdown 2.19  (85ms)
    #> i Building htmlwidgets 1.6.0
    #> v Built nlme 3.1-161 (8s)
    #> v Installed nlme 3.1-161  (33ms)
    #> v Built htmlwidgets 1.6.0 (1.1s)
    #> v Installed htmlwidgets 1.6.0  (22ms)
    #> v 103 deps: kept 15, upd 2, added 86 [10.2s]

Installs development and optional dependencies as well.

## Repositories

### List current repositories

    pak::repo_get()

    #> # A data frame: 5 × 5
    #>   name          url                                type  r_ver…¹ bioc_…²
    #> * <chr>         <chr>                              <chr> <chr>   <chr>
    #> 1 CRAN          https://cloud.r-project.org        cran  *       NA
    #> 2 BioCsoft      https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 3 BioCann       https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 4 BioCexp       https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 5 BioCworkflows https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> # … with abbreviated variable names ¹​r_version, ²​bioc_version

If you haven't set a CRAN or Bioconductor repository, pak does that
automatically.

### Add custom repository

    pak::repo_add(rhub = 'https://r-hub.r-universe.dev')
    pak::repo_get()

    #> # A data frame: 6 × 5
    #>   name          url                                type  r_ver…¹ bioc_…²
    #> * <chr>         <chr>                              <chr> <chr>   <chr>
    #> 1 CRAN          https://cloud.r-project.org        cran  *       NA
    #> 2 rhub          https://r-hub.r-universe.dev       cran… *       NA
    #> 3 BioCsoft      https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 4 BioCann       https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 5 BioCexp       https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 6 BioCworkflows https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> # … with abbreviated variable names ¹​r_version, ²​bioc_version

### Remove custom repositories

    options(repos = getOption("repos")["CRAN"])
    pak::repo_get()

    #> # A data frame: 5 × 5
    #>   name          url                                type  r_ver…¹ bioc_…²
    #> * <chr>         <chr>                              <chr> <chr>   <chr>
    #> 1 CRAN          https://cloud.r-project.org        cran  *       NA
    #> 2 BioCsoft      https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 3 BioCann       https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 4 BioCexp       https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 5 BioCworkflows https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> # … with abbreviated variable names ¹​r_version, ²​bioc_version

If you set the `repos` option to a CRAN repo only, or unset it
completely, then pak keeps only CRAN and (by default) Bioconductor.

### Time travel using RSPM

    pak::repo_add(CRAN = "RSPM@2022-06-30")
    pak::repo_get()

    #> # A data frame: 5 × 5
    #>   name          url                                type  r_ver…¹ bioc_…²
    #> * <chr>         <chr>                              <chr> <chr>   <chr>
    #> 1 CRAN          https://packagemanager.posit.co/c… cran  *       NA
    #> 2 BioCsoft      https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 3 BioCann       https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 4 BioCexp       https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 5 BioCworkflows https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> # … with abbreviated variable names ¹​r_version, ²​bioc_version

Sets a repository that is equivalent to CRAN's state closest to the
specified date. Name this repository `CRAN`, otherwise pak will also add
a default CRAN repository.

### Time travel using MRAN

    pak::repo_add(CRAN = "MRAN@2022-06-30")
    pak::repo_get()

    #> # A data frame: 5 × 5
    #>   name          url                                type  r_ver…¹ bioc_…²
    #> * <chr>         <chr>                              <chr> <chr>   <chr>
    #> 1 CRAN          https://cran.microsoft.com/snapsh… cran  *       NA
    #> 2 BioCsoft      https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 3 BioCann       https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 4 BioCexp       https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> 5 BioCworkflows https://bioconductor.org/packages… bioc  4.2.2   3.16
    #> # … with abbreviated variable names ¹​r_version, ²​bioc_version

Sets a repository that is equivalent to CRAN's state at the specified
date. Name this repository `CRAN`, otherwise pak will also add a default
CRAN repository.

## Caches

By default pak caches both metadata and downloaded packages.

### Inspect metadata cache

    pak::meta_list()

    #> v Updated metadata database: 4.55 MB in 4 files.
    #> v Updating metadata database ... done
    #> # A data frame: 43,718 × 32
    #>    package version depends sugge…¹ license imports linki…² archs enhan…³
    #>    <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr> <chr>
    #>  1 A3      1.0.0   R (>= … random… GPL (>… NA      NA      NA    NA
    #>  2 AATtoo… 0.0.1   R (>= … NA      GPL-3   magrit… NA      NA    NA
    #>  3 ABACUS  1.0.0   R (>= … rmarkd… GPL-3   ggplot… NA      NA    NA
    #>  4 ABC.RAP 0.9.0   R (>= … knitr,… GPL-3   graphi… NA      NA    NA
    #>  5 ABCana… 1.2.1   R (>= … NA      GPL-3   plotrix NA      NA    NA
    #>  6 ABCopt… 0.15.0  NA      testth… MIT + … Rcpp, … Rcpp    ABCo… NA
    #>  7 ABCp2   1.2     MASS    NA      GPL-2   NA      NA      NA    NA
    #>  8 ABHgen… 1.0.1   NA      knitr,… GPL-3   ggplot… NA      NA    NA
    #>  9 ABPS    0.3     NA      testth… GPL (>… kernlab NA      NA    NA
    #> 10 ACA     1.1     R (>= … NA      GPL     graphi… NA      NA    NA
    #> # … with 43,708 more rows, 23 more variables: os_type <chr>,
    #> #   priority <chr>, license_is_foss <chr>, license_restricts_use <chr>,
    #> #   repodir <chr>, rversion <chr>, platform <chr>,
    #> #   needscompilation <chr>, ref <chr>, type <chr>, direct <lgl>,
    #> #   status <chr>, target <chr>, mirror <chr>, sources <list>,
    #> #   filesize <int>, sha256 <chr>, sysreqs <chr>, built <chr>,
    #> #   published <dttm>, deps <list>, md5sum <chr>, path <chr>, and …

### Update metadata cache

By default
[`pkg_install()`](https://pak.r-lib.org/reference/pkg_install.md) and
similar functions automatically update the metadata for the currently
set repositories if it is older than 24 hours. You can also force an
update manually:

    pak::meta_update()

    #> v Updating metadata database ... done

### Clean metadata cache

    pak::meta_clean(force = TRUE)
    pak::meta_summary()

    #> [1] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metad
    #> ata"
    #>
    #> $current_db
    #> [1] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/_metad
    #> ata/pkgs-d1c324e625.rds"
    #>
    #> $raw_files
    #> character(0)
    #>
    #> $db_files
    #> character(0)
    #>
    #> $size
    #> [1] 0
    #>

### Inspect package cache

Downloaded packages are also cached.

    pak::cache_list()

    #> # A data frame: 480 × 11
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
    #> # … with 470 more rows, 2 more variables: vignettes <int>,
    #> #   rversion <chr>, and abbreviated variable name ¹​platform

### View a package cache summary

    pak::cache_summary()

    #> $cachepath
    #> [1] "/Users/gaborcsardi/Library/Caches/org.R-project.R/R/pkgcache/pkg"
    #>
    #> $files
    #> [1] 480
    #>
    #> $size
    #> [1] 653325143
    #>

### Clean package cache

    pak::cache_clean()

## Libraries

### List packages in a library

    pak::lib_status(Sys.getenv("R_LIBS_USER"))

    #> # A data frame: 701 × 39
    #>    library   package title version depends repos…¹ license needs…² built
    #>    <chr>     <chr>   <chr> <chr>   <chr>   <chr>   <chr>   <lgl>   <chr>
    #>  1 /Users/g… abc     "Too… 2.2.1   R (>= … CRAN    GPL (>… FALSE   R 4.…
    #>  2 /Users/g… abc.da… "Dat… 1.0     R (>= … CRAN    GPL (>… FALSE   R 4.…
    #>  3 /Users/g… abind   "Com… 1.4-5   R (>= … CRAN    LGPL (… FALSE   R 4.…
    #>  4 /Users/g… ade4    "Ana… 1.7-19  R (>= … CRAN    GPL (>… TRUE    R 4.…
    #>  5 /Users/g… ape     "Ana… 5.6-2   R (>= … CRAN    GPL-2 … TRUE    R 4.…
    #>  6 /Users/g… aplot   "Dec… 0.1.7   NA      CRAN    Artist… FALSE   R 4.…
    #>  7 /Users/g… archive "Mul… 1.1.5   R (>= … CRAN    MIT + … TRUE    R 4.…
    #>  8 /Users/g… arrayh… "Con… 1.1-0   NA      CRAN    GPL     FALSE   R 4.…
    #>  9 /Users/g… arrow   "Int… 9.0.0   R (>= … CRAN    Apache… TRUE    R 4.…
    #> 10 /Users/g… arules  "Min… 1.7-5   R (>= … CRAN    GPL-3   TRUE    R 4.…
    #> # … with 691 more rows, 30 more variables: remotetype <chr>,
    #> #   remotepkgref <chr>, remoteref <chr>, remoterepos <chr>,
    #> #   remotepkgplatform <chr>, remotesha <chr>, imports <chr>,
    #> #   suggests <chr>, linkingto <chr>, remotes <chr>, remotehost <chr>,
    #> #   remoterepo <chr>, remoteusername <chr>, enhances <chr>,
    #> #   biocviews <chr>, remoteurl <chr>, remotesubdir <chr>,
    #> #   priority <chr>, remoteetag <chr>, remotepackaged <chr>, …

Pass the directory of the library as the argument.
