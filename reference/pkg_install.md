# Install packages

Install one or more packages and their dependencies into a single
package library.

## Usage

``` r
pkg_install(
  pkg,
  lib = .libPaths()[[1L]],
  upgrade = FALSE,
  ask = interactive(),
  dependencies = NA
)
```

## Arguments

- pkg:

  Package names or package references. E.g.

  - `ggplot2`: package from CRAN, Bioconductor or a CRAN-like repository
    in general,

  - `tidyverse/ggplot2`: package from GitHub,

  - `tidyverse/ggplot2@v3.4.0`: package from GitHub tag or branch,

  - `https://examples.com/.../ggplot2_3.3.6.tar.gz`: package from URL,

  - `.`: package in the current working directory.

  See "[Package
  sources](https://pak.r-lib.org/reference/pak_package_sources.md)" for
  more details.

- lib:

  Package library to install the packages to. Note that *all* dependent
  packages will be installed here, even if they are already installed in
  another library. The only exceptions are base and recommended packages
  installed in `.Library`. These are not duplicated in `lib`, unless a
  newer version of a recommended package is needed.

- upgrade:

  When `FALSE`, the default, pak does the minimum amount of work to give
  you the latest version(s) of `pkg`. It will only upgrade dependent
  packages if `pkg`, or one of their dependencies explicitly require a
  higher version than what you currently have. It will also prefer a
  binary package over to source package, even it the binary package is
  older.

  When `upgrade = TRUE`, pak will ensure that you have the latest
  version(s) of `pkg` and all their dependencies.

- ask:

  Whether to ask for confirmation when installing a different version of
  a package that is already installed. Installations that only add new
  packages never require confirmation.

- dependencies:

  What kinds of dependencies to install. Most commonly one of the
  following values:

  - `NA`: only required (hard) dependencies,

  - `TRUE`: required dependencies plus optional and development
    dependencies,

  - `FALSE`: do not install any dependencies. (You might end up with a
    non-working package, and/or the installation might fail.) See
    [Package dependency
    types](https://pak.r-lib.org/reference/package-dependency-types.md)
    for other possible values and more information about package
    dependencies.

## Value

(Invisibly) A data frame with information about the installed
package(s).

## Examples

    pkg_install("dplyr")

    #>
    #> → Will install 5 packages.
    #> → All 5 packages (2.14 MB) are cached.
    #> + dplyr        1.0.9
    #> + generics     0.1.2
    #> + purrr        0.3.4
    #> + R6           2.5.1
    #> + tidyselect   1.1.2
    #> i No downloads are needed, 5 pkgs (2.14 MB) are cached
    #> v Got R6 2.5.1 (aarch64-apple-darwin20) (82.52 kB)
    #> v Installed R6 2.5.1  (43ms)
    #> v Installed generics 0.1.2  (62ms)
    #> v Installed dplyr 1.0.9  (88ms)
    #> v Installed purrr 0.3.4  (88ms)
    #> v Installed tidyselect 1.1.2  (94ms)
    #> v 1 pkg + 17 deps: kept 12, added 5, dld 1 (82.52 kB) [1.2s]

Upgrade dplyr and all its dependencies:

    pkg_install("dplyr", upgrade = TRUE)

    #>
    #> → Will update 1 package.
    #> → The package (742.51 kB) is cached.
    #> + rlang 1.0.2 → 1.0.3 👷🏿‍♀️🔧
    #> i No downloads are needed, 1 pkg (742.51 kB) is cached
    #> i Building rlang 1.0.3
    #> v Built rlang 1.0.3 (5.8s)
    #> v Installed rlang 1.0.3  (32ms)
    #> v 1 pkg + 17 deps: kept 17, upd 1 [6.3s]

Install the development version of dplyr:

    pkg_install("tidyverse/dplyr")

    #> v Loading metadata database ... done
    #>
    #> → Will install 16 packages.
    #> → All 16 packages (8.09 MB) are cached.
    #> + cli          3.4.1
    #> + dplyr        1.0.99.9000 👷🏾‍♂️🔧 (GitHub: e6252f8)
    #> + fansi        1.0.3
    #> + generics     0.1.3
    #> + glue         1.6.2
    #> + lifecycle    1.0.3
    #> + magrittr     2.0.3
    #> + pillar       1.8.1
    #> + pkgconfig    2.0.3
    #> + R6           2.5.1
    #> + rlang        1.0.6
    #> + tibble       3.1.8
    #> + tidyselect   1.2.0
    #> + utf8         1.2.2
    #> + vctrs        0.5.1
    #> + withr        2.5.0
    #> i No downloads are needed, 16 pkgs (8.09 MB) are cached
    #> v Installed R6 2.5.1  (58ms)
    #> v Installed cli 3.4.1  (69ms)
    #> v Installed fansi 1.0.3  (92ms)
    #> v Installed generics 0.1.3  (99ms)
    #> v Installed glue 1.6.2  (108ms)
    #> v Installed lifecycle 1.0.3  (144ms)
    #> v Installed magrittr 2.0.3  (152ms)
    #> v Installed pillar 1.8.1  (160ms)
    #> v Installed pkgconfig 2.0.3  (63ms)
    #> v Installed rlang 1.0.6  (37ms)
    #> v Installed tibble 3.1.8  (41ms)
    #> v Installed tidyselect 1.2.0  (38ms)
    #> v Installed utf8 1.2.2  (36ms)
    #> v Installed vctrs 0.5.1  (39ms)
    #> v Installed withr 2.5.0  (30ms)
    #> i Packaging dplyr 1.0.99.9000
    #> v Packaged dplyr 1.0.99.9000 (8.3s)
    #> i Building dplyr 1.0.99.9000
    #> v Built dplyr 1.0.99.9000 (5.2s)
    #> v Installed dplyr 1.0.99.9000 (github::tidyverse/dplyr@e6252f8) (24ms)
    #> v 1 pkg + 15 deps: added 16 [18.9s]

Switch back to the CRAN version. This will be fast because pak will have
cached the prior install.

    pkg_install("dplyr")

    #> v Updated metadata database: 2.43 MB in 6 files.
    #> v Updating metadata database ... done
    #>
    #> → Will install 16 packages.
    #> → All 16 packages (9.42 MB) are cached.
    #> + cli          3.4.1
    #> + dplyr        1.0.10
    #> + fansi        1.0.3
    #> + generics     0.1.3
    #> + glue         1.6.2
    #> + lifecycle    1.0.3
    #> + magrittr     2.0.3
    #> + pillar       1.8.1
    #> + pkgconfig    2.0.3
    #> + R6           2.5.1
    #> + rlang        1.0.6
    #> + tibble       3.1.8
    #> + tidyselect   1.2.0
    #> + utf8         1.2.2
    #> + vctrs        0.5.1
    #> + withr        2.5.0
    #> i No downloads are needed, 16 pkgs (9.42 MB) are cached
    #> v Installed R6 2.5.1  (66ms)
    #> v Installed cli 3.4.1  (76ms)
    #> v Installed dplyr 1.0.10  (111ms)
    #> v Installed fansi 1.0.3  (119ms)
    #> v Installed generics 0.1.3  (125ms)
    #> v Installed glue 1.6.2  (132ms)
    #> v Installed lifecycle 1.0.3  (149ms)
    #> v Installed magrittr 2.0.3  (162ms)
    #> v Installed pillar 1.8.1  (56ms)
    #> v Installed pkgconfig 2.0.3  (35ms)
    #> v Installed rlang 1.0.6  (57ms)
    #> v Installed tibble 3.1.8  (41ms)
    #> v Installed tidyselect 1.2.0  (40ms)
    #> v Installed utf8 1.2.2  (37ms)
    #> v Installed vctrs 0.5.1  (39ms)
    #> v Installed withr 2.5.0  (31ms)
    #> v 1 pkg + 15 deps: added 16 [7.2s]

## See also

[Get started with pak](https://pak.r-lib.org/reference/get-started.md),
[Package
sources](https://pak.r-lib.org/reference/pak_package_sources.md),
[FAQ](https://pak.r-lib.org/reference/faq.md), [The dependency
solver](https://pak.r-lib.org/reference/pak_solver.md).

Other package functions:
[`lib_status()`](https://pak.r-lib.org/reference/lib_status.md),
[`pak()`](https://pak.r-lib.org/reference/pak.md),
[`pkg_deps_tree()`](https://pak.r-lib.org/reference/pkg_deps_tree.md),
[`pkg_deps()`](https://pak.r-lib.org/reference/pkg_deps.md),
[`pkg_download()`](https://pak.r-lib.org/reference/pkg_download.md),
[`pkg_remove()`](https://pak.r-lib.org/reference/pkg_remove.md),
[`pkg_status()`](https://pak.r-lib.org/reference/pkg_status.md),
[`pkg_sysreqs()`](https://pak.r-lib.org/reference/pkg_sysreqs.md)
