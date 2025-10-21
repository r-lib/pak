# Check if installed packages have all their system requirements

`sysreqs_check_installed()` checks if the system requirements of all
packages (or a subset of packages) are installed.

`sysreqs_fix_installed()` installs the missing system packages.

## Usage

``` r
sysreqs_check_installed(packages = NULL, library = .libPaths()[1])
sysreqs_fix_installed(packages = NULL, library = .libPaths()[1])
```

## Arguments

- packages:

  If not `NULL`, then only these packages are checked. If a package in
  `packages` is not installed, then pak throws a warning.

- library:

  Library or libraries to check.

## Value

Data frame with a custom print and format method, and a
`pkg_sysreqs_check_result` class. Its columns are:

- `system_package`: string, name of the required system package.

- `installed`: logical, whether the system package is correctly
  installed.

- `packages`: list column of character vectors. The names of the
  installed R packages that need this system package.

- `pre_install`: list column of character vectors. Commands to run
  before the installation of the the system package.

- `post_install`: list column of character vectors. Commands to run
  after the installation of the system package.

The data frame also have two attributes with additional data:

- `sysreqs_records`: the raw system requirements records, and

- `system_packages`: the list of the installed system packages.

`sysreqs_fix_packages()` returns the same value, but invisibly.

## Details

These functions use the `sysreqs_platform` configuration option, see
[Configuration](https://pak.r-lib.org/dev/reference/pak-config.md) . Set
this if pak does not detect your platform correctly.

## See also

Other system requirements functions:
[`pkg_sysreqs()`](https://pak.r-lib.org/dev/reference/pkg_sysreqs.md),
[`sysreqs_db_list()`](https://pak.r-lib.org/dev/reference/sysreqs_db_list.md),
[`sysreqs_db_match()`](https://pak.r-lib.org/dev/reference/sysreqs_db_match.md),
[`sysreqs_db_update()`](https://pak.r-lib.org/dev/reference/sysreqs_db_update.md),
[`sysreqs_is_supported()`](https://pak.r-lib.org/dev/reference/sysreqs_is_supported.md),
[`sysreqs_list_system_packages()`](https://pak.r-lib.org/dev/reference/sysreqs_list_system_packages.md),
[`sysreqs_platforms()`](https://pak.r-lib.org/dev/reference/sysreqs_platforms.md)

## Examples

``` r
# This only works on supported platforms
sysreqs_check_installed()
#> system package       installed required by                   
#> --------------       --        -----------                   
#> git                  ✔         credentials, gitcreds         
#> gsfonts              ✔         magick                        
#> libcurl4-openssl-dev ✔         curl                          
#> libfontconfig1-dev   ✔         systemfonts                   
#> libfreetype6-dev     ✔         ragg, systemfonts, textshaping
#> libfribidi-dev       ✔         textshaping                   
#> libgit2-dev          ✔         gert                          
#> libharfbuzz-dev      ✔         textshaping                   
#> libicu-dev           ✔         stringi                       
#> libjpeg-dev          ✔         ragg                          
#> libmagick++-dev      ✔         magick                        
#> libnode-dev          ✔         V8                            
#> libpng-dev           ✔         ragg                          
#> libsecret-1-dev      ✔         keyring                       
#> libssl-dev           ✔         curl, openssl, PKI            
#> libtiff-dev          ✔         ragg                          
#> libwebp-dev          ✔         ragg                          
#> libx11-dev           ✔         clipr                         
#> libxml2-dev          ✔         xml2                          
#> make                 ✔         fs, httpuv, rticles, sass     
#> pandoc               ✔         knitr, pkgdown, rmarkdown     
#> zlib1g-dev           ✔         httpuv                        
```
