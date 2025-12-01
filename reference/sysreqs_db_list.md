# List contents of the system requirements DB, for a platform

It also tries to update the system dependency database, if it is
outdated. (I.e. older than allowed in the `metadata_update_after`
[configuration option](https://pak.r-lib.org/reference/pak-config.md) .

## Usage

``` r
sysreqs_db_list(sysreqs_platform = NULL)
```

## Arguments

- sysreqs_platform:

  System requirements platform. If `NULL`, then the `sysreqs_platform`
  [configuration option](https://pak.r-lib.org/reference/pak-config.md)
  is used, which defaults to the current platform. Set this option if
  pak does not detect your platform correctly.

## Value

Data frame with columns:

- `name`: cross platform system dependency name in the database.

- `patterns`: one or more regular expressions to match to
  `SystemRequirements` fields.

- `packages`: one or more system package names to install.

- `pre_install`: command(s) to run before installing the packages.

- `post_install`:: command(s) to run after installing the packages.

## See also

Other system requirements functions:
[`pkg_sysreqs()`](https://pak.r-lib.org/reference/pkg_sysreqs.md),
[`sysreqs_check_installed()`](https://pak.r-lib.org/reference/sysreqs_check_installed.md),
[`sysreqs_db_match()`](https://pak.r-lib.org/reference/sysreqs_db_match.md),
[`sysreqs_db_update()`](https://pak.r-lib.org/reference/sysreqs_db_update.md),
[`sysreqs_is_supported()`](https://pak.r-lib.org/reference/sysreqs_is_supported.md),
[`sysreqs_list_system_packages()`](https://pak.r-lib.org/reference/sysreqs_list_system_packages.md),
[`sysreqs_platforms()`](https://pak.r-lib.org/reference/sysreqs_platforms.md)

## Examples

``` r
sysreqs_db_list(sysreqs_platform = "ubuntu-22.04")
#> # A data frame: 127 × 5
#>    name       patterns  packages  pre_install post_install
#>    <chr>      <list>    <list>    <list>      <list>      
#>  1 Abseil     <chr [1]> <chr [1]> <NULL>      <NULL>      
#>  2 apparmor   <chr [2]> <chr [1]> <NULL>      <NULL>      
#>  3 atk        <chr [1]> <chr [1]> <NULL>      <NULL>      
#>  4 automake   <chr [1]> <chr [1]> <NULL>      <NULL>      
#>  5 berkeleydb <chr [2]> <chr [1]> <NULL>      <NULL>      
#>  6 blender    <chr [1]> <chr [1]> <NULL>      <NULL>      
#>  7 blosc      <chr [1]> <chr [1]> <NULL>      <NULL>      
#>  8 boost      <chr [1]> <chr [1]> <NULL>      <NULL>      
#>  9 bowtie2    <chr [1]> <chr [1]> <NULL>      <NULL>      
#> 10 bwidget    <chr [1]> <chr [1]> <NULL>      <NULL>      
#> # ℹ 117 more rows
```
