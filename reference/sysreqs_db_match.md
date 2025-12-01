# Match system requirement descriptions to the database

In the usual workflow pak matches the `SystemRequirements` fields of the
`DESCRIPTION` files to the database.

## Usage

``` r
sysreqs_db_match(specs, sysreqs_platform = NULL)
```

## Arguments

- specs:

  Character vector of system requirements descriptions.

- sysreqs_platform:

  System requirements platform. If `NULL`, then the `sysreqs_platform`
  [configuration option](https://pak.r-lib.org/reference/pak-config.md)
  is used, which defaults to the current platform. Set this option if
  pak does not detect your platform correctly.

## Value

Data frame with columns:

- `spec`: the input `specs`.

- `sysreq`: name of the system library or tool.

- `packages`: system packages, list column of character vectors. Rarely
  it can be an empty string, e.g. if a `pre_install` script performs the
  installation.

- `pre_install`: list column of character vectors. Shell script(s) to
  run before the installation.

- `post_install`: list column of character vectors. Shell script(s) to
  run after the installation.

## Details

The `sysreqs_db_match()` function lets you match any string, and it is
mainly useful for debugging.

## See also

Other system requirements functions:
[`pkg_sysreqs()`](https://pak.r-lib.org/reference/pkg_sysreqs.md),
[`sysreqs_check_installed()`](https://pak.r-lib.org/reference/sysreqs_check_installed.md),
[`sysreqs_db_list()`](https://pak.r-lib.org/reference/sysreqs_db_list.md),
[`sysreqs_db_update()`](https://pak.r-lib.org/reference/sysreqs_db_update.md),
[`sysreqs_is_supported()`](https://pak.r-lib.org/reference/sysreqs_is_supported.md),
[`sysreqs_list_system_packages()`](https://pak.r-lib.org/reference/sysreqs_list_system_packages.md),
[`sysreqs_platforms()`](https://pak.r-lib.org/reference/sysreqs_platforms.md)

## Examples

``` r
sysreqs_db_match(
  c("Needs libcurl", "Java, libssl"),
  sysreqs_platform = "ubuntu-22.04"
)
#> [[1]]
#> # A data frame: 1 × 5
#>   spec          sysreq  packages  pre_install post_install
#>   <chr>         <chr>   <list>    <list>      <list>      
#> 1 Needs libcurl libcurl <chr [1]> <NULL>      <NULL>      
#> 
#> [[2]]
#> # A data frame: 1 × 5
#>   spec         sysreq packages  pre_install post_install
#>   <chr>        <chr>  <list>    <list>      <list>      
#> 1 Java, libssl java   <chr [1]> <NULL>      <chr [1]>   
#> 
```
