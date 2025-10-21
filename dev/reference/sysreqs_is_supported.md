# Check if a platform has system requirements support

Check if a platform has system requirements support

## Usage

``` r
sysreqs_is_supported(sysreqs_platform = NULL)
```

## Arguments

- sysreqs_platform:

  System requirements platform. If `NULL`, then the `sysreqs_platform`
  [configuration
  option](https://pak.r-lib.org/dev/reference/pak-config.md) is used,
  which defaults to the current platform. Set this option if pak does
  not detect your platform correctly.

## Value

Logical scalar.

## See also

The `sysreqs_platform` [configuration
option](https://pak.r-lib.org/dev/reference/pak-config.md) .

Other system requirements functions:
[`pkg_sysreqs()`](https://pak.r-lib.org/dev/reference/pkg_sysreqs.md),
[`sysreqs_check_installed()`](https://pak.r-lib.org/dev/reference/sysreqs_check_installed.md),
[`sysreqs_db_list()`](https://pak.r-lib.org/dev/reference/sysreqs_db_list.md),
[`sysreqs_db_match()`](https://pak.r-lib.org/dev/reference/sysreqs_db_match.md),
[`sysreqs_db_update()`](https://pak.r-lib.org/dev/reference/sysreqs_db_update.md),
[`sysreqs_list_system_packages()`](https://pak.r-lib.org/dev/reference/sysreqs_list_system_packages.md),
[`sysreqs_platforms()`](https://pak.r-lib.org/dev/reference/sysreqs_platforms.md)

## Examples

``` r
sysreqs_is_supported()
#> [1] TRUE
```
