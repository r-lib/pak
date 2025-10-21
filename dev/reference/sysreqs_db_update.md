# Update the cached copy of the system requirements database

Update the cached copy of the system requirements database

## Usage

``` r
sysreqs_db_update()
```

## Details

If the the cached copy is recent, then no update is attempted. See the
`metadata_update_after` [configuration
option](https://pak.r-lib.org/dev/reference/pak-config.md) .

## See also

Other system requirements functions:
[`pkg_sysreqs()`](https://pak.r-lib.org/dev/reference/pkg_sysreqs.md),
[`sysreqs_check_installed()`](https://pak.r-lib.org/dev/reference/sysreqs_check_installed.md),
[`sysreqs_db_list()`](https://pak.r-lib.org/dev/reference/sysreqs_db_list.md),
[`sysreqs_db_match()`](https://pak.r-lib.org/dev/reference/sysreqs_db_match.md),
[`sysreqs_is_supported()`](https://pak.r-lib.org/dev/reference/sysreqs_is_supported.md),
[`sysreqs_list_system_packages()`](https://pak.r-lib.org/dev/reference/sysreqs_list_system_packages.md),
[`sysreqs_platforms()`](https://pak.r-lib.org/dev/reference/sysreqs_platforms.md)
