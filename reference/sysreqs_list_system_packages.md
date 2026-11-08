# List installed system packages

List installed system packages

## Usage

``` r
sysreqs_list_system_packages()
```

## Value

Data frame with columns:

- `status`. two or three characters, the notation of `dpkg` on Debian
  based systems. `"ii"` means the package is correctly installed. On
  `RPM` based systems it is always `"ii"` currently.

- `package`: name of the system package.

- `version`: installed version of the system package.

- `capabilities`: list column of character vectors, the capabilities
  provided by the package.

## Details

This function uses the `sysreqs_platform` configuration option, see
[Configuration](https://pak.r-lib.org/reference/pak-config.md) . Set
this if pak does not detect your platform correctly.

## See also

Other system requirements functions:
[`pkg_sysreqs()`](https://pak.r-lib.org/reference/pkg_sysreqs.md),
[`sysreqs_check_installed()`](https://pak.r-lib.org/reference/sysreqs_check_installed.md),
[`sysreqs_db_list()`](https://pak.r-lib.org/reference/sysreqs_db_list.md),
[`sysreqs_db_match()`](https://pak.r-lib.org/reference/sysreqs_db_match.md),
[`sysreqs_db_update()`](https://pak.r-lib.org/reference/sysreqs_db_update.md),
[`sysreqs_is_supported()`](https://pak.r-lib.org/reference/sysreqs_is_supported.md),
[`sysreqs_platforms()`](https://pak.r-lib.org/reference/sysreqs_platforms.md)

## Examples

``` r
sysreqs_list_system_packages()[1:10,]
#> # A data frame: 10 × 4
#>    status package                  version         provides 
#>  * <chr>  <chr>                    <chr>           <list>   
#>  1 ii     3cpio                    0.14.0-1ubuntu1 <chr [0]>
#>  2 ii     7zip                     26.00+dfsg-1    <chr [2]>
#>  3 ii     7zip-rar                 26.00+ds-1      <chr [0]>
#>  4 ii     aardvark-dns             1.16.0-3        <chr [0]>
#>  5 ii     acl                      2.3.2-2         <chr [0]>
#>  6 ii     adduser                  3.153ubuntu1    <chr [0]>
#>  7 ii     adoptium-ca-certificates 1.0.6-1         <chr [0]>
#>  8 ii     adwaita-icon-theme       50.0-1          <chr [2]>
#>  9 ii     ant                      1.10.15-1build1 <chr [0]>
#> 10 ii     ant-optional             1.10.15-1build1 <chr [0]>
```
