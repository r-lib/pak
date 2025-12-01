# List platforms with system requirements support

List platforms with system requirements support

## Usage

``` r
sysreqs_platforms()
```

## Value

Data frame with columns:

- `name`: human readable OS name.

- `os`: OS name, e.g. `linux`.

- `distribution`: OS id, e.g. `ubuntu` or `redhat`.

- `version`: distribution version. A star means that all versions are
  supported, that are also supported by the vendor.

- `update_command`: command to run to update the system package
  metadata.

- `install_command`: command to run to install packages.

- `query_command`: name of the tool to use to query system package
  information.

## See also

Other system requirements functions:
[`pkg_sysreqs()`](https://pak.r-lib.org/reference/pkg_sysreqs.md),
[`sysreqs_check_installed()`](https://pak.r-lib.org/reference/sysreqs_check_installed.md),
[`sysreqs_db_list()`](https://pak.r-lib.org/reference/sysreqs_db_list.md),
[`sysreqs_db_match()`](https://pak.r-lib.org/reference/sysreqs_db_match.md),
[`sysreqs_db_update()`](https://pak.r-lib.org/reference/sysreqs_db_update.md),
[`sysreqs_is_supported()`](https://pak.r-lib.org/reference/sysreqs_is_supported.md),
[`sysreqs_list_system_packages()`](https://pak.r-lib.org/reference/sysreqs_list_system_packages.md)

## Examples

``` r
sysreqs_platforms()
#> # A data frame: 19 × 9
#>    name   os    id    distribution version version_match update_command
#>    <chr>  <chr> <chr> <chr>        <chr>   <chr>         <chr>         
#>  1 Ubunt… linux ubun… ubuntu       *       NA            apt-get -y up…
#>  2 Debia… linux debi… debian       *       NA            apt-get -y up…
#>  3 CentO… linux cent… centos       *       major         NA            
#>  4 Rocky… linux rocky rockylinux   *       major         NA            
#>  5 Rocky… linux rock… rockylinux   *       major         NA            
#>  6 AlmaL… linux alma… rockylinux   *       major         NA            
#>  7 Red H… linux rhel  redhat       6       major         NA            
#>  8 Red H… linux rhel  redhat       7       major         NA            
#>  9 Red H… linux rhel  redhat       *       major         NA            
#> 10 Red H… linux redh… redhat       6       major         NA            
#> 11 Red H… linux redh… redhat       7       major         NA            
#> 12 Red H… linux redh… redhat       *       major         NA            
#> 13 Fedor… linux fedo… fedora       *       NA            NA            
#> 14 openS… linux open… opensuse     *       NA            NA            
#> 15 openS… linux open… opensuse     *       NA            NA            
#> 16 openS… linux open… opensuse     *       NA            NA            
#> 17 SUSE … linux sles  sle          *       NA            NA            
#> 18 SUSE … linux sle   sle          *       NA            NA            
#> 19 Alpin… linux alpi… alpine       *       minor         NA            
#> # ℹ 2 more variables: install_command <chr>, query_command <chr>
```
