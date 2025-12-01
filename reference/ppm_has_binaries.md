# Does PPM build binary packages for the current platform?

Does PPM build binary packages for the current platform?

## Usage

``` r
ppm_has_binaries()
```

## Value

`TRUE` or `FALSE`.

## See also

The 'pkgcache and Posit Package Manager on Linux' article at
<https://r-lib.github.io/pkgcache/>.

Other PPM functions:
[`ppm_platforms()`](https://pak.r-lib.org/reference/ppm_platforms.md),
[`ppm_r_versions()`](https://pak.r-lib.org/reference/ppm_r_versions.md),
[`ppm_repo_url()`](https://pak.r-lib.org/reference/ppm_repo_url.md),
[`ppm_snapshots()`](https://pak.r-lib.org/reference/ppm_snapshots.md)

## Examples

``` r
if (FALSE) {
system_r_platform()
ppm_has_binaries()
}
```
