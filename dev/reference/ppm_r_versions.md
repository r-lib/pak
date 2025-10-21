# List all R versions supported by Posit Package Manager (PPM)

List all R versions supported by Posit Package Manager (PPM)

## Usage

``` r
ppm_r_versions()
```

## Value

Data frame with columns:

- `r_version`: minor R versions, i.e. version numbers containing the
  first two components of R versions supported by this PPM instance.

## See also

The 'pkgcache and Posit Package Manager on Linux' article at
<https://r-lib.github.io/pkgcache/>.

Other PPM functions:
[`ppm_has_binaries()`](https://pak.r-lib.org/dev/reference/ppm_has_binaries.md),
[`ppm_platforms()`](https://pak.r-lib.org/dev/reference/ppm_platforms.md),
[`ppm_repo_url()`](https://pak.r-lib.org/dev/reference/ppm_repo_url.md),
[`ppm_snapshots()`](https://pak.r-lib.org/dev/reference/ppm_snapshots.md)

## Examples

``` r
if (FALSE) {
ppm_r_versions()
}
```
