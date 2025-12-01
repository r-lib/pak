# List all platforms supported by Posit Package Manager (PPM)

List all platforms supported by Posit Package Manager (PPM)

## Usage

``` r
ppm_platforms()
```

## Value

Data frame with columns:

- `name`: platform name, this is essentially an identifier,

- `os`: operating system, `linux`, `windows` or `macOS` currently,

- `binary_url`: the URL segment of the binary repository URL of this
  platform, see
  [`ppm_snapshots()`](https://pak.r-lib.org/reference/ppm_snapshots.md).

- `distribution`: for Linux platforms the name of the distribution,

- `release`: for Linux platforms, the name of the release,

- `binaries`: whether PPM builds binaries for this platform.

## See also

The 'pkgcache and Posit Package Manager on Linux' article at
<https://r-lib.github.io/pkgcache/>.

Other PPM functions:
[`ppm_has_binaries()`](https://pak.r-lib.org/reference/ppm_has_binaries.md),
[`ppm_r_versions()`](https://pak.r-lib.org/reference/ppm_r_versions.md),
[`ppm_repo_url()`](https://pak.r-lib.org/reference/ppm_repo_url.md),
[`ppm_snapshots()`](https://pak.r-lib.org/reference/ppm_snapshots.md)

## Examples

``` r
if (FALSE) {
ppm_platforms()
}
```
