# List all available Posit Package Manager (PPM) snapshots

List all available Posit Package Manager (PPM) snapshots

## Usage

``` r
ppm_snapshots()
```

## Value

Data frame with two columns:

- `date`: the time the snapshot was taken, a `POSIXct` vector,

- `id`: integer id of the snapshot, this can be used in the repository
  URL.

## Details

The repository URL of a snapshot has the following form on Windows:

    {base}/{repo}/{id}

where `{base}` is the base URL for PPM (see
[`ppm_repo_url()`](https://pak.r-lib.org/dev/reference/ppm_repo_url.md))
and `{id}` is either the date or id of the snapshot, or `latest` for the
latest snapshot. E.g. these are equivalent:

    https://packagemanager.posit.co/cran/5
    https://packagemanager.posit.co/cran/2017-10-10

On a Linux distribution that has PPM support, the repository URL that
contains the binary packages looks like this:

    {base}/{repo}/__linux__/{binary_url}/{id}

where `{id}` is as before, and `{binary_url}` is a code name for a
release of a supported Linux distribution. See the `binary_url` column
of the result of
[`ppm_platforms()`](https://pak.r-lib.org/dev/reference/ppm_platforms.md)
for these code names.

## See also

The 'pkgcache and Posit Package Manager on Linux' article at
<https://r-lib.github.io/pkgcache/>.

Other PPM functions:
[`ppm_has_binaries()`](https://pak.r-lib.org/dev/reference/ppm_has_binaries.md),
[`ppm_platforms()`](https://pak.r-lib.org/dev/reference/ppm_platforms.md),
[`ppm_r_versions()`](https://pak.r-lib.org/dev/reference/ppm_r_versions.md),
[`ppm_repo_url()`](https://pak.r-lib.org/dev/reference/ppm_repo_url.md)

## Examples

``` r
if (FALSE) {
ppm_snapshots()
}
```
