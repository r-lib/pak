# Clean up pak caches

Clean up pak caches

## Usage

``` r
pak_cleanup(
  package_cache = TRUE,
  metadata_cache = TRUE,
  pak_lib = TRUE,
  force = FALSE
)
```

## Arguments

- package_cache:

  Whether to clean up the cache of package files.

- metadata_cache:

  Whether to clean up the cache of package meta data.

- pak_lib:

  This argument is now deprecated and does nothing.

- force:

  Do not ask for confirmation. Note that to use this function in
  non-interactive mode, you have to specify `force = TRUE`.

## See also

Other pak housekeeping:
[`pak_sitrep()`](https://pak.r-lib.org/reference/pak_sitrep.md)
