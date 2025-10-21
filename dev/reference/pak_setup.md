# Set up private pak library (deprecated)

This function is deprecated and does nothing. Recent versions of pak do
not need a `pak_setup()` call.

## Usage

``` r
pak_setup(mode = c("auto", "download", "copy"), quiet = FALSE)
```

## Arguments

- mode:

  Where to get the packages from. "download" will try to download them
  from CRAN. "copy" will try to copy them from your current "regular"
  package library. "auto" will try to copy first, and if that fails,
  then it tries to download.

- quiet:

  Whether to omit messages.

## Value

The path to the private library, invisibly.
