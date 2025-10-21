# Update pak itself

Use this function to update the released or development version of pak.

## Usage

``` r
pak_update(force = FALSE, stream = c("auto", "stable", "rc", "devel"))
```

## Arguments

- force:

  Whether to force an update, even if no newer version is available.

- stream:

  Whether to update to the

  - `"stable"`,

  - `"rc"` (release candidate) or

  - `"devel"` (development) version.

  - `"auto"` updates to the same stream as the current one.

  Often there is no release candidate version, then `"rc"` also installs
  the stable version.

## Value

Nothing.
