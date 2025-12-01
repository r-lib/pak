# Install all optional dependencies of pak

These packages are not required for any pak functionality. They are
recommended for some functions that return values that are best used
with these packages. E.g. many functions return data frames, which print
nicer when the pillar package is available.

## Usage

``` r
pak_install_extra(upgrade = FALSE)
```

## Arguments

- upgrade:

  Whether to install or upgrade to the latest versions of the optional
  packages.

## Details

Currently only one package is optional: **pillar**.
