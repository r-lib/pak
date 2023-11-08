
# distro

<!-- badges: start -->
[![R build status](https://github.com/nealrichardson/distro/workflows/R-CMD-check/badge.svg)](https://github.com/nealrichardson/distro/actions)
<!-- badges: end -->

The goal of `distro` is to provide a standardized interface to version and other
facts about the current system's Linux distribution. It is similar in spirit
(though far more limited in scope) to the Python
[`distro`](https://distro.readthedocs.io/en/latest/) package.

Different Linux distributions and versions record version information in a
number of different files and commands. The `lsb_release` command line utility
standardizes some of the access to this information, but it is not guaranteed
to be installed. This package draws from the various possible locations of
version information and provides a single function for querying them.

## Installation

To install `distro` from CRAN,

```r
install.packages("distro")
```

You can install a development version with:

``` r
remotes::install_github("nealrichardson/distro")
```

## Example

There is only one public function in the package:

```r
distro::distro()

# $id
# [1] "ubuntu"
#
# $version
# [1] "16.04"
#
# $codename
# [1] "xenial"
#
# $short_version
# [1] "16.04"
```

## Contributing

Does `distro` fail to produce the expected result on your system? We've tried to
make it easy to extend the tests to accommodate new distributions and ways of
expressing distribution information. That way, you can add information from your
system to the tests as a way of setting up a minimum reproducible example.

* If your system has `lsb_release` installed, see `tests/test-lsb-release.R` for
how to record the results of the command with different flags.
* If your system does not have `lsb_release`, you probably have an
`/etc/os-release` file. Copy the contents of your `/etc/os-release` to the
`tests/os-release` directory and we can set up a test using that.
* If your system has neither of those but has an `/etc/system-release` file, see
`tests/test-system-release.R` for how to provide the contents of that file in a
test
* If your system has none of these, please open an issue!
