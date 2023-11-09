cli
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

> Helpers for Developing Command Line Interfaces

<!-- badges: start -->

[![R-CMD-check](https://github.com/r-lib/cli/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/cli/actions/workflows/R-CMD-check.yaml)
[![](https://www.r-pkg.org/badges/version/cli)](https://www.r-pkg.org/pkg/cli)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/cli)](https://www.r-pkg.org/pkg/cli)
[![Codecov test
coverage](https://codecov.io/gh/r-lib/cli/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/cli?branch=main)
<!-- badges: end -->

A suite of tools to build attractive command line interfaces (CLIs),
from semantic elements: headers, lists, alerts, paragraphs, etc.
Supports theming via a CSS-like language. It also contains a number of
lower level CLI elements: rules, boxes, trees, and Unicode symbols with
ASCII alternatives. It supports ANSI markup for terminal colors and font
styles.

------------------------------------------------------------------------

# Features

- Build a CLI using semantic elements: headings, lists, alerts,
  paragraphs.
- Theming via a CSS-like language.
- Terminal colors and font styles.
- All cli text can contain interpreted string literals, via the
  [glue](https://github.com/tidyverse/glue) package.
- Progress bars from R and C code.
- Error and warning messages with rich text formatting.
- Support for pluralized messages.
- ANSI styled string manipulation.

# Installation

Install the stable version from CRAN:

``` r
install.packages("cli")
```

# Short tour

Some of the more commonly used cli elements, and features.

## Short alert messages

One liner messages to inform or warn.

``` r
pkgs <- c("foo", "bar", "foobar")
cli_alert_success("Downloaded {length(pkgs)} packages.")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/alert-success-dark.svg">
<img src="man/figures/README/alert-success.svg" width="100%" />
</picture>

``` r
db_url <- "example.com:port"
cli_alert_info("Reopened database {.url {db_url}}.")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/alert-info-dark.svg">
<img src="man/figures/README/alert-info.svg" width="100%" /> </picture>

``` r
cli_alert_warning("Cannot reach GitHub, using local database cache.")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/alert-warning-dark.svg">
<img src="man/figures/README/alert-warning.svg" width="100%" />
</picture>

``` r
cli_alert_danger("Failed to connect to database.")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/alert-danger-dark.svg">
<img src="man/figures/README/alert-danger.svg" width="100%" />
</picture>

``` r
cli_alert("A generic alert")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/alert-dark.svg">
<img src="man/figures/README/alert.svg" width="100%" /> </picture>

## Headings

Three levels of headings.

``` r
cli_h1("Heading 1")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/h1-dark.svg">
<img src="man/figures/README/h1.svg" width="100%" /> </picture>

``` r
cli_h2("Heading 2")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/h2-dark.svg">
<img src="man/figures/README/h2.svg" width="100%" /> </picture>

``` r
cli_h3("Heading 3")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/h3-dark.svg">
<img src="man/figures/README/h3.svg" width="100%" /> </picture>

## Lists

Ordered, unordered and description lists, that can be nested.

``` r
fun <- function() {
  cli_ol()
  cli_li("Item 1")
  ulid <- cli_ul()
  cli_li("Subitem 1")
  cli_li("Subitem 2")
  cli_end(ulid)
  cli_li("Item 2")
  cli_end()
}
fun()
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/lists-dark.svg">
<img src="man/figures/README/lists.svg" width="100%" /> </picture>

## Themes

Theming via a CSS-like language.

``` r
fun <- function() {
  cli_div(theme = list(span.emph = list(color = "orange")))
  cli_text("This is very {.emph important}")
  cli_end()
  cli_text("Back to the {.emph previous theme}")
}
fun()
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/themes-dark.svg">
<img src="man/figures/README/themes.svg" width="100%" /> </picture>

## Command substitution

Automatic command substitution via the
[glue](https://github.com/tidyverse/glue) package.

``` r
size <- 123143123
dt <- 1.3454
cli_alert_info(c(
  "Downloaded {prettyunits::pretty_bytes(size)} in ",
  "{prettyunits::pretty_sec(dt)}"))
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/glue-dark.svg">
<img src="man/figures/README/glue.svg" width="100%" /> </picture>

## Pluralization

Pluralization support.

``` r
nfiles <- 3
ndirs <- 1
cli_alert_info("Found {nfiles} file{?s} and {ndirs} director{?y/ies}.")
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/plurals-dark.svg">
<img src="man/figures/README/plurals.svg" width="100%" /> </picture>

## Progress bars

``` r
clean <- function() {
  cli_progress_bar("Cleaning data", total = 100)
  for (i in 1:100) {
    Sys.sleep(5/100)
    cli_progress_update()
  }
}
clean()
```

<picture>
<source media="(prefers-color-scheme: dark)" srcset="man/figures/README/progress-dark.svg">
<img src="man/figures/README/progress.svg" width="100%" /> </picture>

# Documentation

See at
[`https://cli.r-lib.org/`](https://cli.r-lib.org/reference/index.html)
and also in the installed package: `help(package = "cli")`.

# Code of Conduct

Please note that the cli project is released with a [Contributor Code of
Conduct](https://cli.r-lib.org/dev/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

# License

MIT © RStudio
