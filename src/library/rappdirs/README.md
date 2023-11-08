
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rappdirs

<!-- badges: start -->

[![R-CMD-check](https://github.com/r-lib/rappdirs/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/rappdirs/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/rappdirs)](https://CRAN.R-project.org/package=rappdirs)

<!-- badges: end -->

`rappdirs` is a port of
[appdirs](https://github.com/ActiveState/appdirs) to R. It lets you find
the appropriate directory to save caches, logs, and data, on Linux, Mac,
and Windows. It allows you to store files that need to shared across R
sessions in a way that aligns with the [CRAN
policies](https://cran.r-project.org/web/packages/policies.html).

## Motivation

What directory should your app use for storing user data? If running on
Mac OS X, you should use:

    ~/Library/Application Support/<AppName>

If on Windows (at least English Win XP) that should be:

    C:\Documents and Settings\<User>\Application Data\Local Settings\<AppAuthor>\<AppName>

or possibly:

    C:\Documents and Settings\<User>\Application Data\<AppAuthor>\<AppName>

for [roaming
profiles](https://docs.microsoft.com/en-us/previous-versions/windows/it-pro/windows-vista/cc766489(v=ws.10))
but that is another story.

On Linux (and other Unices) the dir, according to the [XDG
spec](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
(and subject to some interpretation), is either:

    ~/.config/<AppName>     

or possibly:

    ~/.local/share/<AppName>

## Usage

This kind of thing is what rappdirs is for. rappdirs will help you
choose an appropriate:

-   user data dir (`user_data_dir()`)
-   user config dir (`user_config_dir()`)
-   user cache dir (`user_cache_dir()`)
-   site data dir (`site_data_dir()`)
-   user log dir (`user_log_dir()`)

For example, on Mac:

``` r
library(rappdirs)
appname <- "SuperApp"
appauthor <- "Acme"
user_config_dir(appname, appauthor)
#> [1] "~/Library/Application Support/SuperApp"
user_data_dir(appname, appauthor)
#> [1] "~/Library/Application Support/SuperApp"
site_data_dir(appname, appauthor)
#> [1] "/Library/Application Support/SuperApp"
user_cache_dir(appname, appauthor)
#> [1] "~/Library/Caches/SuperApp"
user_log_dir(appname, appauthor)
#> [1] "~/Library/Logs/SuperApp"
```
