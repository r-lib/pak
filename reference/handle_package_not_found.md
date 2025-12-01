# Install missing packages on the fly

Use this function to set up a global error handler, that is called if R
fails to load a package. This handler will offer you the choice of
installing the missing package (and all its dependencies), and in some
cases it can also remedy the error and restart the code.

## Usage

``` r
handle_package_not_found(err)
```

## Arguments

- err:

  The error object, of class `packageNotFoundError`.

## Value

Nothing.

## Details

You are not supposed to call this function directly. Instead, set it up
as a global error handler, possibly in your `.Rprofile` file:

    if (interactive() && getRversion() >= "4.0.0") {
      globalCallingHandlers(
        packageNotFoundError = function(err) {
          try(pak::handle_package_not_found(err))
        }
      )
    }

Global error handlers are only supported in R 4.0.0 and later.

Currently `handle_package_not_found()` does not do anything in
non-interactive mode (including in knitr, testthat and RStudio
notebooks), this might change in the future.

In some cases it is possible to remedy the original computation that
tried to load the missing package, and pak will offer you to do so after
a successful installation. Currently, in R 4.0.4, it is not possible to
continue a failed [`library()`](https://rdrr.io/r/base/library.html)
call.
