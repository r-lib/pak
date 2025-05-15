# nocov start

#' @description
#' pkgdepends is a toolkit for package dependencies, downloads and
#' installations, to be used in other packages. If you are looking for a
#' package manager, see [pak](https://github.com/r-lib/pak).
#'
#' @useDynLib pkgdepends, .registration = TRUE, .fixes = "c_"
#' @includeRmd tools/doc/README-body.Rmd
"_PACKAGE"

fix_check <- function() {
  R6::R6Class
  processx::process
  invisible()
}

# nocov end
