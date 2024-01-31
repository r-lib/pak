# -------------------------------------------------------------------------
# Dummy CRAN app

cran_app <- function(...) {
  asNamespace("pkgcache")$cran_app(...)
}

dcf <- function(...) {
  asNamespace("pkgcache")$dcf(...)
}

cran_app_pkgs <- dcf("
  Package: pkg1
  Version: 1.0.0

  Package: pkg1
  Version: 0.9.0

  Package: pkg1
  Version: 0.8.0

  Package: pkg2
  Version: 1.0.0
  Depends: pkg1

  Package: pkg3
  Version: 1.0.0
  Depends: pkg2

  Package: pkg3
  Version: 0.9.9

  Package: pkg4
  Version: 1.0.0
  Imports: pkg2
  Suggests: pkg3

  Package: crayon
  Version: 1.0.0

  Package: needspak
  Imports: pak

  Package: pak

  Package: futurama
  Depends: R (>= 3000.0)

  Package: needsfuturama
  Imports: futurama

  Package: dplyr
  Imports: tibble
  Suggests: testthat

  Package: tibble

  Package: testthat

  Package: curl
  SystemRequirements: libcurl: libcurl-devel (rpm) or libcurl4-openssl-dev (deb).
")

cran_app_pkgs2 <- dcf("
Package: pkgx
Version: 1.0.0
")
