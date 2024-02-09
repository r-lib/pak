bioc_app <- function(...) {
  asNamespace("pkgcache")$bioc_app(...)
}

dcf <- function(...) {
  asNamespace("pkgcache")$dcf(...)
}

bioc_app_pkgs <- dcf("
  Package: Biobase
  Version: 1.2.3
  Depends: R (>= 2.10), BiocGenerics(>= 0.27.1), utils
  Imports: methods
  Suggests: tools, tkWidgets, ALL, RUnit, golubEsets
")
