
pkgdown_url <- function() {
  ver <- utils::packageVersion(.packageName)
  paste0(
    "https://r-lib.github.io/pkgcache/",
    if (!is.na(ver[,4])) "dev/"
  )
}
