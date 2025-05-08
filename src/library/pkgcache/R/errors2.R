new_pkgcache_cond <- function(..., call. = FALSE, class = NULL, data = NULL) {
  cnd <- new_cond(..., call. = call.)
  cnd[names(data)] <- data
  class(cnd) <- c(class, "pkgcache_condition", class(cnd))
  cnd
}

new_pkgcache_warning <- function(
  ...,
  call. = FALSE,
  class = NULL,
  data = NULL
) {
  cnd <- new_cond(..., call. = call.)
  cnd[names(data)] <- data
  class(cnd) <- c(class, "pkgcache_condition", "warning", class(cnd))
  cnd
}
