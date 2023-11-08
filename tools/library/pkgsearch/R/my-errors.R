
new_no_package_error <- function(...) {
  cnd <- new_error(...)
  class(cnd) <- c("package_not_found_error", class(cnd))
  cnd
}
