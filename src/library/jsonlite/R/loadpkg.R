loadpkg <- function(pkg) {
  tryCatch(getNamespace(pkg), error = function(e) {
    stop("Required package ", pkg, " not found. Please run: install.packages('", pkg, "')", call. = FALSE)
  })
}
