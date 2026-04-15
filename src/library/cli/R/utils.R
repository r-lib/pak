is_yes <- function(x) {
  tolower(x) %in% c("true", "yes", "y", "t", "1")
}

format_iso_8601 <- function(p) {
  format(p, "%Y-%m-%dT%H:%M:%S+00:00")
}

has_packages <- function(pkgs) {
  all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE))
}

cli_escape <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  x <- gsub("}", "}}", x, fixed = TRUE)
  x
}

# missing from older R

isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

get_ppid <- function() {
  .Call(clic_getppid)
}
