
is_true_check_env_var <- function(x, default = "") {
  # like utils:::str2logical
  val <- Sys.getenv(x, default)
  if (isTRUE(as.logical(val))) return(TRUE)
  tolower(val) %in% c("1", "yes")
}

isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

is_false_check_env_var <- function(x, default = "") {
  # like utils:::str2logical
  val <- Sys.getenv(x, default)
  if (isFALSE(as.logical(val))) return(TRUE)
  tolower(val) %in% c("0", "no")
}

# Only skip if _R_CHECK_FORCE_SUGGESTS_ is false

skip_if_not_installed <- function(pkg) {
  if (!is_false_check_env_var("_R_CHECK_FORCE_SUGGESTS_")) return()
  testthat::skip_if_not_installed(pkg)
}
