
#' List of all supported signals
#'
#' Only the signals supported by the current platform are included.
#' @return List of integers, named by signal names.
#' 
#' @export

signals <- function() {
  as.list(ps_env$constants$signals)
}

get_terminal_map <- function() {
  ls <- c(
    dir("/dev", pattern = "^tty.*", full.names = TRUE),
    dir("/dev/pts", full.names = TRUE))
  ret <- structure(ls, names = as.character(.Call(psp__stat_st_rdev, ls)))
  ret[names(ret) != "0"]
}
