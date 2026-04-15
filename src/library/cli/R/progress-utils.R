#' @title Progress bar utility functions.
#' @details `cli_progress_num()` returns the number of currently
#' active progress bars. (These do not currently include the progress
#' bars created in C/C++ code.)
#' @return `cli_progress_num()` returns an integer scalar.
#'
#' @rdname progress-utils
#' @family progress bar functions
#' @export

cli_progress_num <- function() {
  length(clienv$progress)
}

#' @details `cli_progress_cleanup()` terminates all active progress bars.
#' (It currently ignores progress bars created in the C/C++ code.)
#' @return `cli_progress_cleanup() does not return anything.
#'
#' @rdname progress-utils
#' @export

cli_progress_cleanup <- function() {
  while ((n <- cli_progress_num()) > 0) {
    cli_progress_done(clienv$progress[[n]]$id)
  }
  ansi_show_cursor()
  invisible()
}

should_run_progress_examples <- function() {
  if (is_rcmd_check()) return(FALSE)
  tolower(Sys.getenv("R_PROGRESS_NO_EXAMPLES")) != "true"
}

is_rcmd_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}
