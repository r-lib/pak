#' @aliases pak-package NULL
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

gcov_flush <- function() {
  # nothing to do here, pak has no compiled code and we don't test
  # the shared libs loaded into the main process currently
}
