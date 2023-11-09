#' File paths relative to the root of a directory hierarchy
#'
#' `find_root_file()` is a wrapper around [find_root()] that
#' appends an arbitrary number of path components to the root using
#' [base::file.path()].
#'
#' This function operates on the notion of relative paths.
#' The `...` argument is expected to contain a path relative to the root.
#' If the first path component passed to `...` is already an absolute path,
#' the `criterion` and `path` arguments are ignored,
#' and `...` is forwarded to [file.path()].
#'
#' @param criterion `[root_criterion]`\cr
#'   A criterion, one of the predefined [criteria]
#'   or created by [root_criterion()].
#'   Will be coerced using [as_root_criterion()].
#' @param path `[character(1)]`\cr
#'   The start directory.
#' @param ... `[character]`\cr
#'   Further path components passed to [file.path()].
#'   All arguments must be the same length or length one.
#' @return The normalized path of the root as specified by the search criteria,
#'   with the additional path components appended.
#'   Throws an error if no root is found.
#'
#' @examples
#' \dontrun{
#' find_package_root_file("tests", "testthat.R")
#' has_file("DESCRIPTION", "^Package: ")$find_file
#' has_file("DESCRIPTION", "^Package: ")$make_fix_file(".")
#' }
#'
#' @seealso [find_root()] [utils::glob2rx()] [base::file.path()]
#'
#' @export
find_root_file <- function(..., criterion, path = ".") {
  if (!missing(..1)) {
    abs <- is_absolute_path(..1)
    if (all(abs)) {
      return(path(...))
    }
    if (any(abs)) {
      stop("Combination of absolute and relative paths not supported.", call. = FALSE)
    }
  }

  root <- find_root(criterion = criterion, path = path)
  path(root, ...)
}
