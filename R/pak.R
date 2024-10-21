
#' Install specified required packages
#'
#' Install the specified packages, or the ones required by
#' the package or project in the current working directory.
#'
#' This is a convenience function:
#' * If you want to install some packages, it is easier to type
#'   than [pak::pkg_install()].
#' * If you want to install all the packages that are needed
#'   for the development of a package or project, then it is
#'   easier to type than [pak::local_install_dev_deps()].
#' * You don't need to remember two functions to install
#'   packages, just one.
#'
#' @param pkg Package names or remote package specifications to install.
#'   See [pak package sources][Package sources] for details. If `NULL`,
#'   will install all development dependencies for the current package, if any.
#'   If there is no current package (i.e. `DESCRIPTION` file), then it will
#'   auto-scan package dependencies from R code in `root`.
#' @param root If `pkg` is `NULL`, then this directory is used as
#'   the package or project root.
#' @param ... Extra arguments are passed to [pkg_install()] or
#'   [local_install_dev_deps()].
#'
#' @export
#' @family package functions
#' @family local package trees

pak <- function(pkg = NULL, root = ".", ...) {
  if (is.null(pkg)) {
    if (file.exists(file.path(root, "DESCRIPTION"))) {
      local_install_dev_deps(...)
    } else {
      pkg_install("deps::.", ...)
    }
  } else {
    pkg_install(pkg, ...)
  }
}
