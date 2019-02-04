
#' Local package trees
#'
#' pak can install packages from local package trees. This is convenient
#' for package development. See the following functions:
#' * [local_install()] installs a package from a package tree and all of its
#'   (hard) dependencies (i.e. `Includes`, `Depends`, `LinkingTo`.
#' * [local_install_deps()] installs all hard dependencies of a package.
#' * [local_install_dev_deps()] installs all hard and soft dependencies
#'   of a package. This function is intended for active package development.
#'
#' Note that the last two functions do not install the package in the
#' specified package tree itself, only its dependencies. This is convenient
#' if the package itself is loaded via some other means, e.g.
#' `devtools::load_all()`, for development.
#'
#' @name local_package_trees
#' @family local package trees
 NULL

#' Install a package tree
#'
#' Installs a package tree (or source package file), together with its
#' dependencies.
#'
#' `local_install()` is equivalent to `pkg_install("local::.")`.
#'
#' @param root Path to the package tree.
#' @inheritParams pkg_install
#' @return Data frame, with information about the installed package(s).
#'
#' @family local package trees
#' @export

local_install <- function(root = ".", lib = .libPaths()[1], upgrade = FALSE,
                          ask = interactive()) {
  pkg_install(paste0("local::", root), lib = lib, upgrade = upgrade,
              ask = ask)
}

#' Install the dependencies of a package tree
#'
#' Installs the hard dependencies of a package tree (or source package file),
#' without installing the package tree itself.
#'
#' Note that development (and optional) dependencies, under `Suggests` in
#' `DESCRIPTION`, are not installed. If you want to install them as well,
#' use [local_install_dev_deps()].
#'
#' @inheritParams local_install
#' @return Data frame, with information about the installed package(s).
#'
#' @family local package trees
#' @export

local_install_deps <- function(root = ".", lib = .libPaths()[1],
                               upgrade = FALSE, ask = interactive()) {
  pkg_install(paste0("deps::", root), lib = lib, upgrade = upgrade,
              ask = ask)
}

#' Install all dependencies of a package tree
#'
#' Installs all dependencies of a package tree (or source package file),
#' without installing the package tree itself. It installs the development
#' dependencies as well, specified in the `Suggests` field of
#' `DESCRIPTION`.
#'
#' @inheritParams local_install
#' 
#' @family local package trees
#' @export

local_install_dev_deps <- function(root = ".", lib = .libPaths()[1],
                                   upgrade = FALSE, ask = interactive()) {
  start <- Sys.time()

  any <- remote(
    function(...) {
      get("local_install_dev_deps_make_plan", asNamespace("pak"))(...)
    },
    list(root = root, lib = lib, upgrade = upgrade, start = start))

  if (any && ask) get_confirmation("? Do you want to continue (Y/n) ")

  inst <- remote(
    function(...) {
      get("local_install_dev_deps_do_plan", asNamespace("pak"))(...)
    },
    list(lib = lib))

  invisible(inst)
}

## ----------------------------------------------------------------------

## Almost the same as a "regular" install, but need to set dependencies

local_install_dev_deps_make_plan <- function(root, lib, upgrade, start) {
  r <- remotes()$new(
    paste0("deps::", root), library = lib,
    config = list(dependencies = TRUE))

  policy <- if (upgrade) "upgrade" else "lazy"
  r$solve(policy = policy)
  r$stop_for_solve_error()
  pkg_data$tmp <- list(remotes = r, start = start)
  sol <- r$get_solution()$data
  print_install_details(sol, lib)
}

## This is the same as a regular install

local_install_dev_deps_do_plan <- function(lib) {
  pkg_install_do_plan(remotes = NULL, lib = lib)
}
