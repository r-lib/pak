
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

local_install <- function(root = ".", lib = .libPaths()[1], upgrade = TRUE,
                          ask = interactive(), dependencies = NA) {

  start <- Sys.time()

  status <- remote(
    function(...) get("local_install_make_plan", asNamespace("pak"))(...),
    list("local", root = root, lib = lib, upgrade = upgrade, ask = ask,
         start = start, dependencies = dependencies,
         loaded = loaded_packages(lib)))

  unloaded <- handle_status(status, lib, ask)$unloaded

  inst <- remote(
    function(...) get("pkg_install_do_plan", asNamespace("pak"))(...),
    list(proposal = NULL, lib = lib))

  if (length(unloaded) > 0) offer_restart(unloaded)

  invisible(inst)
}

local_install_make_plan <- function(type, root, lib, upgrade, ask, start,
                                    dependencies, loaded) {
  root <- rprojroot::find_package_root_file(path = root)
  pkg <- paste0(type, "::", root)
  pkg_install_make_plan(pkg, lib, upgrade, ask, start, dependencies, loaded)
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
                               upgrade = TRUE, ask = interactive(),
                               dependencies = NA) {
  start <- Sys.time()

  status <- remote(
    function(...) get("local_install_make_plan", asNamespace("pak"))(...),
    list("deps", root = root, lib = lib, upgrade = upgrade, ask = ask,
         start = start, dependencies = dependencies,
         loaded = loaded_packages(lib)))

  unloaded <- handle_status(status, lib, ask)$unloaded

  inst <- remote(
    function(...) get("pkg_install_do_plan", asNamespace("pak"))(...),
    list(proposal = NULL, lib = lib))

  if (length(unloaded) > 0) offer_restart(unloaded)

  invisible(inst)
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
                                   upgrade = TRUE, ask = interactive(),
                                   dependencies = TRUE) {
  start <- Sys.time()

  status <- remote(
    function(...) {
      get("local_install_dev_deps_make_plan", asNamespace("pak"))(...)
    },
    list(root = root, lib = lib, upgrade = upgrade, start = start,
         dependencies = dependencies, loaded = loaded_packages(lib)))

  unloaded <- handle_status(status, lib, ask)$unloaded

  inst <- remote(
    function(...) {
      get("local_install_dev_deps_do_plan", asNamespace("pak"))(...)
    },
    list(lib = lib))

  if (length(unloaded) > 0) offer_restart(unloaded)

  invisible(inst)
}

#' Dependencies of a package tree
#'
#' @param root Path to the package tree.
#' @param upgrade Whether to use the most recent available package
#'   versions.
#' @param dependencies Which dependencies to print. Defaults to the hard
#'   dependencies for `local_deps()` and `local_deps_tree()` and the hard
#'   dependencies plus the development dependencies for `local_dev_deps()`
#'   and `local_dev_deps_tree()`.
#' @return All of these functions return the dependencies in a data
#'   frame (tibble). `local_deps_tree()` and `local_dev_deps_tree()` also
#'   print the dependency tree.
#'
#' @family local package trees
#' @export

local_deps <- function(root = ".", upgrade = TRUE, dependencies = NA) {
  ref <- paste0("local::", root)
  pkg_deps(ref, upgrade = upgrade, dependencies = dependencies)
}

#' @export
#' @rdname local_deps

local_deps_tree <- function(root = ".", upgrade = TRUE, dependencies = NA) {
  ref <- paste0("local::", root)
  pkg_deps_tree(ref, upgrade = upgrade, dependencies = dependencies)
}

#' @export
#' @rdname local_deps

local_dev_deps <- function(root = ".", upgrade = TRUE, dependencies = TRUE) {
  local_deps(root, upgrade, dependencies)
}

#' @export
#' @rdname local_deps

local_dev_deps_tree <- function(root = ".", upgrade = TRUE, dependencies = TRUE) {
  local_deps_tree(root, upgrade, dependencies)
}

#' Explain dependencies of a package tree
#'
#' These functions are similar to [pkg_deps_explain()], but work on a
#' local package tree. `local_dev_deps_explain()` also includes development
#' dependencies.
#'
#' @param root Path to the package tree.
#' @param deps Package names of the dependencies to explain.
#' @param upgrade Whether to use the most recent available package
#'   versions.
#' @param dependencies Which dependencies to print. Defaults to the hard
#'   dependencies for `local_deps()` and `local_deps_tree()` and the hard
#'   dependencies plus the development dependencies for `local_dev_deps()`
#'   and `local_dev_deps_tree()`.
#'
#' @export
#' @family local package trees

local_deps_explain <- function(deps, root = ".", upgrade = TRUE,
                               dependencies = NA) {
  ref <- paste0("local::", root)
  pkg_deps_explain(ref, deps, upgrade, dependencies)
}

#' @export
#' @rdname local_deps_explain

local_dev_deps_explain <- function(deps, root = ".", upgrade = TRUE,
                                   dependencies = TRUE) {
  ref <- paste0("local::", root)
  pkg_deps_explain(ref, deps, upgrade, dependencies)
}

#' Create a lock file for the dependencies of a package tree
#'
#' The lock file can be used later, possibly in a new R session, to carry
#' out the installation of the dependencies, with
#' [local_install_lockfile()].
#'
#' Note, since the URLs of CRAN and most CRAN-like repositories change
#' over time, in practice you cannot use the lock file _much_ later.
#' For example, binary packages of older package version
#' might be deleted from the repository, breaking the URLs in the
#' lock file.
#'
#' Currently the intended use case of lock files in on CI systems, to
#' facilitate caching. The (hash of the) lock file provides a good key
#' for caching systems.
#'
#' @param lockfile Path to the lock file.
#' @param lib Library to base the lock file on. In most cases (e.g. on a
#'   CI system, or at deployment), this is an empty library. Supply
#'   `tempfile()` to make sure the lock file is based on an empty library.
#' @inheritParams local_install
#' @family lock files
#' @export

local_create_lockfile <- function(root = ".", lockfile = "pkg.lock",
                                  lib = .libPaths()[1], upgrade = TRUE,
                                  dependencies = TRUE) {
  ret <- remote(
    function(...) {
      get("local_create_lockfile_internal", asNamespace("pak"))(...)
    },
    list(root = root, lockfile = lockfile, lib = lib, upgrade = upgrade,
         dependencies = dependencies)
  )

  invisible(ret)
}

local_create_lockfile_internal <- function(root, lockfile, lib, upgrade,
                                           dependencies) {
  root <- rprojroot::find_package_root_file(path = root)
  prop <- pkgdepends::new_pkg_installation_proposal(
    paste0("deps::", root),
    config = list(library = lib, dependencies = dependencies)
  )
  prop$set_solve_policy(if (upgrade) "upgrade" else "lazy")
  prop$solve()
  prop$stop_for_solution_error()
  prop$create_lockfile(lockfile)
  invisible()
}

#' Install packages based on a lock file
#'
#' Install a lock file that was created with [local_create_lockfile()].
#'
#' @param lockfile Path to the lock file.
#' @param lib Library to carry out the installation on.
#' @family lock files
#' @export

local_install_lockfile <- function(lockfile = "pkg.lock",
                                   lib = .libPaths()[1]) {

  start <- Sys.time()
  ret <- remote(
    function(...) {
      get("local_install_lockfile_internal", asNamespace("pak"))(...)
    },
    list(lockfile = lockfile, lib = lib, start = start)
  )

  invisible(ret)
}

local_install_lockfile_internal <- function(lockfile, lib, start) {
  config <- list(library = lib)
  plan <- pkgdepends::new_pkg_installation_plan(lockfile, config = config)
  plan$download()
  inst <- plan$install()
  attr(inst, "total_time") <- Sys.time() - start
  class(inst) <- c("pkg_install_result", class(inst))

  ## Remove some largeish columns that we don't really need any more
  inst$extra <- NULL

  ## One line summary of the install
  print_install_summary(inst)

  inst
}

## ----------------------------------------------------------------------

## Almost the same as a "regular" install, but need to set dependencies

local_install_dev_deps_make_plan <- function(root, lib, upgrade, start,
                                             dependencies, loaded) {
  root <- rprojroot::find_package_root_file(path = root)
  prop <- pkgdepends::new_pkg_installation_proposal(
    paste0("deps::", root),
    config = list(library = lib, dependencies = dependencies)
  )

  prop$set_solve_policy(if (upgrade) "upgrade" else "lazy")
  prop$solve()
  prop$stop_for_solution_error()
  pkg_data$tmp <- list(proposal = prop, start = start)
  print_install_details(prop, lib, loaded)
}

## This is the same as a regular install

local_install_dev_deps_do_plan <- function(lib) {
  pkg_install_do_plan(proposal = NULL, lib = lib)
}
