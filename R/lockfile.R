
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
