
#' Install a package
#'
#' Install a package and it's dependencies.
#'
#' @param pkg Package names or remote package specifications to install.
#'   See [pkgdepends::remotes] for details about remote package
#'   specifications.
#' @param lib Package library to install the packages to.
#' @param upgrade Whether to upgrade already installed packages to the
#'   latest available version.
#' @param num_workers Number of worker processes to use.
#' @param ask Whether to ask for confirmation.
#'
#' @export

pkg_install <- function(pkg, lib = .libPaths()[[1L]], upgrade = FALSE,
                        num_workers = 1L, ask = interactive()) {

  start <- Sys.time()

  any <- remote(
    function(...) get("pkg_install_make_plan", asNamespace("pkgman"))(...),
    list(pkg = pkg, lib = lib, upgrade = upgrade, ask = ask, start = start))

  if (any && ask) {
    get_confirmation("? Do you want to continue (Y/n) ")
  }

  inst <- remote(
    function(...) get("pkg_install_do_plan", asNamespace("pkgman"))(...),
    list(remotes = NULL, lib = lib, num_workers = num_workers))

  invisible(inst)
}

pkg_install_make_plan <- function(pkg, lib, upgrade, ask, start) {
  r <- pkgdepends::remotes$new(pkg, library = lib)

  ## Solve the dependency graph
  policy <- if (upgrade) "upgrade" else "lazy"
  r$solve(policy = policy)
  r$stop_for_solve_error()
  pkgman_data$tmp <- list(remotes = r, start = start)
  sol <- r$get_solution()$data
  print_install_details(sol, lib)
}

pkg_install_do_plan <- function(remotes, lib, num_workers) {

  remotes <- remotes %||% pkgman_data$tmp$remotes
  start  <- pkgman_data$tmp$start
  pkgman_data$tmp <- NULL

  # Actually download packages as needed
  remotes$download_solution()
  remotes$stop_for_solution_download_error()

  # Get the installation plan and hand it over to pkginstall
  plan <- remotes$get_install_plan()
  inst <- pkginstall::install_package_plan(plan = plan, lib = lib,
                                           num_workers = num_workers)

  attr(inst, "total_time") <- Sys.time() - start
  class(inst) <- c("pkgman_install_result", class(inst))

  ## Remove some largeish columns that we don't really need any more
  inst$extra <- NULL

  ## One line summary of the install
  print_install_summary(inst)

  inst
}

#' Display installed locations of a package
#'
#' @param pkg Name of an installed package to display status for.
#' @param lib One or more library paths to lookup package status in.
#'
#' @export

pkg_status <- function(pkg, lib = .libPaths()) {
  stopifnot(length(pkg == 1) && is.character(pkg))

  remote(
    function(...) asNamespace("pkgman")$pkg_status_internal(...),
    list(pkg = pkg, lib = lib))
}

pkg_status_internal <- function(pkg, lib) {
  st <- lapply(lib, pkgdepends::lib_status, packages = pkg)
  do.call(rbind, st)
}

#' Remove installed packages
#'
#' @param pkg A character vector of packages to remove.
#' @param lib library to remove packages from
#' @export

pkg_remove <- function(pkg, lib = .libPaths()[[1L]]) {
  remote(
    function(...) {
      get("pkg_remove_internal", asNamespace("pkgman"))(...)
    },
    list(pkg = pkg, lib = lib)
  )
}

pkg_remove_internal <- function(pkg, lib) {
  pr <- pkgdepends::parse_remotes(pkg)[[1]]
  suppressMessages(utils::remove.packages(pr$package, lib))
  invisible(pr)
}

## TODO: pkg_check()
## Like lib_check(), but for a single package and its dependencies

## TODO: pkg_doctor()
## Like lib_doctor(), but for a single package and its dependencies

## TODO: pkg_update_status()
## Like lib_status(), but for a single package and its dependencies

## TODO: pkg_update()
## Like  lib_update(), but fir a single package and dependencies
