#' Install a package
#'
#' Install a package and it's dependencies.
#' @param pkg A package specification. See [pkgdepends::remotes] for details.
#' @inheritParams pkginstall::install_packages
#' @export
pkg_install <- function(pkg, lib = .libPaths()[[1L]], num_workers = 1L) {
  if (is_uri(pkg)) {
    stop("TODO: parse uri")
  }

  # Install from CRAN
  r <- pkgdepends::remotes$new(pkg, library = lib)

  # Solve the dependency graph
  r$solve()

  # Actually download packages as needed
  r$download_solution()

  # Get the installation plan and ignore already installed versions.
  plan <- r$get_install_plan()
  plan <- plan[plan$type != "installed", ]
  if (nrow(plan) == 0) {
    return(invisible())
  }

  # Install what is left
  pkginstall::install_packages(plan$file, lib = lib, plan = plan, num_workers = num_workers)
}

#' Install a local development package
#' @param path to the local package
#' @inheritParams pkginstall::install_packages
#' @export
pkg_install_local <- function(path, lib = .libPaths()[[1L]], num_workers = 1L) {

  # Construct a local spec
  pkg <- paste0("local::", path)

  pkg_install(pkgdepends::remotes$new(pkg, library = lib), lib = lib, num_workers = num_workers)
}
