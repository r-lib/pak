#' Install a package
#'
#' Install a package and it's dependencies.
#' @param pkg A package specification. See [pkgdepends::remotes] for details.
#' @inheritParams pkginstall::install_packages
#' @importFrom pkgdepends remotes
#' @importFrom pkginstall install_packages
#' @importFrom crayon blue
#' @export
pkg_install <- function(pkg, lib = .libPaths()[[1L]], num_workers = 1L) {
  if (is_uri(pkg)) {
    stop("TODO: parse uri")
  }

  # Install from CRAN
  r <- remotes$new(pkg, library = lib)

  # Solve the dependency graph
  r$solve()

  # Actually download packages as needed
  r$download_solution()

  # Get the installation plan and ignore already installed versions.
  plan <- r$get_install_plan()
  dependencies <- plan[plan$package != pkg, ]
  uninstalled_plan <- dependencies[dependencies$type != "installed", ]
  if (nrow(uninstalled_plan) == 0) {
    message(glue::glue("
        {green_tick()} {blue(pkg)} and it's {blue(nrow(dependencies))} \\
        dependencies already installed"))
    return(invisible(plan))
  }

  # Install what is left
  install_packages(plan$file, lib = lib, plan = plan, num_workers = num_workers)
}

#' Install a local development package
#' @param path to the local package
#' @inheritParams pkginstall::install_packages
#' @importFrom pkgdepends remotes
#' @export
local_pkg_install <- function(path = ".", lib = .libPaths()[[1L]], num_workers = 1L) {

  # Construct a local spec
  pkg <- paste0("local::", normalizePath(path))

  pkg_install(remotes$new(pkg, library = lib), lib = lib, num_workers = num_workers)
}

#' Display installed locations of a package
#'
#' @param pkg Name of an installed package to display status for.
#' @param lib One or more library paths to lookup package status in.
#' @importFrom tibble tibble
#' @export
pkg_status <- function(pkg, lib = .libPaths()) {
  stopifnot(length(pkg == 1 && is.character(pkg)))

  desc <- lapply(lib, function(lib) {
    res <- suppressWarnings(packageDescription(pkg, lib.loc = lib, fields = c("Version", "Built")))
  })
  found <- !is.na(desc)
  versions <- vcapply(desc[found], "[[", "Version")
  built <- split_built(vcapply(desc[found], "[[", "Built"))
  tibble(!!! append(list(library = lib[found], version = versions), built))
}

split_built <- function(built) {
  nms <- c("build_r_version", "build_platform", "build_date", "build_os")
  if (length(built) < 1) {
    return(setNames(list(character(0), character(0), character(0), character(0)), nms))
  }
  setNames(as.list(strsplit(built, "; ")[[1L]]), nms)
}

#' Remove installed packages
#'
#' @param pkg A character vector of packages to remove.
#' @param lib library to remove packages from
#' @importFrom utils remove.packages
#' @export
pkg_remove <- function(pkg, lib = .libPaths()[[1L]]) {
  # TODO: do we need to do anything else for this?
  suppressMessages(remove.packages(pkg, lib))
}
