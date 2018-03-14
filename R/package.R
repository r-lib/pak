#' Install a package
#'
#' Install a package and it's dependencies.
#' @param pkg Package names or remote package specifications to install.
#'   See [pkgdepends::remotes] for details about remote package
#'   specifications.
#' @param lib Package library to install the packages to.
#' @param upgrade Whether to upgrade already installed packages to the
#'   latest available version.
#' @param num_workers Number of worker processes to use.
#' @param ask Whether to ask for confirmation.
#' @importFrom pkgdepends remotes
#' @importFrom pkginstall install_package_plan
#' @importFrom crayon blue
#' @importFrom cli cli default_theme
#' @export
pkg_install <- function(pkg, lib = .libPaths()[[1L]], upgrade = FALSE,
                        num_workers = 1L, ask = interactive()) {

  start <- Sys.time()
  cli$add_theme(default_theme())

  r <- remotes$new(pkg, library = lib)

  # Solve the dependency graph
  policy <- if (upgrade) "upgrade" else "lazy"
  r$solve(policy = policy)
  r$stop_for_solve_error()

  ask_for_confirmation(ask, r$get_solution()$data$data, lib)

  # Actually download packages as needed
  r$download_solution()
  r$stop_for_solution_download_error()

  # Get the installation plan and hand it over to pkginstall
  plan <- r$get_install_plan()
  inst <- install_package_plan(plan = plan, lib = lib,
                               num_workers = num_workers)

  attr(inst, "total_time") <- Sys.time() - start
  class(inst) <- c("pkgman_install_result", class(inst))
  inst
}

#' Install a local development package
#' @param path to the local package
#' @inheritParams pkg_install
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
#' @importFrom stats setNames
#' @importFrom utils packageDescription
#' @export
pkg_status <- function(pkg, lib = .libPaths()) {
  stopifnot(length(pkg == 1) && is.character(pkg))

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

total_num_deps <- function(plan) {
  nn <- nrow(plan)
  res <- rep(0, nn)
  for (i in seq_along(res)) {
    done <- rep(FALSE, nn)
    edge <- i
    while (length(edge)) {
      done[edge] <- TRUE
      res[i] <- res[i] + length(edge)
      edge <- match(
        setdiff(
          unique(unlist(plan$dependencies[edge])),
          plan$package[done]
        ),
        plan$package
      )
    }
  }
  res
}
