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
#' @export

pkg_install <- function(pkg, lib = .libPaths()[[1L]], upgrade = FALSE,
                        num_workers = 1L, ask = interactive()) {

  start <- Sys.time()

  todo <- remote(
    function(...) get("pkg_install_make_plan", asNamespace("pkgman"))(...),
    list(pkg = pkg, lib = lib, upgrade = upgrade, ask = ask, start = start))

  if (todo && ask) {
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

#' Install a local development package
#' @param path to the local package
#' @inheritParams pkg_install
#' @export
local_pkg_install <- function(path = ".", lib = .libPaths()[[1L]], num_workers = 1L) {

  # Construct a local spec
  pkg <- paste0("local::", normalizePath(path))

  pkg_install(pkgdepends::remotes$new(pkg, library = lib), lib = lib, num_workers = num_workers)
}

#' Display installed locations of a package
#'
#' @param pkg Name of an installed package to display status for.
#' @param lib One or more library paths to lookup package status in.
#' @export
pkg_status <- function(pkg, lib = .libPaths()) {
  stopifnot(length(pkg == 1) && is.character(pkg))

  desc <- lapply(lib, function(lib) {
    res <- suppressWarnings(utils::packageDescription(pkg, lib.loc = lib, fields = c("Version", "Built")))
  })
  found <- !is.na(desc)
  versions <- vcapply(desc[found], "[[", "Version")
  built <- split_built(vcapply(desc[found], "[[", "Built"))
  do.call(tibble::tibble,
          append(list(library = lib[found], version = versions), built))
}

split_built <- function(built) {
  nms <- c("build_r_version", "build_platform", "build_date", "build_os")
  if (length(built) < 1) {
    return(stats::setNames(list(character(0), character(0), character(0), character(0)), nms))
  }
  stats::setNames(as.list(strsplit(built, "; ")[[1L]]), nms)
}

#' Remove installed packages
#'
#' @param pkg A character vector of packages to remove.
#' @param lib library to remove packages from
#' @export
pkg_remove <- function(pkg, lib = .libPaths()[[1L]]) {
  # TODO: do we need to do anything else for this?
  suppressMessages(utils::remove.packages(pkg, lib))
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
