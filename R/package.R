
#' Package sources
#'
#' @description
#' Learn how to tell pak which packages to install, and where those packages
#' can be found.
#' 
#' @rawRd \eval{pak:::include_docs("pkgdepends", "docs/pkg-refs.rds")}
#'
#' @name pak_package_sources
#' @family package functions
NULL

#' The package dependency solver
#'
#' pak contains a package dependency solver, that makes sure that the
#' package source and version requirements of all packages are satisfied,
#' before starting an installation. For CRAN and BioC packages this is
#' usually automatic, because these repositories are generally in a
#' consistent state. If packages depend on other other package sources,
#' however, this is not the case.
#'
#' Here is an example of a conflict detected:
#' ```
#' > pak::pkg_install(c("r-lib/pkgcache@conflict", "r-lib/cli@message"))
#' Error: Cannot install packages:
#'   * Cannot install `r-lib/pkgcache@conflict`.
#'     - Cannot install dependency r-lib/cli@main
#'   * Cannot install `r-lib/cli@main`.
#' - Conflicts r-lib/cli@message
#' ```
#'
#' `r-lib/pkgcache@conflict` depends on the main branch of `r-lib/cli`,
#' whereas, we explicitly requested the `message` branch. Since it cannot
#' install both versions into a single library, pak quits.
#'
#' When pak considers a package for installation, and the package is given
#' with its name only, (e.g. as a dependency of another package), then
#' the package may have _any_ package source. This is necessary, because
#' one R package library may contain only at most one version of a package
#' with a given name.
#'
#' pak's behavior is best explained via an example.
#' Assume that you are installing a local package (see below), e.g.
#' `local::.`, and the local package depends on `pkgA` and `user/pkgB`,
#' the latter being a package from GitHub (see below), and that `pkgA`
#' also depends on `pkgB`. Now pak must install `pkgB` _and_ `user/pkgB`.
#' In this case pak interprets `pkgB` as a package from any package source,
#' instead of a standard package, so installing `user/pkgB` satisfies both
#' requirements.
#'
#' Note that that `cran::pkgB` and `user/pkgB` requirements result a
#' conflict that pak cannot resolve. This is because the first one _must_
#' be a CRAN package, and the second one _must_ be a GitHub package, and
#' two different packages with the same cannot be installed into an R
#' package library.
#'
#' @name pak_solver
#' @family pakcage functions
NULL

#' Install a package
#'
#' Install a package and its dependencies into a single package library.
#'
#' @param pkg Package names or remote package specifications to install.
#'   See [pak package sources][pak_package_sources] for details.
#' @param lib Package library to install the packages to. Note that _all_
#'   dependent packages will the be installed here, even if they are
#'   already installed in another library.
#' @param upgrade When `FALSE`, the default, does the minimum amount of work
#'   to give you the latest version of `pkg`. It will only upgrade packages if
#'   `pkg`, or one of its dependencies, explicitly requires a higher version
#'   than what you currently have.
#'
#'   When `upgrade = TRUE`, will do ensure that you have the latest version of
#'   `pkg` and all its dependencies.
#' @param ask Whether to ask for confirmation when installing a different
#'   version of a package that is already installed. Installations that only
#'   add new packages never require confirmation.
#' @param dependencies Dependency types. See
#'   [pkgdepends::as_pkg_dependencies()] for possible values. Note that
#'   changing this argument from the default might result an installation
#'   failure, e.g. if you set it to `FALSE`, packages might not build if
#'   their dependencies are not already installed.
#' @return (Invisibly) A data frame with information about the installed
#'   package(s).
#'
#' @export
#' @family package functions
#' @examples
#' \dontrun{
#' pkg_install("dplyr")
#'
#' # Upgrade dplyr and all its dependencies
#' pkg_install("dplyr", upgrade = TRUE)
#'
#' # Install the development version of dplyr
#' pkg_install("tidyverse/dplyr")
#'
#' # Switch back to the CRAN version. This will be fast because
#' # pak will have cached the prior install.
#' pkg_install("dplyr")
#' }
pkg_install <- function(pkg, lib = .libPaths()[[1L]], upgrade = FALSE,
                        ask = interactive(), dependencies = NA) {

  start <- Sys.time()

  status <- remote(
    function(...) get("pkg_install_make_plan", asNamespace("pak"))(...),
    list(pkg = pkg, lib = lib, upgrade = upgrade, ask = ask,
         start = start, dependencies = dependencies,
         loaded = loaded_packages(lib)))

  unloaded <- handle_status(status, lib, ask)$unloaded

  inst <- remote(
    function(...) get("pkg_install_do_plan", asNamespace("pak"))(...),
    list(proposal = NULL, lib = lib))

  if (length(unloaded) > 0) offer_restart(unloaded)

  invisible(inst)
}

pkg_install_make_plan <- function(pkg, lib, upgrade, ask, start,
                                  dependencies, loaded) {
  prop <- pkgdepends::new_pkg_installation_proposal(
    pkg,
    config = list(library = lib, dependencies = dependencies)
  )

  ## Solve the dependency graph
  prop$set_solve_policy(if (upgrade) "upgrade" else "lazy")
  prop$solve()
  prop$stop_for_solution_error()
  pkg_data$tmp <- list(proposal = prop, start = start)
  print_install_details(prop, lib, loaded)
}

pkg_install_do_plan <- function(proposal, lib) {

  num_workers <- get_num_workers()
  proposal <- proposal %||% pkg_data$tmp$proposal
  start  <- pkg_data$tmp$start
  pkg_data$tmp <- NULL

  # Actually download packages as needed
  proposal$download()
  proposal$stop_for_download_error()

  # sysreqs
  proposal$install_sysreqs()

  # Get the installation plan and hand it over to pkgdepends
  plan <- proposal$get_install_plan()
  inst <- pkgdepends::install_package_plan(plan = plan, lib = lib,
                                           num_workers = num_workers)

  attr(inst, "total_time") <- Sys.time() - start
  class(inst) <- c("pkg_install_result", class(inst))

  ## Remove some largeish columns that we don't really need any more
  inst$extra <- NULL

  ## One line summary of the install
  print_install_summary(inst)

  inst
}

#' Display installed locations of a package
#'
#' @param pkg Name of one or more installed packages to display status for.
#' @param lib One or more library paths to lookup packages status in.
#' @return Data frame with data about installations of `pkg`.
#' Columns include: `library`, `package`, `title`, `version`.
#'
#' @export
#' @family package functions
#' @examples
#' \dontrun{
#' pkg_status("MASS")
#' }

pkg_status <- function(pkg, lib = .libPaths()) {
  stopifnot(length(pkg == 1) && is.character(pkg))

  load_extra("pillar")
  remote(
    function(...) asNamespace("pak")$pkg_status_internal(...),
    list(pkg = pkg, lib = lib))
}

pkg_status_internal <- function(pkg, lib = .libPaths()) {
  st <- lapply(lib, pkgdepends::lib_status, packages = pkg)
  do.call(rbind, st)
}

#' Remove installed packages
#'
#' @param pkg A character vector of packages to remove.
#' @param lib library to remove packages from
#' @export
#' @family package functions

pkg_remove <- function(pkg, lib = .libPaths()[[1L]]) {
  remote(
    function(...) {
      get("pkg_remove_internal", asNamespace("pak"))(...)
    },
    list(pkg = pkg, lib = lib)
  )
  invisible()
}

pkg_remove_internal <- function(pkg, lib) {
  pr <- pkgdepends::parse_pkg_ref(pkg)
  suppressMessages(utils::remove.packages(pr$package, lib))
  invisible(pr)
}

#' Look up the dependencies of a package
#'
#' @param pkg Package name or remote package specification to resolve.
#' @param upgrade Whether to use the most recent available package
#'   versions.
#' @param dependencies Dependency types. See
#'   [pkgdepends::as_pkg_dependencies()] for possible values.
#' @return A data frame.
#'
#' @family package functions
#' @export
#' @examplesIf FALSE
#' pkg_deps("curl")
#' pkg_deps("r-lib/fs")

pkg_deps <- function(pkg, upgrade = TRUE, dependencies = NA) {
  stopifnot(length(pkg == 1) && is.character(pkg))
  load_extra("pillar")
  remote(
    function(...) {
      get("pkg_deps_internal", asNamespace("pak"))(...)
    },
    list(pkg = pkg, upgrade = upgrade, dependencies = dependencies)
  )
}

pkg_deps_internal <- function(pkg, upgrade, dependencies = NA) {
  deps <- pkg_deps_internal2(pkg, upgrade, dependencies)
  data <- deps$get_solution()$data
  # This refers to the 'desc' package namespace, and we don't really need it
  data$extra <- NULL
  data
}

pkg_deps_internal2 <- function(pkg, upgrade, dependencies) {
  dir.create(lib <- tempfile())
  on.exit(rimraf(lib), add = TRUE)
  config <- list(library = lib)
  if (!is.null(dependencies)) config$dependencies <- dependencies
  deps <- pkgdepends::new_pkg_deps(pkg, config = config)
  if (upgrade) deps$set_solve_policy("upgrade")
  deps$solve()
  deps$stop_for_solution_error()
  deps
}

#' Draw the dependency tree of a package
#'
#' @param pkg Package name or remote package specification to resolve.
#' @param upgrade Whether to use the most recent available package
#'   versions.
#' @param dependencies Dependency types. See
#'   [pkgdepends::as_pkg_dependencies()] for possible values.
#' @return The same data frame as [pkg_deps()], invisibly.
#'
#' @family package functions
#' @export
#' @examplesIf FALSE
#' pkg_deps_tree("dplyr")
#' pkg_deps_tree("r-lib/usethis")

pkg_deps_tree <- function(pkg, upgrade = TRUE, dependencies = NA) {
  stopifnot(length(pkg == 1) && is.character(pkg))
  ret <- remote(
    function(...) {
      get("pkg_deps_tree_internal", asNamespace("pak"))(...)
    },
    list(pkg = pkg, upgrade = upgrade, dependencies = dependencies)
  )
  cat(ret$tree, sep = "\n")
  invisible(ret$data)
}

pkg_deps_tree_internal <- function(pkg, upgrade, dependencies = NA) {
  deps <- pkg_deps_internal2(pkg, upgrade, dependencies)
  tree <- deps$draw()
  data <- deps$get_solution()$data
  # This refers to the 'desc' package namespace, and we don't really need it
  data$extra <- NULL
  list(tree = tree, data = data)
}

#' @rdname lib_status
#' @family package functions
#' @export

pkg_list <- function(lib = .libPaths()[1]) {
  lib_status(lib)
}

#' Download a package and potentially its dependencies as well
#'
#' @param pkg Package names or remote package specifications to download.
#' @param dest_dir Destination directory for the packages. If it does not
#'   exist, then it will be created.
#' @param dependencies Dependency types, to download the (recursive)
#'   dependencies of `pkg` as well. See [pkgdepends::as_pkg_dependencies()]
#'   for possible values.
#' @param platforms Types of binary or source packages to download. The
#'   default is the value of [pkgdepends::default_platforms()].
#' @param r_versions R version(s) to download packages for. (This does not
#'   matter for source packages, but it does for binaries.) It defaults to
#'   the current R version.
#' @return Data frame with information about the downloaded
#'   packages, invisibly.
#'
#' @export
#' @family package functions
#' @examplesIf FALSE
#' pkg_download("forcats")
#' pkg_download("r-lib/pak", platforms = "source")

pkg_download <- function(pkg, dest_dir = ".", dependencies = FALSE,
                         platforms = NULL, r_versions = NULL) {
  args <- list(
    pkg = pkg,
    dest_dir = dest_dir,
    dependencies = dependencies,
    platforms = platforms,
    r_versions = r_versions
  )

  dl <- remote(
    function(...) {
      get("pkg_download_internal", asNamespace("pak"))(...)
    },
    args
  )

  load_extra("pillar")
  invisible(dl)
}

pkg_download_internal <- function(pkg, dest_dir = ".", dependencies = FALSE,
                                  platforms = NULL, r_versions = NULL) {
  mkdirp(dest_dir)
  config <- list(cache_dir = dest_dir, dependencies = dependencies)
  if (!is.null(platforms)) config$platforms <- platforms
  if (!is.null(r_versions)) config$`r-versions` <- r_versions
  dl <- pkgdepends::new_pkg_download_proposal(pkg, config = config)
  dl$resolve()
  dl$download()
  dl$stop_for_download_error()
  dl$get_downloads()
}
