
#' Package sources
#'
#' @section Standard packages:
#'
#' pak can install packages from various package sources. By default,
#' a package name without the specification of its source, refers to a
#' CRAN or Bioconductor package. pak calls these _standard_ packages.
#' For example:
#'
#' ```r
#' ## CRAN package
#' pkg_install("glue")
#' ## BioC package
#' pkg_install("limma")
#' ```
#' When considering a standard package, the calling version of R is used
#' to determine the available source and binary packages on CRAN and the
#' Bioconductor repositories.
#'
#' The full specification of standard packages is simply
#' ```
#' [standard::]<package>
#' ```
#'
#' If you know the exact source of the package, you can also write
#' ```
#' cran::<package>
#' bioc::<package>
#' ```
#'
#' @section GitHub packages:
#'
#' pak can install packages from GitHub repositories. Any package that
#' is specified in the `user/repo` notation is taken to be a GitHub package.
#' For example:
#'
#' ```r
#' ## Package from GitHub
#' pkg_install("r-lib/glue")
#' ```
#'
#' The full specification of GitHub packages is
#' ```
#' [<package>=][github::]<username>/<repo>[/<subdir>]
#'     [@<committish> | #<pull> | @[*]release]
#'
#' ```
#'
#' * `<package>` is the name of the package. If this is missing, the
#'   name of the package must match the name of the repository.
#' * `<username>`: GitHub user or organization name.
#' * `<repo>`: repository name.
#' * `<subdir>`: If the R package is in a subdirectory within the
#'   repository.
#' * `<commitish>`: A branch name, git tag or SHA hash, to specify the
#'   branch, tag or commit to download or install.
#' * `<pull>`: Pull request number, to install the branch that corresponds
#'   to a pull request.
#' * The `@*release` string can be used to install the latest release.
#'
#' @section Local package trees:
#'
#' pak can install packages from package trees. You can either use the
#' [local_install()] function for this, or specify the `local::` package
#' source. E.g. these are equivalent:
#'
#' ```r
#' local_install("/path/to/my/package")
#' pkg_install("local::/path/to/my/package")
#' ```
#'
#' The `local::` form is handy if you want to mix it with other package
#' specifications, e.g. to install a local package, and another standard
#' package:
#'
#' ```r
#' pkg_install(c("local://path/to/my/package", "testthat"))
#' ```
#'
#' @section The `Remotes` field:
#'
#' You can mark any regular dependency defined in the `Depends`, `Imports`,
#' `Suggests` or `Enhances` fields as being installed from a remote
#' location by adding the remote location to `Remotes` in your
#' `DESCRIPTION` file. This will cause pak to download and install them
#' from the specified location, instead of CRAN.
#'
#' The remote dependencies specified in `Remotes` is a comma separated
#' list of package sources:
#'
#' ```
#' Remotes: <pkg-source-1>, <pkg-source-2>, [ ... ]
#' ```
#'
#' Note that you will still need add the package to one of the regular
#' dependency fields, i.e. `Imports`, `Suggests`, etc. Here is a concrete
#' example that specifies the `r-lib/glue` package:
#'
#' ```
#' Imports: glue
#' Remotes: `r-lib/glue,
#'   r-lib/httr@v0.4,
#'   klutometis/roxygen#142,
#'   r-lib/testthat@c67018fa4970
#' ```
#'
#' The CRAN and Bioconductor repositories do not support the `Remotes`
#' field, so you need to remove this field, before submitting your package
#' to either of them.
#'
#' @section The package dependency solver:
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
#'     - Cannot install dependency r-lib/cli@master
#'   * Cannot install `r-lib/cli@master`.
#' - Conflicts r-lib/cli@message
#' ```
#'
#' `r-lib/pkgcache@conflict` depends on the master branch of `r-lib/cli`,
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
#' @name pak_package_sources
#' @family package functions
NULL

#' Install a package
#'
#' Install a package and its dependencies, into a single package library.
#'
#' @param pkg Package names or remote package specifications to install.
#' @param lib Package library to install the packages to. Note that _all_
#'   dependent packages will the be installed here, even if they are
#'   already installed in another library.
#' @param upgrade Whether to upgrade already installed packages to the
#'   latest available version. If this is `FALSE`, then only packages
#'   that need updates to satisfy version requirements, will be updated.
#'   If it is `TRUE`, all specified or dependent packages will be updated
#'   to the latest available version.
#' @param ask Whether to ask for confirmation.
#' @return Data frame, with information about the installed package(s).
#'
#' @export
#' @family package functions
#' @examples
#' \dontrun{
#' pkg_install("dplyr")
#' pkg_install("dplyr", upgrade = TRUE)
#'
#' ## Package from GitHub
#' pkg_install("r-lib/pkgconfig")
#' }

pkg_install <- function(pkg, lib = .libPaths()[[1L]], upgrade = FALSE,
                        ask = interactive()) {

  start <- Sys.time()

  any <- remote(
    function(...) get("pkg_install_make_plan", asNamespace("pak"))(...),
    list(pkg = pkg, lib = lib, upgrade = upgrade, ask = ask, start = start))

  if (any && ask) {
    get_confirmation("? Do you want to continue (Y/n) ")
  }

  inst <- remote(
    function(...) get("pkg_install_do_plan", asNamespace("pak"))(...),
    list(remotes = NULL, lib = lib))

  invisible(inst)
}

pkg_install_make_plan <- function(pkg, lib, upgrade, ask, start) {
  r <- remotes()$new(pkg, library = lib)

  ## Solve the dependency graph
  policy <- if (upgrade) "upgrade" else "lazy"
  r$solve(policy = policy)
  r$stop_for_solve_error()
  pkg_data$tmp <- list(remotes = r, start = start)
  sol <- r$get_solution()$data
  print_install_details(sol, lib)
}

pkg_install_do_plan <- function(remotes, lib) {

  num_workers <- get_num_workers()
  remotes <- remotes %||% pkg_data$tmp$remotes
  start  <- pkg_data$tmp$start
  pkg_data$tmp <- NULL

  # Actually download packages as needed
  remotes$download_solution()
  remotes$stop_for_solution_download_error()

  # Get the installation plan and hand it over to pkginstall
  plan <- remotes$get_install_plan()
  inst <- install_package_plan(plan = plan, lib = lib,
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
#' @param pkg Name of an installed package to display status for.
#' @param lib One or more library paths to lookup package status in.
#'
#' @export
#' @family package functions
#' @examples
#' \dontrun{
#' pkg_status("MASS")
#' }

pkg_status <- function(pkg, lib = .libPaths()) {
  stopifnot(length(pkg == 1) && is.character(pkg))

  remote(
    function(...) asNamespace("pak")$pkg_status_internal(...),
    list(pkg = pkg, lib = lib))
}

pkg_status_internal <- function(pkg, lib) {
  st <- lapply(lib, pkgdepends_lib_status, packages = pkg)
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
  pr <- parse_remotes(pkg)[[1]]
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
## Like  lib_update(), but for a single package and dependencies
