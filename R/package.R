
#' Install packages
#'
#' Install one or more packages and their dependencies into a single
#' package library.
#'
#' @param pkg Package names or package references. E.g.
#'   - `ggplot2`: package from CRAN, Bioconductor or a CRAN-like repository
#'     in general,
#'   - `tidyverse/ggplot2`: package from GitHub,
#'   - `tidyverse/ggplot2@v3.4.0`: package from GitHub tag or branch,
#'   - `https://examples.com/.../ggplot2_3.3.6.tar.gz`: package from URL,
#'   - `.`: package in the current working directory.
#'
#'   See "[Package sources]" for more details.
#' @param lib Package library to install the packages to. Note that _all_
#'   dependent packages will be installed here, even if they are
#'   already installed in another library. The only exceptions are base
#'   and recommended packages installed in `.Library`. These are not
#'   duplicated in `lib`, unless a newer version of a recommemded package
#'   is needed.
#' @param upgrade When `FALSE`, the default, pak does the minimum amount
#'   of work to give you the latest version(s) of `pkg`. It will only upgrade
#'   dependent packages if `pkg`, or one of their dependencies explicitly
#'   require a higher version than what you currently have. It will also
#'   prefer a binary package over to source package, even it the binary
#'   package is older.
#'
#'   When `upgrade = TRUE`, pak will ensure that you have the latest
#'   version(s) of `pkg` and all their dependencies.
#' @param ask Whether to ask for confirmation when installing a different
#'   version of a package that is already installed. Installations that only
#'   add new packages never require confirmation.
#' @param dependencies What kinds of dependencies to install. Most commonly
#'   one of the following values:
#'   - `NA`: only required (hard) dependencies,
#'   - `TRUE`: required dependencies plus optional and development
#'     dependencies,
#'   - `FALSE`: do not install any dependencies. (You might end up with a
#'     non-working package, and/or the installation might fail.)
#'   See [Package dependency types] for other possible values and more
#'   information about package dependencies.
#' @return (Invisibly) A data frame with information about the installed
#'   package(s).
#'
#' @export
#' @seealso [Get started with pak], [Package sources], [FAQ],
#'   [The dependency solver].
#' @family package functions
#' @section Examples:
#' ```{asciicast pkg-install-dplyr}
#' pkg_install("dplyr")
#' ```
#'
#' Upgrade dplyr and all its dependencies:
#' ```{asciicast pkg-install-upgrade}
#' pkg_install("dplyr", upgrade = TRUE)
#' ````
#'
#' Install the development version of dplyr:
#' ```{asciicast pkg-install-gh}
#' pkg_install("tidyverse/dplyr")
#' ```
#'
#' Switch back to the CRAN version. This will be fast because
#' pak will have cached the prior install.
#' ```{asciicast pkg-install-cran}
#' pkg_install("dplyr")
#' ```

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
    list(proposal = NULL))

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

pkg_install_do_plan <- function(proposal) {

  proposal <- proposal %||% pkg_data$tmp$proposal
  start  <- pkg_data$tmp$start
  pkg_data$tmp <- NULL

  # Actually download packages as needed
  proposal$download()
  proposal$stop_for_download_error()

  # sysreqs
  proposal$install_sysreqs()

  # Get the installation plan and hand it over to pkgdepends
  inst <- proposal$install()

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
#'   By default all libraries are used.
#' @return Data frame with data about installations of `pkg`.
#'   \eval{include_docs("pkgdepends", "docs/lib-status-return.rds")}
#'
#' @export
#' @family package functions
#' @section Examples:
#' ```{asciicast pkg-status}
#' pkg_status("MASS")
#' ```

pkg_status <- function(pkg, lib = .libPaths()) {
  stopifnot(length(pkg == 1) && is.character(pkg))

  load_extra("pillar")
  remote(
    function(...) asNamespace("pak")$pkg_status_internal(...),
    list(pkg = pkg, lib = lib))
}

pkg_status_internal <- function(pkg, lib = .libPaths()) {
  st <- lapply(lib, pkgdepends::lib_status, packages = pkg)
  do.call("rbind_expand", st)
}

#' Remove installed packages
#'
#' @param pkg A character vector of packages to remove.
#' @param lib library to remove packages from.
#' @return Nothing.
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
#' @param upgrade Whether to use the most recent available package
#'   versions.
#' @inheritParams pkg_install
#' @return A data frame with the dependency data, it includes `pkg`
#'   as well. It has the following columns.
#'   \eval{include_docs("pkgdepends", "docs/resolution-result.rds")}
#'
#' @family package functions
#' @export
#' @section Examples:
#' ```{asciicast pkg-deps}
#' pkg_deps("dplyr")
#' ```
#'
#' For a package on GitHub:
#' ```{asciicast pkg-deps-gh}
#' pkg_deps("r-lib/callr")
#' ```

pkg_deps <- function(pkg, upgrade = TRUE, dependencies = NA) {
  stopifnot(length(pkg) == 1 && is.character(pkg))
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
#' @param upgrade Whether to use the most recent available package
#'   versions.
#' @inheritParams pkg_install
#' @return The same data frame as [pkg_deps()], invisibly.
#'
#' @family package functions
#' @export
#' @section Examples:
#' ```{asciicast pkg-deos-tree}
#' pkg_deps_tree("dplyr")
#' ```
#'
#' ```{asciicast pkg-deps-tree-2}
#' pkg_deps_tree("r-lib/usethis")
#' ```

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

#' Download a package and its dependencies
#'
#' TODO: explain result
#'
#' @param dest_dir Destination directory for the packages. If it does not
#'   exist, then it will be created.
#' @param platforms Types of binary or source packages to download. The
#'   default is the value of [pkgdepends::default_platforms()].
#' @param r_versions R version(s) to download packages for. (This does not
#'   matter for source packages, but it does for binaries.) It defaults to
#'   the current R version.
#' @inheritParams pkg_install
#' @return Data frame with information about the downloaded
#'   packages, invisibly. Columns:
#'   \eval{include_docs("pkgdepends", "docs/download-result.rds")}
#'
#' @export
#' @family package functions
#' @section Examples:
#'
#' ```{asciicast pkg-download}
#' dl <- pkg_download("forcats")
#' ```
#'
#' ```{asciicast pkg-download-2}
#' dl
#' ```
#'
#' ```{asciicast pkg-download-3}
#' dl$fulltarget
#' ```
#'
#' ```{asciicast pkg-download-4}
#' pkg_download("r-lib/pak", platforms = "source")
#' ```

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
