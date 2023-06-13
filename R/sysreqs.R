
sysreqs_is_supported <- function(sysreqs_platform = NULL) {
  remote(
    function(...) pkgdepends::sysreqs_is_supported(...),
    list(sysreqs_platform = sysreqs_platform)
  )
}

sysreqs_platforms <- function() {
  remote(
    function() pkgdepends::sysreqs_platforms()
  )
}

sysreqs_list_system_packages <- function() {
  load_extra("pillar")
  remote(
    function() pkgdepends::sysreqs_list_system_packages()
  )
}

sysreqs_db_match <- function(specs, sysreqs_platform = NULL) {
  load_extra("pillar")
  remote(
    function(...) pkgdepends::sysreqs_db_match(...),
    list(specs = specs, sysreqs_platform = sysreqs_platform)
  )
}

sysreqs_db_update <- function() {
  invisible(remote(
    function() pkgdepends::sysreqs_db_update()
  ))
}

sysreqs_db_list <- function(sysreqs_platform = NULL) {
  load_extra("pillar")
  remote(
    function(...) pkgdepends::sysreqs_db_list(...),
    list(sysreqs_platform = sysreqs_platform)
  )
}

sysreqs_check_installed <- function(packages = NULL,
                                    library = .libPaths()[1]) {
  load_extra("pillar")
  remote(
    function(...) {
      ret <- pkgdepends::sysreqs_check_installed(...)
      asNamespace("pak")$pak_preformat(ret)
    },
    list(packages = packages, library = library)
  )
}

#' Calculate system requirements of one of more packages
#'
#' @inheritParams pkg_install
#' @param sysreqs_platform System requirements platform. If `NULL`, then the
#'   `sysreqs_platform` \eval{man_config_link("configuration option")}
#'   is used, which defaults to the current platform. Set this option if
#'   \eval{.packageName} does not detect your platform correctly.
#' @return List with entries:
#'   * `os`: character string. Operating system.
#'   * `distribution`: character string. Linux distribution, `NA` if the
#'     OS is not Linux.
#'   * `version`: character string. Distribution version, `NA` is the OS
#'     is not Linux.
#'   * `pre_install`: character vector. Commands to run before the
#'     installation of system packages.
#'   * `install_scripts`: character vector. Commands to run to install the
#'     system packages.
#'   * `post_install`: character vector. Commands to run after the
#'     installation of system packages.
#'   * `packages`: data frame. Information about the system packages that
#'     are needed. It has columns:
#'     * `sysreq`: string, cross-platform name of the system requirement.
#'     * `packages`: list column of character vectors. The names of the R
#'       packages that have this system requirement.
#'     * `pre_install`: list column of character vectors. Commands run
#'       before the package installation for this system requirement.
#'     * `system_packages`: list column of character vectors. Names of
#'       system packages to install.
#'     * `post_install`: list column of character vectors. Commands run
#'       after the package installation for this system requirement.
#'
#' @family package functions
#' @family system requirements functions
#' @export

pkg_sysreqs <- function(pkg, upgrade = TRUE, dependencies = NA,
                        sysreqs_platform = NULL) {
  load_extra("pillar")
  remote(
    function(...) {
      get("pkg_sysreqs_internal", asNamespace("pak"))(...)
    },
    list(
      pkg = pkg,
      upgrade = upgrade,
      dependencies = dependencies,
      sysreqs_platform = sysreqs_platform
    )
  )
}

pkg_sysreqs_internal <- function(pkg, upgrade = TRUE, dependencies = NA,
                                 sysreqs_platform = NULL) {
  dir.create(lib <- tempfile())
  on.exit(rimraf(lib), add = TRUE)
  config <- list(library = lib)
  if (!is.null(sysreqs_platform)) {
    config[["sysreqs_platform"]] <- sysreqs_platform
  }
  if (!is.null(dependencies)) {
    config[["dependencies"]] <- dependencies
  }
  srq <- pkgdepends::sysreqs_install_plan(
    pkg,
    upgrade = upgrade,
    config = config
  )
  srq
}
