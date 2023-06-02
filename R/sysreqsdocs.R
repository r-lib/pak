
#' List platforms with system requirements support
#'
#' @usage
#' sysreqs_platforms()
#'
#' @return
#' Data frame with columns:
#' * `name`: human readable OS name.
#' * `os`: OS name, e.g. `linux`.
#' * `distribution`: OS id, e.g. `ubuntu` or `redhat`.
#' * `version`: distribution version. A star means that all versions are
#'    supported, that are also supported by the vendor.
#' * `update_command`: command to run to update the system package metadata.
#' * `install_command`: command to run to install packages.
#' * `query_command`: name of the tool to use to query system package
#'    information.
#'
#' @name sysreqs_platforms
#' @export
#' @family system requirements functions
#' @examplesIf Sys.getenv("IN_PKGDOWN") == "true"
#' sysreqs_platforms()

sysreqs_platforms

#' Check if a platform has system requirements support
#'
#' @usage
#' sysreqs_is_supported(sysreqs_platform = NULL)
#'
#' @param sysreqs_platform System requirements platform. If `NULL`, then the
#'   `sysreqs_platform` \eval{man_config_link("configuration option")}
#'   is used, which defaults to the current platform. Set this option if
#'   \eval{.packageName} does not detect your platform correctly.
#' @return Logical scalar.
#'
#' @name sysreqs_is_supported
#' @export
#' @family system requirements functions
#' @seealso The `sysreqs_platform`
#'   \eval{man_config_link("configuration option")}.
#' @examplesIf Sys.getenv("IN_PKGDOWN") == "true"
#' sysreqs_is_supported()

sysreqs_is_supported

#' List contents of the system requirements DB, for a platform
#'
#' It also tries to update the system dependency database, if it is
#' outdated. (I.e. older than allowed in the `metadata_update_after`
#' \eval{man_config_link("configuration option")}.
#'
#' @usage
#' sysreqs_db_list(sysreqs_platform = NULL)
#'
#' @param sysreqs_platform System requirements platform. If `NULL`, then the
#'   `sysreqs_platform` \eval{man_config_link("configuration option")}
#'   is used, which defaults to the current platform. Set this option if
#'   \eval{.packageName} does not detect your platform correctly.
#' @return Data frame with columns:
#' * `name`: cross platform system dependency name in the database.
#' * `patterns`: one or more regular expressions to match to
#'   `SystemRequirements` fields.
#' * `packages`: one or more system package names to install.
#' * `pre_install`: command(s) to run before installing the packages.
#' * `post_install`:: command(s) to run after installing the packages.
#'
#' @name sysreqs_db_list
#' @export
#' @family system requirements functions
#' @examplesIf Sys.getenv("IN_PKGDOWN") == "true"
#' sysreqs_db_list(sysreqs_platform = "ubuntu-22.04")

sysreqs_db_list

#' Match system requirement descriptions to the database
#'
#' In the usual workflow `r pak_or_pkgdepends()` matches the
#' `SystemRequirements` fields of the `DESCRIPTION` files to the database.
#'
#' The `sysreqs_db_match()` function lets you match any string, and it is
#' mainly useful for debugging.
#'
#' @usage
#' sysreqs_db_match(specs, sysreqs_platform = NULL)
#'
#' @param specs Character vector of system requirements descriptions.
#' @param sysreqs_platform System requirements platform. If `NULL`, then the
#'   `sysreqs_platform` \eval{man_config_link("configuration option")}
#'   is used, which defaults to the current platform. Set this option if
#'   \eval{.packageName} does not detect your platform correctly.
#' @return Data frame with columns:
#' * `spec`: the input `specs`.
#' * `sysreq`:  name of the system library or tool.
#' * `packages`: system packages, list column of character vectors.
#'    Rarely it can be an empty string, e.g. if a `pre_install` script
#'    performs the installation.
#' * `pre_install`: list column of character vectors. Shell script(s) to
#'    run before the installation.
#' * `post_install`: list column of character vectors. Shell script(s) to
#'    run after the installation.
#'
#' @name sysreqs_db_match
#' @export
#' @family system requirements functions
#' @examplesIf Sys.getenv("IN_PKGDOWN") == "true"
#' sysreqs_db_match(
#'   c("Needs libcurl", "Java, libssl"),
#'   sysreqs_platform = "ubuntu-22.04"
#' )

sysreqs_db_match

#' Update the cached copy of the system requirements database
#'
#' @usage
#' sysreqs_db_update()
#'
#' @details
#' If the the cached copy is recent, then no update is attempted. See the
#' `metadata_update_after` \eval{man_config_link("configuration option")}.
#'
#' @name sysreqs_db_update
#' @export
#' @family system requirements functions

sysreqs_db_update

#' Check if installed packages have all their system requirements
#'
#' @usage
#' sysreqs_check_installed(packages = NULL, library = .libPaths()[1])
#'
#' @details
#' This function uses the `sysreqs_platform` configuration option,
#' see \eval{man_config_link("Configuration")}. Set this if
#' `r pak_or_pkgdepends()` does not detect your platform correctly.
#'
#' @param packages If not `NULL`, then only these packages are checked.
#'   If a package in `packages` is not installed, then
#'   `r pak_or_pkgdepends()` throws a warning.
#' @param library Library or libraries to check.
#' @return Data frame with a custom print and format method, and a
#'   `pkg_sysreqs_check_result` class. Its columns are:
#'    * `system_package`: string, name of the required system package.
#'    * `installed`: logical, whether the system package is correctly
#'      installed.
#'    * `packages`: list column of character vectors. The names of the
#'      installed R packages that need this system package.
#'
#' @name sysreqs_check_installed
#' @export
#' @family system requirements functions
#' @examplesIf Sys.getenv("IN_PKGDOWN") == "true" && Sys.info()[["sysname"]] == "Linux"
#' # This only works on supported platforms
#' sysreqs_check_installed()

sysreqs_check_installed

#' List installed system packages
#'
#' @usage
#' sysreqs_list_system_packages()
#'
#' @details
#' This function uses the `sysreqs_platform` configuration option,
#' see \eval{man_config_link("Configuration")}. Set this if
#' `r pak_or_pkgdepends()` does not detect your platform correctly.
#'
#' @return Data frame with columns:
#'   * `status`. two or three characters, the notation of `dpkg` on Debian
#'     based systems. `"ii"` means the package is correctly installed.
#'     On `RPM` based systems it is always `"ii"` currently.
#'   * `package`: name of the system package.
#'   * `version`: installed version of the system package.
#'   * `capabilities`: list column of character vectors, the capabilities
#'     provided by the package.
#'
#' @name sysreqs_list_system_packages
#' @export
#' @family system requirements functions
#' @examplesIf Sys.getenv("IN_PKGDOWN") == "true" && Sys.info()[["sysname"]] == "Linux"
#' sysreqs_list_system_packages()[1:10,]

sysreqs_list_system_packages
