
#' Create a project that has a private package library
#'
#' @param project_root Root directory of the project. If it does not
#'   exist, it will be created.
#'
#' @family project functions
#' @keywords internal

proj_create <- function(project_root = ".") {
  remote(
    function(...) {
      asNamespace("pak")$proj_create_internal(...)
    },
    list(project_root = project_root))
  .libPaths(unique(c('r-packages', .libPaths())))
}

proj_create_internal <- function(project_root) {
  mkdirp(file.path(project_root, "r-packages"))
  prof <- file.path(project_root, ".Rprofile")
  if (file.exists(prof)) {
    cliapp::cli_alert_danger(
      "{path .Rprofile} already exist at {path {prof}}, remove it first")
    stop("Could not create .Rprofile, already exists")
  } else {
    writeLines(proj_rprofile(), con = prof)
    descfile <- file.path(project_root, "DESCRIPTION")
    if (file.exists(descfile)) {
      cliapp::cli_alert_info("{path DESCRIPTION} file already exists.")
    } else {
      proj_create_desc(project_root)
      cliapp::cli_alert_success("Created {path DESCRIPTION} file.")
    }
    cliapp::cli_alert_success("Created {path .Rprofile} at {path {prof}}.")
  }
}

proj_rprofile <- function() {
  c("if (file.exists('~/.Rprofile')) source('~/.Rprofile')",
    "dir.create('r-packages', showWarnings = FALSE, recursive = TRUE)",
    ".libPaths(unique(c('r-packages', .libPaths())))"
  )
}

proj_create_desc <- function(project_root) {
  dsc <- desc::desc(text = "")
  dsc$set(
    Package = to_package_name(basename(project_root)),
    Version = "0.0.0.9000",
    Title = "<Package title>",
    Description = "<Longer desciption of package or project>",
    URL = "<Package URL>",
    BugReports = "<Issue tracker URL>",
    License = "Private package",
    Encoding = "UTF-8",
    Roxygen = "list(markdown = TRUE)")

  tryCatch(
    dsc$add_me(role = c("cre", "aut")),
    error = function(e) e)

  descfile <- file.path(project_root, "DESCRIPTION")
  dsc$write(descfile)
}

#' Install project dependencies into the project library
#'
#' The project library is in `r-packages`, within the project directory.
#'
#' @param pkg Package(s) to install. By default (if `NULL`), they are
#'   taken from the `DESCRIPTION` file in `root`. If not `NULL`, then the
#'   packages will be added to the `DESCRIPTION` files, and existing
#'   references to these packages will be removed first.
#' @param root Path to the project directory. A subdirectory can be given
#'   as well. Defaults to the current working directory.
#' @param upgrade Whether to try to install the latest available versions
#'   of the specified package(s) and/or all dependencies.
#' @param optional If `pkg` is not `NULL`, then the new packages will be
#'   added as optional, i.e. in the `Suggests` section.
#' @param ask Whether to ask the user for confirmation. For non-interactive
#'   installs supply`FALSE` here.
#' @return Data frame containing data about the installed / updated
#'   packages.
#'
#' @family project functions
#' @keywords internal

proj_install <- function(pkg = NULL, root = ".", upgrade = FALSE,
                         optional = FALSE, ask = interactive()) {

  start <- Sys.time()

  any <- remote(
    function(...) {
      get("proj_install_make_plan", asNamespace("pak"))(...)
    },
    list(pkg = pkg, root = root, upgrade = upgrade, ask = ask,
         start = start, dev = FALSE))

  if (any && ask) get_confirmation("? Do you want to continue (Y/n) ")

  inst <- remote(
    function(...) get("proj_install_do_plan", asNamespace("pak"))(...),
    list(optional = optional))

  invisible(inst)
}

proj_install_make_plan <- function(pkg, root, upgrade, ask, start, dev) {
  dirs <- proj_get_dirs(root)

  r <- remotes()$new(
    pkg %||% paste0("deps::", dirs$root), library = dirs$lib,
    config = if (dev) list(dependencies = TRUE) else list())

  policy <- if (upgrade) "upgrade" else "lazy"
  r$solve(policy = policy)
  r$stop_for_solve_error()

  pkg_data$tmp <- list(
    remotes = r, start = start, lib = dirs$lib, root = dirs$root, pkg = pkg)

  sol <- r$get_solution()$data
  print_install_details(sol, dirs$lib)
}

proj_install_do_plan <- function(optional) {
  tmp <- pkg_data$tmp

  res <- pkg_install_do_plan(remotes = tmp$remotes, lib = tmp$lib)

  if (!is.null(tmp$pkg)) {
    add_refs_to_description(tmp$root, tmp$pkg, optional)
  }

  res
}

#' Install project dependencies, including development dependencies, into
#' private project library
#'
#' @inheritParams proj_install
#' @return Data frame containing data about the installed / updated
#'   packages.
#'
#' @family project functions
#' @keywords internal

proj_install_dev <-  function(root = ".", upgrade = FALSE,
                              ask = interactive()) {
  start <- Sys.time()

  any <- remote(
    function(...) {
      get("proj_install_make_plan", asNamespace("pak"))(...)
    },
    list(pkg = NULL, root = root, upgrade = upgrade, ask = ask,
         start = start, dev = TRUE))

  if (any && ask) get_confirmation("? Do you want to continue (Y/n) ")

  inst <- remote(
    function(...) get("proj_install_do_plan", asNamespace("pak"))(...),
    list(optional = FALSE))

  invisible(inst)
}

#' Remove package(s) from a project
#'
#' Removes it both from the project library and the `DESCRIPTION` file.
#' Note that it does not remove dependencies that are not needed any more.
#'
#' @param pkg Package(s) to remove. These can be package names or general
#'   remote references, e.g. `github::r-lib/pak`. The packages are also
#'   removed from `DESCRIPTION`.
#' @inheritParams proj_install
#'
#' @family project functions
#' @keywords internal

proj_remove <- function(pkg, root = ".", ask = interactive()) {

  any <-  remote(
    function(...) {
      get("proj_remove_internal", asNamespace("pak"))(...)
    },
    list(pkg = pkg, root = root, ask = ask))

  if (any && ask) get_confirmation("? Do you want to continue (Y/n) ")

  remo <- remote(
    function(...) get("proj_remove_internal_do", asNamespace("pak"))(...),
    list())

  invisible(remo)
}

proj_remove_internal <-  function(pkg, root, ask) {
  dirs <- proj_get_dirs(root)
  parsed <- parse_remotes(pkg)
  packages <- vcapply(parsed, "[[", "package")
  cliapp::cli_alert("Will {emph remove} {length(pkg)} packages:")
  print_package_list(packages)
  pkg_data$tmp <- list(
    packages = packages, lib = dirs$lib, root = dirs$root, refs = pkg,
    parsed = parsed)
  length(parsed) > 0
}

proj_remove_internal_do <- function() {
  tmp <- pkg_data$tmp
  suppressMessages(utils::remove.packages(tmp$packages, tmp$lib))
  remove_refs_from_description(tmp$root, tmp$parsed)
}

#' Status of packages in the project library
#'
#' @inheritParams proj_install
#' @return Data frame (tibble) the contains data about the packages
#'   installed in the project library.
#'
#' @family project functions
#' @keywords internal

proj_status <- function(root = ".") {
  remote(
    function(...) asNamespace("pak")$proj_status_internal(...),
    list(root = root))
}

proj_status_internal <- function(root) {
  dirs <- proj_get_dirs(root)
  lib_status(dirs$lib)
}

## TODO: proj_check()
## Run the solver on the project, from DESCRIPTION and see if everything
## is OK

## TODO: proj_doctor()
## Check if packages can be loaded, also for broken DLL files

## TODO: proj_prune()
## Remove unneeded packages from project library

## ----------------------------------------------------------------------

proj_get_dirs <- function(root) {
  root <- rprojroot::find_root(proj_crit(), path = root)
  mkdirp(lib <- file.path(root, "r-packages"))
  if (!file.info(lib)$isdir) stop("`", lib, "` is not a directory")
  list(root = root, lib = lib)
}

## TODO What should this be? R package only? Or we create DESCRIPTION
## files automatically?

proj_crit <- function() {
  rprojroot::has_file(".here") |
    rprojroot::is_rstudio_project |
    rprojroot::is_r_package |
    rprojroot::is_git_root |
    rprojroot::is_remake_project |
    rprojroot::is_projectile_project
}

## Rules:
## * optional = FALSE means Imports, otherwise Suggests
## * If the package is already included, we remove it first
## * If you want LinkingTo or Depends, then you'll need to add it manually
##
## TODO: messages about the changes and/or the current state
## TODO: add version requirements automatically

add_refs_to_description <- function(root, refs, optional) {
  dsc <- desc::desc(root)

  parsed <- parse_remotes(refs)

  for (i in seq_along(refs)) {
    ref <- refs[[i]]
    pkg <- parsed[[i]]$package
    type <- parsed[[i]]$type

    if (type %in% c("deps", "installed")) next

    ## Remove remotes that refer to the same package
    proj_remotes <- dsc$get_remotes()
    proj_remotes_parsed <- parse_remotes(proj_remotes)
    proj_remotes_pkgs <- vcapply(proj_remotes_parsed, "[[", "package")
    remotes_to_del <- proj_remotes[proj_remotes_pkgs == pkg]
    if (length(remotes_to_del)) dsc$del_remotes(remotes_to_del)

    ## Remove precious instance of package from dependencies
    dsc$del_dep(pkg)

    ## Add package to dependencies
    dsc$set_dep(pkg, type = if (optional) "Suggests" else "Imports")

    ## If not a standard ref, add it to 'Remotes' as well
    if (type %in% c("cran", "standard", "bioc")) next
    dsc$add_remotes(ref)
  }

  dsc$write()
}

## TODO: messages about the changes and/or the current state

remove_refs_from_description <- function(root, parsed) {
  dsc <- desc::desc(root)

  ## Remove the specified refs from Remotes
  refs <- vcapply(parsed, "[[", "ref")
  dsc$del_remotes(refs)

  ## Remove the packages from dependencies
  pkgs <- vcapply(parsed, "[[", "package")
  for (pkg in pkgs) dsc$del_dep(pkg)

  ## Remove all refs from Remotes that refer to the one of these
  ## packages
  proj_remotes <- dsc$get_remotes()
  proj_remotes_parsed <- parse_remotes(proj_remotes)
  proj_remotes_pkgs <- vcapply(proj_remotes_parsed, "[[", "package")
  remotes_to_del <- proj_remotes[proj_remotes_pkgs %in% pkgs]
  if (length(remotes_to_del)) dsc$del_remotes(remotes_to_del)

  dsc$write()
}
