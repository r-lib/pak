
## Take packages from DESCRIPTION and make sure they are installed,
## with their dependencies

#' @export

proj_install <- function(pkg = NULL, path = ".", upgrade = FALSE,
                         optional = FALSE, ask = interactive()) {

  start <- Sys.time()

  any <- remote(
    function(...) {
      get("proj_install_make_plan", asNamespace("pkgman"))(...)
    },
    list(pkg = pkg, path = path, upgrade = upgrade, ask = ask,
         start = start))

  if (any && ask) get_confirmation("? Do you want to continue (Y/n) ")

  inst <- remote(
    function(...) get("proj_install_do_plan", asNamespace("pkgman"))(...),
    list(remotes = NULL, optional = optional))

  invisible(inst)
}

## TODO: install package from path? We should probably do that?

proj_install_make_plan <- function(pkg, path, upgrade, ask, start) {
  dirs <- proj_get_dirs(path)
  ref <- pkg %||% paste0("deps::", dirs$root)
  ret <- pkg_install_make_plan(
    ref, lib = dirs$lib, upgrade = upgrade, ask = ask, start = start)
  pkgman_data$tmp$lib <- dirs$lib
  pkgman_data$tmp$root <- dirs$root
  pkgman_data$tmp$pkg <- pkg
  ret
}

proj_install_do_plan <- function(remotes, optional) {
  tmp <- pkgman_data$tmp

  res <- pkg_install_do_plan(remotes = tmp$remotes, lib = tmp$lib)

  if (!is.null(tmp$pkg)) {
    add_refs_to_description(tmp$root, tmp$pkg, optional)
  }

  res
}

## Uninstall package, remove from DESCRIPTION

#' @export

proj_remove <- function(pkg, path = ".", ask = interactive()) {

  any <-  remote(
    function(...) {
      get("proj_remove_internal", asNamespace("pkgman"))(...)
    },
    list(pkg = pkg, path = path, ask = ask))

  if (any && ask) get_confirmation("? Do you want to continue (Y/n) ")

  remo <- remote(
    function(...) get("proj_remove_internal_do", asNamespace("pkgman"))(...),
    list())

  invisible(remo)
}

proj_remove_internal <-  function(pkg, path, ask) {
  dirs <- proj_get_dirs(path)
  parsed <- pkgdepends::parse_remotes(pkg)
  packages <- vcapply(parsed, "[[", "package")
  cliapp::cli_alert("Will {emph remove} {length(pkg)} packages:")
  print_package_list(packages)
  pkgman_data$tmp <- list(
    packages = packages, lib = dirs$lib, root = dirs$root, refs = pkg,
    parsed = parsed)
  length(parsed) > 0
}

proj_remove_internal_do <- function() {
  tmp <- pkgman_data$tmp
  suppressMessages(utils::remove.packages(tmp$packages, tmp$lib))
  remove_refs_from_desciption(tmp$root, tmp$parsed)
}

#' @export

proj_status <- function(path = ".") {
  remote(
    function(...) asNamespace("pkgman")$proj_status_intenal(...),
    list(path = path))
}

proj_status_internal <- function(path) {
  dirs <- proj_get_dirs(path)
  pkgdepends::lib_status(dirs$lib)
}

## TODO: proj_check()
## Run the solver on the project, from DESCRIPTION and see if everything
## is OK

## TODO: proj_doctor()
## Check if packages can be loaded, also for broken DLL files

## TODO: proj_prune()
## Remove unneeded packages from project library

## ----------------------------------------------------------------------

proj_get_dirs <- function(path) {
  root <- rprojroot::find_root(proj_crit(), path = path)
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

  parsed <- pkgdepends::parse_remotes(refs)

  for (i in seq_along(refs)) {
    ref <- refs[[i]]
    pkg <- parsed[[i]]$package
    type <- parsed[[i]]$type

    if (type %in% c("deps", "installed")) next

    ## Remove remotes that refer to the same package
    proj_remotes <- dsc$get_remotes()
    proj_remotes_parsed <- pkgdepends::parse_remotes(proj_remotes)
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
  proj_remotes_parsed <- pkgdepends::parse_remotes(proj_remotes)
  proj_remotes_pkgs <- vcapply(proj_remotes_parsed, "[[", "package")
  remotes_to_del <- proj_remotes[proj_remotes_pkgs %in% pkgs]
  if (length(remotes_to_del)) dsc$del_remotes(remotes_to_del)

  dsc$write()
}
