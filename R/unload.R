#' Unload packages
#'
#' It will unload `packages` and their loaded reverse dependencies
#' (imports, really).
#'
#' It will not unload
#' * base packages
#' * pak
#' * packages that are not loaded, obviously.
#'
#' This function is similar to `pkgload::unload()`, but it is specialized
#' to pak, in that
#' * it does not deal with packages loaded by pkgload.
#' * it will not unload base packages and pak itself.
#' * it will also unload the loaded reverse dependencies of the unloaded
#'   packages, in the right order.
#' * It never forces an unload. (It does not need to, since reverse
#'   dependencies are unloaded first.)
#'
#' @param packages [character] The packages to unload.
#' @return The unloaded packages, unvisibly.
#'
#' @noRd

unload <- function(packages, msg = "  Unloading %s...") {
  badbase <- intersect(base_packages(), packages)
  if (length(badbase) > 0) {
    packages <- setdiff(packages, badbase)
    warning("Will not unload base packages: ", paste(badbase, collapse = ", "))
  }

  if ("pak" %in% packages) {
    packages <- setdiff(packages, "pak")
    warning("Will not unload pak")
  }

  badloaded <- setdiff(packages, loadedNamespaces())
  if (length(badloaded) > 0) {
    packages <- setdiff(packages, badloaded)
    warning(
      "Will not unload packages that are load not loaded: ",
      paste(badloaded, collapse = ", ")
    )
  }

  # This will also include reverse dependencies
  packages <- unload_order_topo(packages)

  # Always run garbage collector to force any deleted external pointers to
  # finalise
  gc()

  for (pkg in packages) {
    message(sprintf(msg, pkg))
    unloadNamespace(pkg)
    unload_dll(pkg)
  }

  # TODO: summary of packages unloaded
  bad <- intersect(packages, loadedNamespaces())
  if (length(bad) > 0) {
    stop(
      "Could not unload ",
      if (length(bad) == 1) "one package: " else "some packages: ",
      paste(bad, collapse = ", ")
    )
  }
  message()

  invisible(packages)
}

#' Query additional packages that need to be unloaded
#'
#' @param packages [character] Packages that we want to unload.
#' @return [character] All packages that need to be unloaded, because
#' they (recursively) depend on `packages`.
#'
#' @noRd

needs_unload <- function(packages) {
  packages <- setdiff(packages, c(base_packages(), "pak"))
  packages <- intersect(packages, loadedNamespaces())
  unload_order_topo(packages)
}

package_imports <- function(package, base = FALSE) {
  imp <- unique(names(getNamespaceInfo(package, "imports")))
  if (!base) imp <- imp[!imp %in% base_packages()]
  # pkgload has some unnamed components somehow?
  imp[imp != ""]
}

unload_order_topo <- function(packages) {
  all <- setdiff(loadedNamespaces(), base_packages())
  imp <- lapply_with_names(all, package_imports)
  imp_by <- lapply_with_names(all, function(p) {
    all[vlapply(imp, `%in%`, x = p)]
  })

  revs <- packages
  while (length(more <- setdiff(unlist(imp_by[revs]), revs)) > 0) {
    revs <- c(revs, more)
  }
  imp_by <- lapply(imp_by, intersect, revs)[revs]

  topo <- character()
  while (length(topo) < length(revs)) {
    new <- names(imp_by)[viapply(imp_by, length) == 0]
    if (length(new) == 0) stop("Loop in package imports???")
    topo <- c(topo, new)
    imp_by <- lapply(imp_by, setdiff, new)[setdiff(names(imp_by), new)]
  }

  topo
}

unload_dll <- function(package) {
  pkglibs <- loaded_dlls(package)

  for (lib in pkglibs) {
    dyn.unload(lib[["path"]])
  }

  # Remove the unloaded dlls from .dynLibs()
  libs <- .dynLibs()
  .dynLibs(libs[!(libs %in% pkglibs)])

  invisible()
}

loaded_dlls <- function(package) {
  libs <- .dynLibs()
  matchidx <- vapply(libs, "[[", character(1), "name") == package
  libs[matchidx]
}
