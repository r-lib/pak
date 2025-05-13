#' Create a lock file
#'
#' The lock file can be used later, possibly in a new R session, to carry
#' out the installation of the dependencies, with
#' [lockfile_install()].
#'
#' Note, since the URLs of CRAN and most CRAN-like repositories change
#' over time, in practice you cannot use the lock file _much_ later.
#' For example, binary packages of older package version
#' might be deleted from the repository, breaking the URLs in the
#' lock file.
#'
#' Currently the intended use case of lock files in on CI systems, to
#' facilitate caching. The (hash of the) lock file provides a good key
#' for caching systems.
#'
#' @param lockfile Path to the lock file.
#' @inheritParams pkg_install
#'
#' @family lock files
#' @export

lockfile_create <- function(
  pkg = "deps::.",
  lockfile = "pkg.lock",
  lib = NULL,
  upgrade = FALSE,
  dependencies = NA
) {
  ret <- remote(
    function(...) {
      get("lockfile_create_internal", asNamespace("pak"))(...)
    },
    list(
      pkg = pkg,
      lockfile = lockfile,
      lib = lib,
      upgrade = upgrade,
      dependencies = dependencies
    )
  )

  invisible(ret)
}

lockfile_create_internal <- function(
  pkg,
  lockfile,
  lib,
  upgrade,
  dependencies
) {
  if (is.null(lib)) {
    lib <- tempfile()
    mkdirp(lib)
    on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  }

  cli::cli_progress_step(
    "Creating lockfile {.path {lockfile}}",
    msg_done = "Created lockfile {.path {lockfile}}"
  )

  prop <- pkgdepends::new_pkg_installation_proposal(
    pkg,
    config = list(library = lib, dependencies = dependencies)
  )
  prop$set_solve_policy(if (upgrade) "upgrade" else "lazy")
  prop$solve()
  prop$stop_for_solution_error()
  prop$create_lockfile(lockfile)
  invisible()
}

#' Install packages based on a lock file
#'
#' Install a lock file that was created with [lockfile_create()].
#'
#' @param lockfile Path to the lock file.
#' @param lib Library to carry out the installation on.
#' @param update Whether to online install the packages that
#'   either not installed in `lib`, or a different version is installed
#'   for them.
#'
#' @family lock files
#' @export

lockfile_install <- function(
  lockfile = "pkg.lock",
  lib = .libPaths()[1],
  update = TRUE
) {
  start <- Sys.time()
  mkdirp(lib)
  ret <- remote(
    function(...) {
      get("lockfile_install_internal", asNamespace("pak"))(...)
    },
    list(
      lockfile = lockfile,
      lib = lib,
      update = update,
      start = start,
      loaded = loaded_packages(lib)
    )
  )

  invisible(ret)
}

lockfile_install_internal <- function(lockfile, lib, update, loaded, start) {
  cli::cli_alert_info("Installing lockfile {.path {lockfile}}")

  config <- list(library = lib)
  plan <- pkgdepends::new_pkg_installation_plan(lockfile, config = config)
  if (update) plan$update()

  print_install_details(plan, lib, loaded)

  plan$download()
  plan$install_sysreqs()
  inst <- plan$install()
  attr(inst, "total_time") <- Sys.time() - start
  class(inst) <- c("pkg_install_result", class(inst))

  ## Remove some largeish columns that we don't really need any more
  inst$extra <- NULL

  ## One line summary of the install
  print_install_summary(inst)

  cli::cli_alert_success("Installed lockfile {.path {lockfile}}")

  inst
}
