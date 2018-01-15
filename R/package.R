#' Install a package
#'
#' Install a package and it's dependencies.
#' @param pkg A package specification. See [pkgdepends::remotes] for details.
#' @inheritParams pkginstall::install_packages
#' @importFrom pkgdepends remotes
#' @importFrom pkginstall install_packages
#' @importFrom crayon blue
#' @export
pkg_install <- function(pkg, lib = .libPaths()[[1L]], num_workers = 1L) {
  r <- remotes$new(pkg, library = lib)

  # Solve the dependency graph
  r$solve()
  r$stop_for_solve_error()

  # Actually download packages as needed
  r$download_solution()
  r$stop_for_solution_download_error()

  # Get the installation plan and ignore already installed versions.
  plan <- r$get_install_plan()
  needs_install <- plan[plan$type != "installed", ]

  # Do we need to do anything?
  if (nrow(needs_install) == 0) {
    if (is_verbose()) {
      plan$num_deps <- total_num_deps(plan)
      dir <- plan[plan$direct, ]
      ind <- plan[!plan$direct, ]
      for (i in seq_len(nrow(dir))) {
        msg_success("{fmt_pkg(dir$package[i])} {fmt_ver(dir$version[i])} \\
                   and {dir$num_deps[i]} dependencies already installed")
      }
    }

  } else {
    # Remove already installed dependencies from the plan
    installed <- plan$package[plan$type == "installed"]
    needs_install$dependencies <-
      lapply(needs_install$dependencies, setdiff, y = installed)

    # Install what is left
    inst_stat <- install_packages(
      needs_install$file, lib = lib, plan = needs_install,
      num_workers = num_workers)

    if (is_verbose()) print(inst_stat)

    inst_lib <- match(plan$file, names(inst_stat))
    plan$installed[match(names(inst_stat), plan$file)] <- unlist(inst_stat)
  }

  invisible(plan)
}

#' Install a local development package
#' @param path to the local package
#' @inheritParams pkginstall::install_packages
#' @importFrom pkgdepends remotes
#' @export
local_pkg_install <- function(path = ".", lib = .libPaths()[[1L]], num_workers = 1L) {

  # Construct a local spec
  pkg <- paste0("local::", normalizePath(path))

  pkg_install(remotes$new(pkg, library = lib), lib = lib, num_workers = num_workers)
}

#' Display installed locations of a package
#'
#' @param pkg Name of an installed package to display status for.
#' @param lib One or more library paths to lookup package status in.
#' @importFrom tibble tibble
#' @importFrom stats setNames
#' @importFrom utils packageDescription
#' @export
pkg_status <- function(pkg, lib = .libPaths()) {
  stopifnot(length(pkg == 1) && is.character(pkg))

  desc <- lapply(lib, function(lib) {
    res <- suppressWarnings(packageDescription(pkg, lib.loc = lib, fields = c("Version", "Built")))
  })
  found <- !is.na(desc)
  versions <- vcapply(desc[found], "[[", "Version")
  built <- split_built(vcapply(desc[found], "[[", "Built"))
  tibble(!!! append(list(library = lib[found], version = versions), built))
}

split_built <- function(built) {
  nms <- c("build_r_version", "build_platform", "build_date", "build_os")
  if (length(built) < 1) {
    return(setNames(list(character(0), character(0), character(0), character(0)), nms))
  }
  setNames(as.list(strsplit(built, "; ")[[1L]]), nms)
}

#' Remove installed packages
#'
#' @param pkg A character vector of packages to remove.
#' @param lib library to remove packages from
#' @importFrom utils remove.packages
#' @export
pkg_remove <- function(pkg, lib = .libPaths()[[1L]]) {
  # TODO: do we need to do anything else for this?
  suppressMessages(remove.packages(pkg, lib))
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
