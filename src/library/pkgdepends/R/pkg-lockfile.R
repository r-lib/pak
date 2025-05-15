#' @param lockfile Path to the lock file to use.
#' @param config Configuration options, a named list. See
#'  ['Configuration'][pkgdepends-config]. If it does not include `library`, then
#'  `.libPaths()[1]` is added as `library`.
#' @param ... Additional arguments, passed to
#'   [`pkg_installation_plan$new()`](#method-new).
#' @return `new_pkg_installation_plan()` returns a `pkg_installation_plan`
#'   object.
#'
#' @export
#' @rdname pkg_installation_plan

new_pkg_installation_plan <- function(
  lockfile = "pkg.lock",
  config = list(),
  ...
) {
  config$library <- config$library %||% .libPaths()[[1]]
  pkg_installation_plan$new(lockfile, config = config, ...)
}

#' R6 class for installation from a lock file
#'
#' @description
#' An installation plan is similar to an installation proposal
#' (i.e. [pkg_installation_proposal]), but it already contains the solved
#' dependencies, complete with download URLs.
#'
#' @details
#' Typically you create a `pkg_installation_plan` object with
#' `new_pkg_installation_plan()` and then call its `$download()` method
#' to download the packages and then its `$install()` method to install
#' them.
#'
#' @export

pkg_installation_plan <- R6::R6Class(
  "pkg_installation_plan",
  inherit = pkg_installation_proposal,
  public = list(
    #' @description
    #' Create a new `pkg_installation_plan` object. Consider using
    #' `new_pkg_installation_plan()` instead of calling the constructor
    #' directly.
    #'
    #' The returned object can be used to download and install
    #' packages, according to the plan.
    #'
    #' @param lockfile Path to the lock file to use.
    #' @param config Configuration options. See
    #'   ['Configuration'][pkgdepends-config]. It needs to include the package
    #'   library to install to, in `library`.
    #' @param remote_types Custom remote ref types, this is for advanced
    #'   use, and experimental currently.

    initialize = function(
      lockfile = "pkg.lock",
      config = list(),
      remote_types = NULL
    ) {
      assert_that(is_path(config$library))
      private$library <- config$library
      config$goal <- "install"
      private$plan <- pkg_plan$new(
        lockfile = lockfile,
        config = config,
        library = config$library,
        remote_types = remote_types
      )
    },

    #' @description
    #' This function is implemented for installation plans, and will error.

    resolve = function() {
      throw(pkg_error(
        "Cannot resolve an installation plan, it is already resolved."
      ))
    },

    #' @description
    #' This function is implemented for installation plans, and will error.

    async_resolve = function() {
      throw(pkg_error(
        "Cannot resolve an installation plan, it is already resolved."
      ))
    },

    #' @description
    #' Installation plans are already solved, and this method will return
    #' `NA_character_`, always.

    get_solve_policy = function() NA_character_,

    #' @description
    #' This function is implemented for installation plans, and will error.

    set_solve_policy = function() {
      throw(pkg_error(
        "Cannot solve an installation plan, it is already solved."
      ))
    },

    #' @description
    #' This function is implemented for installation plans, and will error.

    solve = function() {
      throw(pkg_error(
        "Cannot solve an installation plan, it is already solved."
      ))
    },

    #' @description
    #' Update the plan to the current state of the library. If the library
    #' has not changed since the plan was created, then it does nothing.
    #' If new packages have been installed, then it might not be necessary
    #' to download and install all packages in the plan.
    #'
    #' @details
    #' This operation is different than creating a new proposal with the
    #' updated library, because it uses the the packages and package
    #' versions of the original plan. E.g. if the library has a newer
    #' version of a package, then `$update()` will downgrade it to the
    #' version in the plan.

    update = function() pkg_lockfile_update(self, private),

    #' @description
    #' Update information about installed and missing system requirements.

    update_sysreqs = function() pkg_lockfile_update_sysreqs(self, private),

    #' @description
    #' Format a `pkg_installation_plan` object, typically for printing.
    #'
    #' @param ... not used currently.
    #'
    #' @return
    #' A character vector, each element should be a line in the printout.

    format = function(...) {
      refs <- private$plan$get_refs()

      has_dls <- private$plan$has_solution_downloads()
      dls <- if (has_dls) private$plan$get_solution_download()
      dls_err <- has_dls && any(dls$status == "Failed")

      has_sys <- !is.null(private$plan$get_solution()$sysreqs)

      c(
        "<pkg_installation_plan>",
        "+ refs:",
        paste0("  - ", refs),
        "+ has solution",
        if (has_dls) "+ has downloads",
        if (dls_err) "x has download errors",
        "(use `$update()` to update the plan for an updated library)",
        "(use `$show_solution()` to see the packages to install",
        "(use `$get_solution()` to see the full solution results)",
        "(use `$draw()` to draw the dependency tree)",
        if (!has_dls) "(use `$download()` to download packages)",
        if (has_dls) "(use `$get_downloads()` to get download data)",
        if (has_dls) "(use `$get_install_plan()` to get the installation plan)",
        if (has_sys)
          "(use `$install_sysreqs()` to install system requirements)",
        if (has_dls) "(use `$install()` to install the packages)"
      )
    }
  ),

  private = list(
    plan = NULL,
    library = NULL
  )
)

pkg_lockfile_update <- function(self, private) {
  private$plan$update()
  invisible(self)
}

pkg_lockfile_update_sysreqs <- function(self, private) {
  private$plan$update_sysreqs()
  invisible(self)
}
