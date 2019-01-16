
#' Install pkg's dependencies into its private library
#'
#' To avoid interference between your regular R packages and pkg's
#' dependencies, pkg works off a private library, which can be created
#' by `pkg_create_private_lib()`.
#'
#' @param mode Where to get the packages from. "download" will try to
#'   download them from CRAN. "copy" will try to copy them from your
#'   current "regular" package library. "auto" will try to copy first,
#'   and if that fails, then it tries to download.
#' @param quiet Whether to omit messages.
#' @return The path to the private library, invisibly.
#'
#' @seealso [pkg_sitrep()].
#'
#' @export

pkg_create_private_lib <- function(mode = c("auto", "download", "copy"),
                                quiet = FALSE) {

  mode <- match.arg(mode)

  lib <- private_lib_dir()

  if (mode == "auto" && !quiet && !testthat_testing()) {
    message(
      "\n`pkg` will create its private package library in",
      "\n`", lib, "`. ",
      "\nIt will try to copy packages from your regular library",
      "\nSee `?pkg_create_private_lib()` for alternatives.\n")

    ans <- readline("Do you want to continue (Y/n)? ")
    if (! ans %in% c("", "y", "Y")) stop("Aborted", call. = FALSE)
  }

  if (!quiet) message("\nCreating private lib in `", lib, "`...")

  done <- FALSE

  if (mode %in% c("auto", "copy")) {
    tryCatch({
      create_private_lib()
      done <- TRUE
    }, error = function(e) {
      if (mode == "copy") stop(e) else if (!quiet) print(e)
    })
  }

  if (!done) {
    tryCatch({
      download_private_lib(quiet = quiet)
      return(invisible())
    }, error = function(e) {
      stop(e)
    })
  }

  if (!quiet) message("\nCreated private lib in `", lib, "`...")

  invisible(lib)
}

#' pkg SITuation REPort
#'
#' It prints
#' * pkg version,
#' * the current library path,
#' * location of the private library,
#' * whether the pkg private library exists,
#' * whether the pkg private library is functional.
#'
#' @export

pkg_sitrep <- function() {

  ## version
  ver <- as.character(utils::packageVersion("pkg"))
  cat0("* pkg version:\n- ", ver, "\n")

  ## library path
  cat0("* Library path:\n")
  cat(paste0("- ", .libPaths()), sep = "\n")

  ## private library location
  lib <- private_lib_dir()
  cat0("* Private library location:\n- ", lib, "\n")

  ## Whether it exists
  if (has_lib <- file.exists(lib)) {
    cat0("* Private library exists.\n")

  } else {
    cat0("! Private library does not exist (create with ",
         "`pkg_create_private_lib()`)\n")
  }

  if (has_lib) {
    ret <- tryCatch({
      check_for_private_lib()
      check_private_lib()
      new_remote_session(create = FALSE)
      deps <- utils::packageDescription("pkg")$Imports
      deps <- c("pkg", parse_dep_fields(deps))
      remote(args = list(deps = deps), function(deps) {
        for (d in deps) library(d, character.only = TRUE)
      })
      TRUE
    }, error = function(e) e )

    if (isTRUE(ret)) {
      cat0("* Private library is functional\n")
    } else {
      cat0("! Private library is not functional, re-create with ",
           "`pkg_create_private_lib()`\n")
      print(ret)
    }
  }

  invisible()
}
