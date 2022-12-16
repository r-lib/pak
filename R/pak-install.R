
#' Set up private pak library (deprecated)
#'
#' This function is deprecated and does nothing.
#' Recent versions of pak do not need a `pak_setup()` call.
#'
#' @param mode Where to get the packages from. "download" will try to
#'   download them from CRAN. "copy" will try to copy them from your
#'   current "regular" package library. "auto" will try to copy first,
#'   and if that fails, then it tries to download.
#' @param quiet Whether to omit messages.
#' @return The path to the private library, invisibly.
#'
#' @export

pak_setup <- function(mode = c("auto", "download", "copy"), quiet = FALSE) {
  warning("`pak_setup()` is deprecated and does nothing.")
  return(invisible())
}

#' pak SITuation REPort
#'
#' It prints
#' * pak version,
#' * the current library path,
#' * location of the private library,
#' * whether the pak private library exists,
#' * whether the pak private library is functional.
#'
#' @aliases pak_sitrep_data
#' @export
#' @family pak housekeeping
#' @section Examples:
#' ```{asciicast pak-sitrep}
#' pak_sitrep()
#' ```

pak_sitrep <- function() {

  ## version
  ver <- as.character(utils::packageVersion("pak"))
  cat0("* pak version:\n- ", ver, "\n")

  ## platform data
  ver <- pak_sitrep_data
  plt <- R.Version()$platform
  comp <- platform_match(ver$platform, plt)
  cat0("* Version information:\n")
  cat0(
    "- pak platform: ", ver$platform,
    " (current: ", R.Version()$platform, if (comp) ", compatible", ")\n"
  )
  if (!comp) cat0("- platform is incompatible!\n")
  repo <- ver$`github-repository`
  sha <- ver$`github-sha`
  if (repo != "r-lib/pak") {
    cat0(
      "- pak repository: ", repo,
      if (repo == "-") " (local install?)",
      "\n"
    )
    }
  if (repo != "-" || sha != "-") cat0("- pak sha: ", sha, "\n")

  ## recommended packages
  xpkgs <- extra_packages()
  if (length(xpkgs)) {
    xinst <- pkg_is_installed(xpkgs)
    if (any(xinst)) {
      cat0("* Optional packages installed:\n")
      cat0(paste0("- ", xpkgs[xinst], "\n"))
    }
    if (any(!xinst)) {
      cat0("* Optional packages missing:\n")
      cat0(paste0("- ", xpkgs[!xinst], "\n"))
    }
  }

  ## library path
  cat0("* Library path:\n")
  cat(paste0("- ", .libPaths()), sep = "\n")

  ## private library location
  lib <- private_lib_dir()

  if (identical(names(lib), "embedded")) {
    if (ver$failed) {
      cat0("* Private library is embedded, but dysfunctional, try `pak_update()` to repair.\n")
    } else {
      cat0("* Private library is embedded.\n")
    }

  } else {
    cat0("* Private library location:\n- ", lib, "\n")

    ## Whether it exists
    if (has_lib <- file.exists(lib)) {
      cat0("* Private library exists.\n")

    } else {
      cat0("! Private library does not exist (create with ",
           "`craete_dev_lib()`)\n")
    }

    if (has_lib) {
      ret <- tryCatch({
        new_remote_session()
        # TODO: check that all packages can be loaded in subprocess
        TRUE
      }, error = function(e) e )

      if (isTRUE(ret)) {
        cat0("* Private library is functional\n")
      } else {
        cat0("! Private library is not functional, re-create with ",
             "`create_dev_lib(clean = TRUE)`\n")
        cat0("Error: ", conditionMessage(ret))
      }
    }
  }

  invisible()
}
