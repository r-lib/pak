
#' Install pak's dependencies into its private library
#'
#' To avoid interference between your regular R packages and pak's
#' dependencies, pak works off a private library, which can be created
#' by `pak_setup()`.
#'
#' @param mode Where to get the packages from. "download" will try to
#'   download them from CRAN. "copy" will try to copy them from your
#'   current "regular" package library. "auto" will try to copy first,
#'   and if that fails, then it tries to download.
#' @param quiet Whether to omit messages.
#' @return The path to the private library, invisibly.
#'
#' @export
#' @family pak housekeeping

pak_setup <- function(mode = c("auto", "download", "copy"),
                      quiet = FALSE) {

  mode <- match.arg(mode)

  lib <- private_lib_dir()

  if (identical(names(lib), "embedded")) {
    message("Private library is embedded, pak is ready to go.")
    return(invisible())
  }

  # Only ask if private library doesn't already exist
  if (!file.exists(lib)) {
    if (mode == "auto" && !quiet && !testthat_testing()) {
      message(
        "\n`pak` needs to create a private package library in",
        "\n`", lib, "`. ",
        "\nIt will try to copy packages from your regular library",
        "\nSee `?pak_setup()` for alternatives.\n")

      ans <- readline("Do you want to continue (Y/n)? ")
      if (! ans %in% c("", "y", "Y")) stop("Aborted", call. = FALSE)
    }

    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  }

  done <- FALSE
  if (mode %in% c("auto", "copy")) {
    tryCatch({
      create_private_lib(quiet = quiet)
      done <- TRUE
    }, error = function(e) {
      if (mode == "copy") stop(e) else if (!quiet) print(e)
    })
  }

  if (!done) {
    if (!quiet) message("\nInstalling packages into private lib")
    download_private_lib(quiet = quiet)
  }

  invisible(lib)
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
#' @export
#' @family pak housekeeping

pak_sitrep <- function() {

  ## version
  ver <- as.character(utils::packageVersion("pak"))
  cat0("* pak version:\n- ", ver, "\n")

  ## platform data
  if (is.null(pkg_data$pak_version)) {
    cat0("* No version information, loaded with `load_all()`?\n")
  } else {
    ver <- pkg_data$pak_version
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
  }

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
    cat0("* Private library is embedded.\n")

  } else {
    cat0("* Private library location:\n- ", lib, "\n")

    ## Whether it exists
    if (has_lib <- file.exists(lib)) {
      cat0("* Private library exists.\n")

    } else {
      cat0("! Private library does not exist (create with ",
           "`pak_setup()`)\n")
    }

    if (has_lib) {
      ret <- tryCatch({
        check_for_private_lib()
        check_private_lib()
        new_remote_session(create = FALSE)
        deps <- utils::packageDescription("pak")$Imports
        deps <- c("pak", parse_dep_fields(deps))
        remote(args = list(deps = deps), function(deps) {
          for (d in deps) library(d, character.only = TRUE)
        })
        TRUE
      }, error = function(e) e )

      if (isTRUE(ret)) {
        cat0("* Private library is functional\n")
      } else {
        cat0("! Private library is not functional, re-create with ",
             "`pak_setup()`\n")
        cat0("Error: ", conditionMessage(ret))
      }
    }
  }

  invisible()
}
