#' pak SITuation REPort
#'
#' It prints
#' * pak version,
#' * platform the package was built on, and the current platform,
#' * the current library path,
#' * versions of dependencies.
#'
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

  lib <- private_lib_dir()
  if (is_load_all()) {
    cat0("* Using `load_all()` from ", find.package("pak"), ".\n")
    cat0("* Private library location:\n- ", lib, "\n")
  } else {
    cat0("* pak is installed at ", find.package("pak"), ".\n")
  }

  invisible()
}

is_load_all <- function() {
  !is.null(asNamespace("pak")[[".__DEVTOOLS__"]])
}
