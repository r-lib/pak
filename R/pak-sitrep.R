#' pak SITuation REPort
#'
#' It prints
#' * pak version,
#' * platform the package was built on, and the current platform,
#' * the current library path,
#' * versions of dependencies,
#' * whether dependencies can be loaded.
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
    "- pak platform: ",
    ver$platform,
    " (current: ",
    R.Version()$platform,
    if (comp) ", compatible",
    ")\n"
  )
  if (!comp) cat0("- platform is incompatible!\n")
  repo <- ver$`github-repository`
  sha <- ver$`github-sha`
  if (repo != "r-lib/pak") {
    cat0(
      "- pak repository: ",
      repo,
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
  if (!is.null(asNamespace("pak")[[".__DEVTOOLS__"]])) {
    cat0("* Using `load_all()` from ", find.package("pak"), ".\n")
    cat0("* Private library location:\n- ", lib, "\n")
  } else {
    cat0("* pak is installed at ", find.package("pak"), ".\n")
  }

  dver <- pak_library_versions(lib)
  cat0("* Dependency versions:\n")
  cat0(paste0(
    "- ",
    format(dver$package),
    " ",
    format(dver$version),
    ifelse(is.na(dver$sha), "", paste0(" (", substr(dver$sha, 1, 7), ")")),
    "\n"
  ))

  ret <- tryCatch(
    {
      new_remote_session()
      # TODO: check that all packages can be loaded in subprocess
      TRUE
    },
    error = function(e) e
  )

  if (isTRUE(ret)) {
    cat0("* Dependencies can be loaded\n")
  } else {
    cat("! Could not load dependencies, pak installation is broken. :(\n")
    cat0("Error: ", conditionMessage(ret))
  }

  invisible()
}

pak_library_versions <- function(lib) {
  pkgs <- dir(lib)
  vers <- lapply(pkgs, function(pkg) get_ver(file.path(lib, pkg)))
  data.frame(
    stringsAsFactors = FALSE,
    package = pkgs,
    version = vcapply(vers, "[", 1),
    sha = vcapply(vers, "[", 2)
  )
}

# this is slightly different than the one in install-embedded.R

get_ver <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  desc <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc)) {
    return(NA_character_)
  }
  dsc <- read.dcf(desc)
  ver <- package_version(dsc[, "Version"])
  devver <- as.numeric(ver[1, 4])
  if (!is.na(devver) && devver >= 9000) {
    if ("RemoteSha" %in% colnames(dsc)) {
      sha <- dsc[, "RemoteSha"]
      return(c(ver, sha))
    }
  }

  as.character(ver)
}
