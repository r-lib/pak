#' Is Rtools installed?
#'
#' To build binary packages on windows, Rtools (found at
#' \url{https://CRAN.R-project.org/bin/windows/Rtools/}) needs to be on
#' the path. The default installation process does not add it, so this
#' script finds it (looking first on the path, then in the registry).
#' It also checks that the version of rtools matches the version of R.
#' `has_rtools()` determines if Rtools is installed, caching the results.
#' Afterward, run `rtools_path()` to find out where it's installed.
#'
#' @section Acknowledgements:
#'   This code borrows heavily from RStudio's code for finding Rtools.
#'   Thanks JJ!
#' @param debug If `TRUE`, will print out extra information useful for
#'   debugging. If `FALSE`, it will use result cached from a previous run.
#' @return Either a visible `TRUE` if rtools is found, or an invisible
#'   `FALSE` with a diagnostic [message()].
#'   As a side-effect the internal package variable `rtools_path` is
#'   updated to the paths to rtools binaries.
#' @keywords internal
#' @export
#' @examples
#' has_rtools()
has_rtools <- function(debug = FALSE) {
  if (!debug && rtools_path_is_set()) {
    return(!identical(rtools_path(), ""))
  }

  if (!is_windows()) {
    return(FALSE)
  }

  # R 4.5.0 or later on ARM64
  if (getRversion() >= "4.5.0" && grepl("aarch", R.version$platform)) {
    rtools45_aarch64_home <- Sys.getenv("RTOOLS45_AARCH64_HOME", "C:\rtools45-aarch64")
    if (file.exists(file.path(rtools45_aarch64_home, "usr", "bin"))) {
      if (debug) {
        cat("Found in Rtools 4.5 (aarch64) installation folder\n")
      }
      rtools_path_set(rtools(rtools45_aarch64_home, "4.5"))
      return(TRUE)
    }
  }

  # R 4.5.0 or later
  if (getRversion() >= "4.5.0" && !grepl("aarch", R.version$platform)) {
    rtools45_home <- Sys.getenv("RTOOLS45_HOME", "C:\\rtools45")
    if (file.exists(file.path(rtools45_home, "usr", "bin"))) {
      if (debug) {
        cat("Found in Rtools 4.5 installation folder\n")
      }
      rtools_path_set(rtools(rtools45_home, "4.5"))
      return(TRUE)
    }
  }

  # R 4.4.0 or later on ARM64
  if (getRversion() >= "4.4.0" && getRversion() < "4.5.0" &&
      grepl("aarch", R.version$platform)) {
    rtools44_aarch64_home <- Sys.getenv("RTOOLS44_AARCH64_HOME", "C:\rtools44-aarch64")
    if (file.exists(file.path(rtools44_aarch64_home, "usr", "bin"))) {
      if (debug) {
        cat("Found in Rtools 4.4 (aarch64) installation folder\n")
      }
      rtools_path_set(rtools(rtools44_aarch64_home, "4.4"))
      return(TRUE)
    }
  }

  # R 4.4.0 or later
  if (getRversion() >= "4.4.0" && getRversion() < "4.5.0" &&
      !grepl("aarch", R.version$platform)) {
    rtools44_home <- Sys.getenv("RTOOLS44_HOME", "C:\\rtools44")
    if (file.exists(file.path(rtools44_home, "usr", "bin"))) {
      if (debug) {
        cat("Found in Rtools 4.4 installation folder\n")
      }
      rtools_path_set(rtools(rtools44_home, "4.4"))
      return(TRUE)
    }
  }

  # R 4.3.0 or later on ARM64
  if (getRversion() >= "4.3.0" && getRversion() < "4.4.0" &&
      grepl("aarch", R.version$platform)) {
    rtools43_aarch64_home <- Sys.getenv("RTOOLS43_AARCH64_HOME", "C:\rtools43-aarch64")
    if (file.exists(file.path(rtools43_aarch64_home, "usr", "bin"))) {
      if (debug) {
        cat("Found in Rtools 4.3 (aarch64) installation folder\n")
      }
      rtools_path_set(rtools(rtools43_aarch64_home, "4.3"))
      return(TRUE)
    }
  }

  # R 4.3.0 or later
  if (getRversion() >= "4.3.0" && getRversion() < "4.4.0" &&
      !grepl("aarch", R.version$platform)) {
    rtools43_home <- Sys.getenv("RTOOLS43_HOME", "C:\\rtools43")
    if (file.exists(file.path(rtools43_home, "usr", "bin"))) {
      if (debug) {
        cat("Found in Rtools 4.3 installation folder\n")
      }
      rtools_path_set(rtools(rtools43_home, "4.3"))
      return(TRUE)
    }
  }

  # R 4.2.x or later and ucrt?
  ucrt <- is_ucrt()
  if (getRversion() >= "4.2.0" && getRversion() < "4.3.0") {
    if (ucrt) {
      rtools42_home <- Sys.getenv("RTOOLS42_HOME", "C:\\rtools42")
      if (file.exists(file.path(rtools42_home, "usr", "bin"))) {
        if (debug) {
          cat("Found in Rtools 4.2 installation folder\n")
        }
        rtools_path_set(rtools(rtools42_home, "4.2"))
        return(TRUE)
      }
    }
  }

  # In R 4.0 we can use RTOOLS40_HOME, recent versions of Rtools40 work fine
  # with ucrt as well, currently.
  if (getRversion() >= "4.0.0" && getRversion() < "4.3.0") {
    rtools40_home <- Sys.getenv("RTOOLS40_HOME", "C:\\rtools40")
    fld <- if (ucrt) "ucrt64" else "usr"
    if (file.exists(file.path(rtools40_home, fld, "bin"))) {
      if (debug) {
        cat("Found in Rtools 4.0 installation folder\n")
      }
      rtools_path_set(rtools(rtools40_home, "4.0"))
      return(TRUE)
    }
  }

  # First, R CMD config CC --------------------------------------------
  # This does not work if 'make' is not yet on the path
  from_config <- scan_config_for_rtools(debug)
  if (is_compatible(from_config)) {
    if (debug) {
      cat("Found compatible gcc from R CMD config CC\n")
    }
    rtools_path_set(from_config)
    return(TRUE)
  }

  # Next, try the path ------------------------------------------------
  from_path <- scan_path_for_rtools(debug)
  if (is_compatible(from_path)) {
    if (debug) {
      cat("Found compatible gcc on path\n")
    }
    rtools_path_set(from_path)
    return(TRUE)
  }

  if (!is.null(from_path)) {
    # Installed
    if (is.null(from_path$version)) {
      # but not from rtools
      if (debug) {
        cat("gcc and ls on path, assuming set up is correct\n")
      }
      return(TRUE)
    } else {
      # Installed, but not compatible
      needed <- rtools_needed()
      message(
        "WARNING: Rtools ", from_path$version, " found on the path",
        " at ", from_path$path, " is not compatible with R ", getRversion(), ".\n\n",
        "Please download and install ", needed, " from ", rtools_url(needed),
        ", remove the incompatible version from your PATH."
      )
      return(invisible(FALSE))
    }
  }

  # Next, try the registry --------------------------------------------------
  registry_candidates <- scan_registry_for_rtools(debug)

  if (length(registry_candidates) == 0) {
    # Not on path or in registry, so not installled
    needed <- rtools_needed()
    message(
      "WARNING: Rtools is required to build R packages, but is not ",
      "currently installed.\n\n",
      "Please download and install ", needed, " from ", rtools_url(needed), "."
    )
    return(invisible(FALSE))
  }

  from_registry <- Find(is_compatible, registry_candidates, right = TRUE)
  if (is.null(from_registry)) {
    # In registry, but not compatible.
    versions <- vapply(registry_candidates, function(x) x$version, character(1))
    needed <- rtools_needed()
    message(
      "WARNING: Rtools is required to build R packages, but no version ",
      "of Rtools compatible with R ", getRversion(), " was found. ",
      "(Only the following incompatible version(s) of Rtools were found: ",
      paste(versions, collapse = ", "), ")\n\n",
      "Please download and install ", needed, " from ", rtools_url(needed), "."
    )
    return(invisible(FALSE))
  }

  # On Rtools 3.x do an extra check if the installed version is accurate.
  # With rtools40 this is no longer needed (it doesn't have a Version.txt)
  if (isTRUE(from_registry$version < "4")) {
    installed_ver <- installed_version(from_registry$path, debug = debug)
    if (is.null(installed_ver)) {
      # Previously installed version now deleted
      needed <- rtools_needed()
      message(
        "WARNING: Rtools is required to build R packages, but the ",
        "version of Rtools previously installed in ", from_registry$path,
        " has been deleted.\n\n",
        "Please download and install ", needed, " from ", rtools_url(needed), "."
      )
      return(invisible(FALSE))
    }

    if (installed_ver != from_registry$version) {
      # Installed version doesn't match registry version
      needed <- rtools_needed()
      message(
        "WARNING: Rtools is required to build R packages, but no version ",
        "of Rtools compatible with R ", getRversion(), " was found. ",
        "Rtools ", from_registry$version, " was previously installed in ",
        from_registry$path, " but now that directory contains Rtools ",
        installed_ver, ".\n\n",
        "Please download and install ", needed, " from ", rtools_url(needed), "."
      )
      return(invisible(FALSE))
    }
  }

  # Otherwise it must be ok :)

  # Recently Rtools is versioned properly
  from_registry$version <- sub(
    "^([0-9]+[.][0-9]+)[.].*$", "\\1",
    from_registry$version
  )
  rtools_path_set(from_registry)
  TRUE
}

is_ucrt <- function() {
  identical(R.Version()$crt, "ucrt")
}

#' @rdname has_rtools
#' @usage NULL
#' @export
find_rtools <- has_rtools

#' @rdname has_rtools
#' @usage NULL
#' @export
setup_rtools <- has_rtools

#' @export
#' @rdname has_rtools
check_rtools <- function(debug = FALSE) {
  if (is_windows() && !has_rtools(debug = debug)) {
    stop("Rtools is not installed.", call. = FALSE)
  }

  TRUE
}

installed_version <- function(path, debug) {
  if (!file.exists(file.path(path, "Rtools.txt"))) {
    return(NULL)
  }

  # Find the version path
  version_path <- file.path(path, "VERSION.txt")
  if (debug) {
    cat("VERSION.txt\n")
    cat(readLines(version_path), "\n")
  }
  if (!file.exists(version_path)) {
    return(NULL)
  }

  # Rtools is in the path -- now crack the VERSION file
  contents <- NULL
  try(contents <- readLines(version_path), silent = TRUE)
  if (is.null(contents)) {
    return(NULL)
  }

  # Extract the version
  contents <- gsub("^\\s+|\\s+$", "", contents)
  version_re <- "Rtools version (\\d\\.\\d+)\\.[0-9.]+$"

  if (!grepl(version_re, contents)) {
    return(NULL)
  }

  m <- regexec(version_re, contents)
  regmatches(contents, m)[[1]][2]
}

is_compatible <- function(rtools) {
  if (is.null(rtools)) {
    return(FALSE)
  }
  if (is.null(rtools$version)) {
    return(FALSE)
  }

  stopifnot(is.rtools(rtools))
  version <- rtools$version
  version <- sub("^([0-9]+[.][0-9]+)[.].*$", "\\1", version)
  info <- version_info[[version]]
  if (is.null(info)) {
    return(FALSE)
  }

  r_version <- getRversion()
  r_version >= info$version_min && r_version <= info$version_max
}

rtools <- function(path, version, ...) {
  structure(list(version = version, path = path, ...), class = "rtools")
}
is.rtools <- function(x) inherits(x, "rtools")

#' Retrieve a text string with the rtools version needed
#'
#' @keywords internal
#' @export
rtools_needed <- function(r_version = getRversion()) {
  vi <- version_info
  vi$custom <- NULL

  for (i in rev(seq_along(vi))) {
    version <- names(vi)[i]
    info <- vi[[i]]
    ok <- r_version >= info$version_min && r_version <= info$version_max
    if (ok) {
      return(paste("Rtools", version))
    }
  }
  "the appropriate version of Rtools"
}

rtools_url <- function(needed) {
  "https://cran.r-project.org/bin/windows/Rtools/"
}
