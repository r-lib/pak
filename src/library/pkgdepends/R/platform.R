#' @details
#' `current_r_platform()` detects the platform of the current R version.
#'
#' By default pkgdepends works with source packages and binary packages for
#' the current platform. You can change this, see
#' ['Configuration'][pkgdepends-config].
#'
#' The following platform names can be configured and returned by
#' `current_r_platform()` and `default_platforms()`:
#' * `"source"` for source packages,
#' * A platform string like `R.version$platform`, but on Linux the name
#'   and version of the distribution are also included. Examples:
#'   - `x86_64-apple-darwin17.0`: macOS High Sierra.
#'   - `aarch64-apple-darwin20`: macOS Big Sur on arm64.
#'   - `x86_64-w64-mingw32`: 64 bit Windows.
#'   - `i386-w64-mingw32`: 32 bit Windows.
#'   - `i386+x86_64-w64-mingw32`: 64 bit + 32 bit Windows.
#'   - `i386-pc-solaris2.10`: 32 bit Solaris. (Some broken 64 Solaris
#'     builds might have the same platform string, unfortunately.)
#'   - `x86_64-pc-linux-gnu-debian-10`: Debian Linux 10 on x86_64.
#'   - `x86_64-pc-linux-musl-alpine-3.14.1`: Alpine Linux.
#'   - `x86_64-pc-linux-gnu-unknown`: Unknown Linux Distribution on x86_64.
#'   - `s390x-ibm-linux-gnu-ubuntu-20.04`: Ubuntu Linux 20.04 on S390x.
#'   - `amd64-portbld-freebsd12.1`: FreeBSD 12.1 on x86_64.
#'
#' In addition, the following platform names can be used to configure
#' pkgdepends:
#' * `"macos"` for macOS binaries that are appropriate for the R versions
#'   pkgdepends is working with (defaulting to the version of the current
#'   session), as defined by CRAN binaries. E.g. on R 3.5.0 macOS binaries
#'   are built for macOS El Capitan.
#' * `"windows"` for Windows binaries for the default CRAN architecture.
#'   This is currently Windows Vista for all supported R versions, but it
#'   might change in the future. The actual binary packages in the
#'   repository might support both 32 bit and 64 builds, or only one of
#'   them. In practice 32-bit only packages are very rare. CRAN builds
#'   before and including R 4.1 have both architectures, from R 4.2 they
#'   are 64 bit only. `"windows"` is an alias to `i386+x86_64-w64-mingw32`
#'   currently.
#'
#' @return
#' `current_r_platform()` returns a string, the name of the current
#' platform.
#'
#' @export
#' @rdname default_platforms

current_r_platform <- function() {
  pkgcache::current_r_platform()
}

#' R platforms
#'
#'`default_platfoms()` returns the default platforms for the current R
#' session. These typically consist of the detected platform of the current
#' R session, and `"source"`, for source packages.
#'
#' @return
#' `default_platforms()` returns a character vector of platform names.
#'
#' @family platform functions
#' @export
#' @examples
#' current_r_platform()
#' default_platforms()

default_platforms <- function() unique(c(current_r_platform(), "source"))

# Is `cand` an OK platform for `exp`? This is pretty straightforward,
# except for windows.
#
# NOTE: RSPM delivers binaries as source packages, so we
#       do not need to handle RSPM specially, source packages
#       will be always accepted, unless the user explicitly
#       opts out from them.

platform_is_ok <- function(cand, exp, exp_archs = NULL) {
  if (cand %in% c("*", "source") && "source" %in% exp) return(TRUE)

  # This is an installed linux package, probably, prefix is OK, e.g.
  # cand = x86_64-pc-linux-gnu vs exp = x86_64-pc-linux-gnu-ubuntu-18.04
  if (grepl("-linux-", cand) && any(substr(exp, 1, nchar(cand)) == cand)) {
    return(TRUE)
  }

  if (cand %in% c("i386+x86_64-w64-mingw32", "x86_64+i386-w64-mingw32")) {
    # This is a multi-arch binary, that is OK, if binaries are allowed
    any(c("x86_64-w64-mingw32", "i386-w64-mingw32") %in% exp)
  } else if (cand == "x86_64-w64-mingw32") {
    # This is an x64 only binary. If we are on x64 and we "prefer-x64"
    # then it is OK. Otherwise we would prefer a multi-arch binary or
    # a source package.
    "x86_64-w64-mingw32" %in% exp && exp_archs == "prefer-x64"
  } else if (cand == "i386-w64-mingw32") {
    # This is an i386 only binary. This is not OK currently.
    # (We do not allow an i386-only installation.)
    FALSE
  } else {
    # Otherwise it is just a match
    cand %in% exp
  }
}
