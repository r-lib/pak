
#' R platforms
#'
#' @details
#' `system_r_platform()` detects the platform of the current R version.
#' `system_r_platform_data()` is similar, but returns the raw data instead
#' of a character scalar.
#'
#' By default pak works with source packages and binary packages for
#' the current platform. You can change this, by providing different
#' platform names in the [`pkg.platforms`][pak-config] option or the
#' [`PKG_PLATFORMS`][pak-config] environment variable.
#'
#' This option may contain the following platform names:
#' * `"source"` for source packages,
#' * `"macos"` for macOS binaries that are appropriate for the R versions
#'   pak is working with. Packages for incompatible CPU architectures are
#'   dropped (defaulting to the CPU of the current macOS machine and x86_64 on
#'   non-macOS systems). The macOS Darwin version is selected based on the
#'   CRAN macOS binaries. E.g. on R 3.5.0 macOS binaries
#'   are built for macOS El Capitan.
#' * `"windows"` for Windows binaries for the default CRAN architecture.
#'   This is currently Windows Vista for all supported R versions, but it
#'   might change in the future. The actual binary packages in the
#'   repository might support both 32 bit and 64 builds, or only one of
#'   them. In practice 32-bit only packages are very rare. CRAN builds
#'   before and including R 4.1 have both architectures, from R 4.2 they
#'   are 64 bit only. `"windows"` is an alias to `i386+x86_64-w64-mingw32`
#'   currently.
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
#' @return `system_r_platform()` returns a character scalar.
#'
#' `system_r_platform_data()` returns a data frame with character
#' scalar columns:
#'   * `cpu`,
#'   * `vendor`,
#'   * `os`,
#'   * `distribution` (only on Linux),
#'   * `release` (only on Linux),
#'   * `platform`: the concatenation of the other columns, separated by
#'     a dash.
#' @export
#' @seealso These function call [pkgcache::current_r_platform()] and
#' [pkgcache::current_r_platform_data()].
#' @examplesIf FALSE
#' system_r_platform()
#' system_r_platform_data()

system_r_platform <- function() {
  remote(
    function(...) asNamespace("pak")$system_r_platform_internal(...),
    list()
  )
}

system_r_platform_internal <- function() {
  pkgcache::current_r_platform()
}

#' @export
#' @rdname system_r_platform

system_r_platform_data <- function() {
  remote(
    function(...) asNamespace("pak")$system_r_platform_data_internal(...),
    list()
  )
}

system_r_platform_data_internal <- function() {
  pkgcache::current_r_platform_data()
}
