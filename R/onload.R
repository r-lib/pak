
pkg_data <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  utils::data(
    pak_sitrep_data,
    package = pkgname,
    envir = environment(.onLoad)
  )

  check_platform(libname, pkgname)
  pkg_data$ns <- list()

  worker <- Sys.getenv("R_PKG_PKG_WORKER", "")
  if (worker == "") {
    ## In the main process
    fix_macos_path_in_rstudio()

  } else if (worker == "true") {
    ## In the worker process
    Sys.setenv("R_PKG_PKG_WORKER" = "false")
    # We don't use the env vars that cli supports, on purpose, because
    # they are inherited in the subprocess of the subprocess
    options(
      cli.num_colors = as.numeric(Sys.getenv("R_PKG_NUM_COLORS", "1")),
      rlib_interactive = (Sys.getenv("R_PKG_INTERACTIVE") == "TRUE"),
      cli.dynamic = (Sys.getenv("R_PKG_DYNAMIC_TTY") == "TRUE")
    )
    ca_path <- system.file(package = "pak", "curl-ca-bundle.crt")
    if (ca_path != "") options(async_http_cainfo = ca_path)
    use_private_lib()

  } else {
    ## In a subprocess of a worker
    use_private_lib()
  }

  invisible()
}

check_platform <- function(libname = dirname(find.package("pak")),
                           pkgname = "pak", data = pak_sitrep_data) {
  # Is this load_all()?
  if (!file.exists(file.path(libname, pkgname, "help"))) return(TRUE)

  # Is this during installation?
  if (Sys.getenv("R_PACKAGE_DIR", "") != "") return(TRUE)

  current <- current_r_platform_data()
  install <- parse_platform(data$platform)

  if (!platform_match(install, current)) {
    warning(
      "! Wrong OS or architecture, pak is probably dysfunctional.\n",
      "  Call `pak_update()` to fix this.",
      call. = FALSE
    )
  }
}

platform_match <- function(install, current) {
  # Example platform strings:
  # - x86_64-w64-mingw32                         (Windows Server 2008, 64 bit build)
  # - i386-w64-mingw32                           (Windows Server 2008, 32 bit build)
  # - x86_64-apple-darwin17.0                    (macOS High Sierra)
  # - aarch64-apple-darwin20                     (macOS amd64 Big Sur)
  # - aarch64-apple-darwin21                     (macOS amd64 Monterey)
  # - x86_64-pc-linux-gnu-fedora-33              (Fedora Linux)
  # - x86_64-pc-linux-gnu-alpine-3.11.11         (Alpine Linux, older R)
  # - x86_64-pc-linux-musl-alpine-3.14.1         (Alpine Linux, newer R)  
  # - s390x-ibm-linux-gnu-ubuntu-18.04           (Ubuntu on S390x)
  # - powerpc64le-unknown-linux-gnu-ubuntu-18.04 (Ubuntu on ppc64le)
  # - aarch64-unknown-linux-gnu-ubuntu-18.04     (Ubuntu on arm)
  # - i386-pc-solaris2.10                        (32 bit Solaris 10, gcc)
  # - i386-pc-solaris2.10                        (64 bit Solaris 10, gcc, by mistake)
  # - i386-pc-solaris2.10                        (32 bit Solaris 10, ods)
  # - amd64-portbld-freebsd12.1                  (x86_64 FreeBSD 12.x, R from ports)

  # Shortcut, this happens quite often
  if (install$platform == current$platform) return(TRUE)

  if (current$os == "linux" || grepl("linux-", current$os)) {
    platform_match_linux(install, current)
  } else if (grepl("darwin", current$os)) {
    platform_match_macos(install, current)
  } else if (current$os == "mingw32") {
    platform_match_windows(install, current)
  } else {
    platform_match_other(install, current)
  }
}

platform_match_linux <- function(install, current) {
  # Must be Linux, the same arch
  if (install$os != "linux" && !grepl("linux-", install$os)) return(FALSE)
  if (install$cpu != current$cpu) return(FALSE)

  # Distros must match, but accept alpine
  libc_ins <- get_libc_from_os(install$os)
  install$os == current$os || identical(libc_ins, "musl")
}

platform_match_macos <- function(install, current) {
  if (! grepl("darwin", install$os)) return(FALSE)
  if (install$cpu != current$cpu) return(FALSE)

  # An older build on newer darwin is probably fine,
  # e.g. darwin17.0 vs darwin20
  install$os <= current$os
}

platform_match_windows <- function(install, current) {
  if (install$os != "windows") return (FALSE)

  # Usually OK, but if there is a CPU mismatch, we'd ideally check if
  # the current cpu is also supported, but we don't have a good way of
  # doing that currently.
  TRUE
}

platform_match_other <- function(install, current) {
  # Must match exactly?
  install$platform == current$platform
}

get_libc_from_os <- function(x) {
  pcs <- strsplit(x, "-", fixed = TRUE)[[1]]
  if (pcs[1] != "linux") return(NA_character_)
  pcs[2]
}
