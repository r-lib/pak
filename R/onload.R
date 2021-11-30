
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


}

platform_match_macos <- function(install, current) {

}

platform_match_windows <- function(install, current) {

}

platform_match_other <- function(install, current) {

}

  # OS must match, but linux-musl might be good for linux-gnu
  os_ins <- install$os
  os_cur <- current$os
  ins_linux <- install$os == "linux" || grepl("linux-", install$os)
  cur_linux <- current$os == "linux" || grepl("linux-", current$os)
  both_linux <- ins_linux && cur_linux
  if (install$os != current$os && !both_linux) return(FALSE)

  # If it is Windows, then all should be good in general, but on 32 bit
  # R we need to check that 32bit Windows is supported by this pak build.
  if (os_ins == "mingw32") return(TODO)

  # On macOS we need to check the arch, and current must not be older
  ins_darwin <- grepl("^darwin", install$os)
  cur_darwin <- grepl("^darwin", current$os)
  both_darwin <- ins_darwin && cur_darwin
  if (both_darwin) return(arch_ins == arch_cur)

  # If it is Solaris, then arch must match. Btw. our 64 bit build has the
  # same platform string as the 32 bit build, which is probably a bug.
  if (os_ins == "solaris") return(arch_ins == arch_cur)

  # If it is Linux, then arch must match, if libc is musl, that's ok,
  # because that's probably our static build
  if (os_ins == "linux") {
    if (arch_ins != arch_cur) return(FALSE)
    libc_ins <- get_libc_from_platform(install)
    libc_cur <- get_libc_from_platform(current)
    same <- !is.na(libc_ins) && !is.na(libc_cur) && libc_ins == libc_cur
    return(same || identical(libc_ins,  "musl"))
  }

  # Otherwise, the whole platform string must match. We might improve
  # this in the future.
  install == current
}

get_arch_from_platform <- function(x) {
  pcs <- strsplit(x, "-", fixed = TRUE)[[1]]
  pcs[1]
}

get_libc_from_platform <- function(x) {
  pcs <- strsplit(x, "-", fixed = TRUE)[[1]]
  if (pcs[3] != "linux") return(NA_character_)
  pcs[4]
}
