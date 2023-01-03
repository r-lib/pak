
pkg_data <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  utils::data(
    pak_sitrep_data,
    package = pkgname,
    envir = environment(.onLoad)
  )

  if (Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") == "") {
    check_platform(libname, pkgname)
  }
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
    cainfo <- getOption("async_http_cainfo")
    if (is.null(cainfo) && ca_path != "") options(async_http_cainfo = ca_path)
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

  current <- R.Version()$platform
  install <- data$platform

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
  # - x86_64-w64-mingw32            (Windows Server 2008, 64 bit build)
  # - i386-w64-mingw32              (Windows Server 2008, 32 bit build)
  # - x86_64-apple-darwin17.0       (macOS High Sierra)
  # - x86_64-pc-linux-gnu           (Fedora Linux, older Alpine Linux)
  # - x86_64-pc-linux-musl          (newer Alpine Linux)
  # - s390x-ibm-linux-gnu           (Ubuntu on S390x)
  # - powerpc64le-unknown-linux-gnu (Ubuntu on ppc64le)
  # - aarch64-unknown-linux-gnu     (Ubuntu on arm)
  # - i386-pc-solaris2.10           (32 bit Solaris 10, gcc)
  # - i386-pc-solaris2.10           (64 bit Solaris 10, gcc, by mistake)
  # - i386-pc-solaris2.10           (32 bit Solaris 10, ods)
  # - amd64-portbld-freebsd12.1     (x86_64 FreeBSD 12.x, R from ports)

  os_ins <- get_os_from_platform(install)
  os_cur <- get_os_from_platform(current)
  arch_ins <- get_arch_from_platform(install)
  arch_cur <- get_arch_from_platform(current)

  # OS must match in the first place
  if (os_ins != os_cur) return(FALSE)

  # If it is Windows, then all should be good in general. Ideally we would
  # check if the current arch is really available.
  # both 32 bit or 64 bit
  if (os_ins == "windows") return(TRUE)

  # If it is macOS, then all should be good, still, but as a preparation
  # for arm, we check the arch
  if (os_ins == "macos") return(arch_ins == arch_cur)

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

get_os_from_platform <- function(x) {
  pcs <- strsplit(x, "-", fixed = TRUE)[[1]]
  if (pcs[3] == "mingw32") return("windows")
  if (pcs[2] == "apple") return("macos")
  if (pcs[3] == "linux") return("linux")
  if (grepl("^solaris", pcs[3])) return("solaris")
  sub("[0-9.]*$", "", pcs[3])
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
