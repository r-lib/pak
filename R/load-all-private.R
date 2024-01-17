load_all_private <- function() {
  if (length(pkg_data[["ns"]]) > 0) {
    return()
  }
  lib <- private_lib_dir()
  if (Sys.getenv("TEST_COVERAGE_PAK") == "true") {
    deps_path <- file.path(lib, "deps-covr.rds")
    cnt_path <- file.path(lib, "deps-cnt.rds")
    asNamespace("covrlabs")$add_counters(readRDS(cnt_path))
  } else {
    deps_path <- file.path(lib, "deps.rds")
  }
  pkg_data[["ns"]] <- readRDS(deps_path)
  parent.env(pkg_data[["ns"]]) <- getNamespace(.packageName)
  # These register C functions with a c_ prefix
  prefix_pkgs <- c("filelock", "processx", "zip")
  for (pkg in names(pkg_data[["ns"]])) {
    pkg_env <- pkg_data[["ns"]][[pkg]]
    reg_prefix <- if (pkg %in% prefix_pkgs) "c_" else ""
    parent.env(pkg_env) <- getNamespace(.packageName)

    pkg_env[["__pkg-dir__"]] <- normalizePath(file.path(lib, pkg))

    dll_file <- paste0(pkg, .Platform$dynlib.ext)
    dll_path <- file.path(lib, pkg, "libs", .Platform$r_arch, dll_file)
    if (file.exists(dll_path)) {
      # TODO: copy it on windows (or always?)
      dll <- dyn.load(dll_path)
      dll[["name"]] <- paste0("pak-", dll[["name"]])
      .dynLibs(c(.dynLibs(), list(dll)))
      natfuns <- getDLLRegisteredRoutines(dll)$.Call
      for (natfun in natfuns) {
        pkg_env[[paste0(reg_prefix, natfun$name)]] <- natfun
      }
    }
  }

  # This is use in .onLoad() in pkgcache
  pcnp <- Sys.getenv("PKGCACHE_NO_PILLAR")
  if (pcnp == "") {
    on.exit(Sys.unsetenv("PKGCACHE_NO_PILLAR"), add = TRUE)
  } else {
    on.exit(Sys.setenv(PKGCACHE_NO_PILLAR = pcnp), add = TRUE)
  }
  Sys.setenv("PKGCACHE_NO_PILLAR" = "true")

  for (pkg in names(pkg_data[["ns"]])) {
    pkg_env <- pkg_data[["ns"]][[pkg]]
    if (".onLoad" %in% names(pkg_env)) {
      pkg_env[[".onLoad"]](lib, pkg)
    }
  }
}
