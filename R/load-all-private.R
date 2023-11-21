load_all_private <- function() {
  if (length(pkg_data[["ns"]]) > 0) {
    return()
  }
  lib <- private_lib_dir()
  deps_path <- file.path(lib, "deps.rds")
  pkg_data[["ns"]] <- readRDS(deps_path)
  parent.env(pkg_data[["ns"]]) <- getNamespace(.packageName)
  for (pkg in names(pkg_data[["ns"]])) {
    pkg_env <- pkg_data[["ns"]][[pkg]]
    reg_prefix <- if (pkg == "processx") "c_" else ""
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

  cldir <- pkg_data[["ns"]][["processx"]][["__pkg-dir__"]]
  Sys.setenv("CALLR_PROCESSX_CLIENT_LIB" = cldir)
  Sys.setenv("PKGCACHE_NO_PILLAR" = "true")
  for (pkg in names(pkg_data[["ns"]])) {
    pkg_env <- pkg_data[["ns"]][[pkg]]
    if (".onLoad" %in% names(pkg_env)) {
      pkg_env[[".onLoad"]](lib, pkg)
    }
  }
}
