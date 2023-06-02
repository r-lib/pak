
sysreqs_is_supported <- function(platform = NULL) {
  remote(
    function(...) pkgdepends::sysreqs_is_supported(...),
    list(platform = platform)
  )
}

sysreqs_platforms <- function() {
  remote(
    function() pkgdepends::sysreqs_platforms()
  )
}

sysreqs_list_system_packages <- function() {
  load_extra("pillar")
  remote(
    function() pkgdepends::sysreqs_list_system_packages()
  )
}

sysreqs_db_match <- function(specs, platform = NULL) {
  load_extra("pillar")
  remote(
    function(...) pkgdepends::sysreqs_db_match(...),
    list(specs = specs, platform = platform)
  )
}

sysreqs_db_update <- function() {
  invisible(remote(
    function() pkgdepends::sysreqs_db_update()
  ))
}

sysreqs_db_list <- function(platform = NULL) {
  load_extra("pillar")
  remote(
    function(...) pkgdepends::sysreqs_db_list(...),
    list(platform = platform)
  )
}

sysreqs_check_installed <- function(packages = NULL,
                                    library = .libPaths()[1]) {
  load_extra("pillar")
  remote(
    function(...) {
      ret <- pkgdepends::sysreqs_check_installed(...)
      asNamespace("pak")$pak_preformat(ret)
    },
    list(packages = packages, library = library)
  )
}
