#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  set_pkg_config_path()
}

set_pkg_config_path <- function() {
  if (
    Sys.getenv("PKG_BUILD_IGNORE_PKG_CONFIG_PATH") != "true" &&
      .Platform$OS.type == "windows" &&
      Sys.which("pkg-config") != "" &&
      getRversion()[, 1:2] == "4.2" &&
      Sys.which("gcc") != "" &&
      grepl(
        "/rtools42/",
        normalizePath(Sys.which("gcc"), winslash = "/"),
        ignore.case = TRUE
      ) &&
      !grepl("/rtools42", Sys.getenv("PKG_CONFIG_PATH"), ignore.case = TRUE)
  ) {
    old <- Sys.getenv("PKG_CONFIG_PATH")
    new <- paste0(
      "/c/rtools42/x86_64-w64-mingw32.static.posix/lib/pkgconfig",
      ":",
      old
    )
    message("Updating PKG_CONFIG_PATH.\nOld: ", old, "\nNew: ", new)
    Sys.setenv(PKG_CONFIG_PATH = new)
  }
}
