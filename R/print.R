
## Cases:
## - was already installed, current
## - was already installed, not current
## - updated, downloaded
## - updated, from cache
## - newly installed, downloaded
## - newly installed, from cache
##
## Show information:
## - number of direct refs
## - number of dependencies
## - number of packages already installed, current
## - number of pkgs updated
## - number of pkgs newly installed
## - number of packages downloaded
## - amount of data downloaded
## - number of packages cached
## - number of packages not current
## - total time
## - resolution time
## - download time (TODO)
## - build time
## - install time

## 10 pkgs, 25 deps | current: 4, updated: 3, not updated: 2, new: 12
## downloaded: 2 (541 kB), cached: 33
## in 34s, resolve: 12s, download: 15s, build: 5s, install: 2s

#' @export

print.pkg_install_result <- function(x, ...) {
  nice_df_print(x, ...)
}

nice_df_print <- function(x, ...) {
  old <- options(max.print = 100)
  on.exit(options(old))
  print.data.frame(x)
  invisible(x)
}

print_install_summary <- function(x) {
  direct <- sum(x$direct)
  deps <- sum(! x$direct)

  newly <- sum(x$lib_status == "new")
  upd   <- sum(x$lib_status == "update")
  curr  <- sum(x$lib_status == "current")
  noupd <- sum(x$lib_status == "no-update")

  downloaded <- sum(x$download_status == "Got")
  cached <- sum(x$download_status == "Had" &
                ! x$type %in% c("installed", "deps"))
  dlbytes <- sum(x$file_size[x$download_status == "Got"])
  build_time <- sum(unlist(x$build_time), na.rm = TRUE)
  inst_time <- sum(unlist(x$install_time), na.rm = TRUE)
  total_time <- prettyunits::pretty_dt(attr(x, "total_time")) %||% "???s"

  cliapp::cli_alert_success(paste0(
    direct, " + ", deps, " pkgs | ",
    "kept ", curr, ", updated ", upd, ", new ", newly, " | ",
    "downloaded ", downloaded, " (", prettyunits::pretty_bytes(dlbytes), ")",
    " {timestamp {total_time}}"))
}

warn_for_loaded_packages <- function(pkgs, lib) {
  if (length(maybe_bad <- intersect(pkgs, loadedNamespaces()))) {
    loaded_from <- vcapply(
      maybe_bad,
      function(x) dirname(getNamespaceInfo(x, "path"))
    )
    bad <- maybe_bad[normalizePath(loaded_from) == normalizePath(lib)]
    bad <- setdiff(bad, "pak")
    if (length(bad)) {
      cliapp::cli_alert_warning(
        "Package(s) {format_items(bad)} are already loaded, installing \\
         them may cause problems. Use {code pkgload::unload()} to unload them.",
        wrap = TRUE
      )
      cliapp::cli_text(" ")
    }
  }
}
