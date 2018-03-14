
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
#' @importFrom prettyunits pretty_bytes pretty_dt

print.pkgman_install_result <- function(x, ...) {

  direct <- sum(x$direct)
  deps <- sum(! x$direct)

  newly <- sum(x$lib_status == "new")
  upd   <- sum(x$lib_status == "update")
  curr  <- sum(x$lib_status == "current")
  noupd <- sum(x$lib_status == "no-update")

  downloaded <- sum(x$download_status == "Got")
  cached <- sum(x$download_status == "Had" & x$type != "installed")
  dlbytes <- sum(x$bytes[x$download_status == "Got"])
  build_time <- sum(unlist(x$build_time), na.rm = TRUE)
  inst_time <- sum(unlist(x$install_time), na.rm = TRUE)
  total_time <- pretty_dt(attr(x, "total_time")) %||% "???s"

  cli$alert_success(paste0(
    direct, " + ", deps, " pkgs | ",
    "kept ", curr, ", updated ", upd, ", new ", newly, " | ",
    "downloaded ", downloaded, " (", pretty_bytes(dlbytes), ")",
    " [{timestamp {total_time}}]"))
}

ask_for_confirmation <- function(ask, sol, lib) {

  direct <- sum(sol$direct)
  deps <- sum(! sol$direct)

  n_newly <- sum(newly <- sol$lib_status == "new")
  n_upd   <- sum(upd   <- sol$lib_status == "update")
  n_curr  <- sum(curr  <- sol$lib_status == "current")
  n_noupd <- sum(noupd <- sol$lib_status == "no-update")

  if (! (n_newly + n_upd)) return()

  package_list <- function(x) {
    cli$div(
      class = "pkglist",
      theme = list(div.pkglist = list("margin-left" = 2))
    )
    cli$text(paste(x, collapse = ", "))
    cli$text(" ")
  }

  cli$text(" ")
  if (n_newly) {
    cli$alert("Will {emph install} {n_newly} packages:")
    package_list(sol$ref[newly])
  }
  if (n_upd) {
    cli$alert("Will {emph update} {n_upd} packages:")
    package_list(sol$ref[upd])
  }
  if (n_curr + n_noupd) {
    cli$alert("Will {emph not update} {n_curr + n_noupd} packages.")
    cli$text(" ")
  }

  warn_for_loaded_packages(sol$package[newly | upd], lib)

  if (ask) {
    yesno(
      paste0(crayon::yellow("?"), " Do you want to continue? (Y/n) "),
      "Installation aborted")
  }

  cli$text(" ")
}

warn_for_loaded_packages <- function(pkgs, lib) {
  if (length(maybe_bad <- intersect(pkgs, loadedNamespaces()))) {
    loaded_from <- vcapply(
      maybe_bad,
      function(x) dirname(getNamespaceInfo(x, "path"))
    )
    bad <- maybe_bad[normalizePath(loaded_from) == normalizePath(lib)]
    if (length(bad)) {
      cli$alert_warning(
        "Package(s) {format_items(bad)} are already loaded, installing \\
         them may cause problems. Use {code pkgload::unload()} to unload them.",
        wrap = TRUE
      )
      cli$text(" ")
    }
  }
}

yesno <-  function(q, msg = "Aborted.") {
  ans <- readline(q)
  if (! tolower(ans) %in% c("", "y", "yes", "yeah", "yep")) {
    stop(msg, call. = FALSE)
  }
}
