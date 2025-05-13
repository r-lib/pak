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
  direct <- sum(x$direct & x$type != "deps")
  deps <- sum(!x$direct)

  newly <- sum(x$lib_status == "new" & x$type != "deps")
  upd <- sum(x$lib_status == "update" & !x$type %in% c("installed", "deps"))
  curr <- sum(x$lib_status == "current" & x$type != "deps")
  # Not used currently. The packages we could have updated but did not
  noupd <- sum(x$lib_status == "no-update")

  downloaded <- sum(x$download_status == "Got")
  cached <- sum(
    x$download_status == "Had" &
      !x$type %in% c("installed", "deps")
  )
  dlbytes <- sum(x$file_size[x$download_status == "Got"])
  total_time <- format_time$pretty_dt(attr(x, "total_time")) %||% ""

  pkgsum <- paste0(
    if (direct > 0) "{direct} pkg{?s}",
    if (direct > 0 && deps > 0) " + ",
    if (deps > 0) "{deps} dep{?s}"
  )
  updsum <- paste0(
    c(
      if (curr > 0) "kept {curr}",
      if (upd > 0) "upd {upd}",
      if (newly > 0) "added {newly}"
    ),
    collapse = ", "
  )

  dlsum <- if (downloaded > 0) {
    bytes <- format_bytes$pretty_bytes(dlbytes)
    paste0(
      ", dld {downloaded}",
      if (!is.na(bytes) && bytes != 0) " ({bytes})"
    )
  } else {
    ""
  }
  ts <- if (nzchar(total_time)) " {.timestamp {total_time}}" else ""

  cli::cli_alert_success(c(
    pkgsum,
    ": ",
    updsum,
    dlsum,
    ts
  ))
}
