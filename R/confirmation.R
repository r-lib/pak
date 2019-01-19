
print_package_list <- function(x) {
  cliapp::cli_div(
    class = "pkglist",
    theme = list(div.pkglist = list("margin-left" = 2)))
  cliapp::cli_text(paste(x, collapse = ", "))
  cliapp::cli_text()
}

print_install_details <- function(sol, lib) {
  direct <- sum(sol$direct)
  deps <- sum(! sol$direct)

  n_newly <- sum(newly <- sol$lib_status == "new")
  n_upd   <- sum(upd   <- sol$lib_status == "update")
  n_curr  <- sum(curr  <- sol$lib_status == "current")
  n_noupd <- sum(noupd <- sol$lib_status == "no-update")

  if (! (n_newly + n_upd)) return(FALSE)

  cliapp::cli_text(" ")
  if (n_newly) {
    cliapp::cli_alert("Will {emph install} {n_newly} packages:")
    print_package_list(sol$ref[newly])
  }
  if (n_upd) {
    cliapp::cli_alert("Will {emph update} {n_upd} packages:")
    print_package_list(sol$ref[upd])
  }
  if (n_curr + n_noupd) {
    cliapp::cli_alert("Will {emph not update} {n_curr + n_noupd} packages.")
    cliapp::cli_text(" ")
  }

  warn_for_loaded_packages(sol$package[newly | upd], lib)

  w_dl <- sol$cache_status == "miss" & !is.na(sol$cache_status)
  w_ch <- sol$cache_status == "hit" & !is.na(sol$cache_status)
  n_dl <- sum(w_dl, na.rm = TRUE)
  u_dl <- sum(is.na(sol$filesize[w_dl]))
  n_ch <- sum(w_ch, na.rm = TRUE)
  b_dl <- prettyunits::pretty_bytes(sum(sol$filesize[w_dl], na.rm = TRUE))
  b_ch <- prettyunits::pretty_bytes(sum(sol$filesize[w_ch], na.rm = TRUE))

  any_unk <- length(u_dl) > 0

  if (n_dl == 0) {
    cliapp::cli_alert("All {n_ch} packages ({b_ch}) are cached.")

  } else if (n_ch == 0) {
    if (n_dl -  u_dl > 0) {
      cliapp::cli_alert("Will {emph download} {n_dl - u_dl} CRAN packages ({b_dl}).")
    }
    if (u_dl > 0) {
      cliapp::cli_alert("Will {emph download} {u_dl} packages with unknown size.")
    }

  } else if (!any_unk) {
    cliapp::cli_alert(
      "Will {emph download} {n_dl} packages ({b_dl}), cached: {n_ch} ({b_ch}).")

  } else {
    if (n_dl - u_dl > 0) {
      cliapp::cli_alert(
        "Will {emph download} {n_dl - u_dl} CRAN packages ({b_dl}), cached: {n_ch} ({b_ch}).")
    }
    if (u_dl > 0) {
      cliapp::cli_alert("Will {emph download} {u_dl} packages with unknown size.")
    }
  }
  cliapp::cli_text(" ")

  invisible(TRUE)
}

get_confirmation <-  function(q, msg = "Aborted.") {
  ans <- readline(q)
  if (! tolower(ans) %in% c("", "y", "yes", "yeah", "yep")) {
    stop(msg, call. = FALSE)
  }
}

get_confirmation2 <- function(q = "? Do you want to continue (Y/n) ") {
  ans <- readline(q)
  tolower(ans) %in% c("", "y", "yes", "yeah", "yep")
}
