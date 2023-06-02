
should_ask_confirmation <- function(sol) {
  # We should ask if at least one package is updated
  any(sol$lib_status == "update")
}

print_install_details <- function(prop, lib, loaded) {
  cli::cli_div(
    theme = list(
      "div.alert-warning" = list("margin-top" = 1, "margin-bottom" = 1)
    )
  )

  sol <- prop$get_solution()$data
  direct <- sum(sol$direct)
  deps <- sum(! sol$direct)

  n_newly <- sum(newly <- sol$lib_status == "new")
  n_upd   <- sum(upd   <- sol$lib_status == "update")
  n_curr  <- sum(curr  <- sol$lib_status == "current")
  n_noupd <- sum(noupd <- sol$lib_status == "no-update")

  cli::cli_verbatim(" ")

  if (n_newly) {
    cli::cli_alert("Will {.emph install} {n_newly} package{?s}.")
  }
  if (n_upd) {
    cli::cli_alert("Will {.emph update} {n_upd} package{?s}.")
  }

  w_dl <- sol$cache_status == "miss" & !is.na(sol$cache_status)
  w_ch <- sol$cache_status == "hit" & !is.na(sol$cache_status)
  n_dl <- sum(w_dl, na.rm = TRUE)
  u_dl <- sum(is.na(sol$filesize[w_dl]))
  n_ch <- sum(w_ch, na.rm = TRUE)
  b_dl <- prettyunits::pretty_bytes(sum(sol$filesize[w_dl], na.rm = TRUE))
  b_ch <- prettyunits::pretty_bytes(sum(sol$filesize[w_ch], na.rm = TRUE))

  any_unk <- length(u_dl) > 0

  if (n_dl == 0) {
    if (n_ch > 0) {
      if (n_ch == 1) {
        cli::cli_alert("The package ({b_ch}) is cached.")
      } else {
        cli::cli_alert("All {n_ch} packages ({b_ch}) are cached.")
      }
    }

  } else if (n_ch == 0) {
    if (n_dl -  u_dl > 0) {
      cli::cli_alert("Will {.emph download} {n_dl - u_dl} CRAN package{?s} ({b_dl}).")
    }
    if (u_dl > 0) {
      cli::cli_alert("Will {.emph download} {u_dl} package{?s} with unknown size.")
    }

  } else if (!any_unk) {
    cli::cli_alert(
      "Will {.emph download} {n_dl} package{?s} ({b_dl}), cached: {n_ch} ({b_ch}).")

  } else {
    if (n_dl - u_dl > 0) {
      cli::cli_alert(
        "Will {.emph download} {n_dl - u_dl} CRAN package{?s} ({b_dl}), cached: {n_ch} ({b_ch}).")
    }
    if (u_dl > 0) {
      cli::cli_alert("Will {.emph download} {u_dl} package{?s} with unknown size.")
    }
  }

  if (n_newly + n_upd > 0) {
    prop$show_solution(key = FALSE)
  }

  sysreqs <- prop$get_sysreqs()
  if (!is.null(sysreqs)) {
    num <- length(sysreqs$miss) + length(sysreqs$upd)
    if (length(sysreqs$inst) > 0 && num == 0) {
      cli::cli_alert_success("All system requirements are already installed.")
    } else if (num > 0) {
      cli::cli_alert("Will {.emph install} {num} system package{?s}:")
    }
    prop$show_sysreqs()
  }

  if (length(loaded) > 0 || get_os() == "win") {
    ls <- warn_for_loaded_packages(sol$package[newly | upd], lib, loaded)
  } else {
    ls <- list(status = "none-none", pkgs = sol$package, lib = lib)
  }

  should_ask <- should_ask_confirmation(sol)
  invisible(list(should_ask = should_ask, loaded_status = ls))
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

get_answer <- function(answers, prompt = NULL) {
  prompt <- prompt %||% paste0("? Your choice [", answers[1], "]: ")
  while (TRUE) {
    ans <- readline(prompt)
    ans <- str_trim(ans)
    if (ans == "") ans <- answers[1]
    if (ans %in% answers) return(ans)
  }
}

offer_restart <- function(unloaded) {
  message("\n",
          "! pak had to unload some packages before installation, and the\n",
          "  current R session may be unstable. It is best to restart R now.\n"
  )

  rs <- rstudio$detect()$type

  if (rs == "rstudio_console") {
    message(
      "  1. Restart R without saving data.\n",
      "  2. Save data to `.RData` and restart R.\n",
      "  3. Do not restart R.\n"
    )
    ans <- get_answer(1:3)
    if (ans == "1") {
      rstudioapi::restartSession()

    } else if (ans == "2") {
      message("Saving workspace to .RData...")
      save.image()
      rstudioapi::restartSession()

    } else if (ans == "3") {
      invisible("OK")

    }
  }
}
