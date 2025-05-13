warn_for_loaded_packages <- function(pkgs, lib, loaded, pid = NULL) {
  if (length(pkgs) == 0) return(list(status = "none-none"))
  if (get_os() == "win") {
    warn_for_loaded_packages_win(pkgs, lib, loaded, pid)
  } else {
    warn_for_loaded_packages_unix(pkgs, lib, loaded, pid)
  }
}

handle_status <- function(status, lib, ask) {
  loaded_status <- status$loaded_status
  sts <- NULL
  while (ask && grepl("locked", loaded_status$status)) {
    status$should_ask <- FALSE
    ans <- get_answer(loaded_status$answers)
    sts <- loaded_packages_response(loaded_status, ans)
    if (sts$status != "try-again") break
    loaded_status <- remote(
      function(...) get("warn_for_loaded_packages", asNamespace("pak"))(...),
      list(loaded_status$pkgs, lib, loaded_packages(lib))
    )
  }

  any <- status$should_ask
  if (any && ask) {
    get_confirmation("? Do you want to continue (Y/n) ")
  }

  invisible(sts)
}

# -- Unix ------------------------------------------------------------

warn_for_loaded_packages_unix <- function(pkgs, lib, loaded, pid) {
  if (is.null(loaded)) return()
  bad <- intersect(pkgs, loaded)
  bad <- setdiff(bad, "pak")
  if (length(bad)) warn_for_loaded_packages_emit(bad)
  list(status = "none-none")
}

warn_for_loaded_packages_emit <- function(pkgs) {
  cli::cli_alert_warning(
    "{.pkg {pkgs}} {?is/are} loaded in the current R session, \\
       you probably need to restart R after the installation.",
    wrap = TRUE
  )
}

# -- Windows ---------------------------------------------------------

warn_for_loaded_packages_win <- function(pkgs, lib, loaded, pid = NULL) {
  locked <- get_locked_libs(lib, pkgs)
  current <- loaded_status_current(pkgs, loaded, locked, pid = pid)
  others <- loaded_status_others(locked, pid = pid)
  status <- list(
    pkgs = pkgs,
    lib = lib,
    current = current,
    others = others,
    status = paste0(current$status, "-", others$status)
  )

  status$answers <- if (current$status == "none" && others$status == "none") {
    # Nothing to do
    NULL
  } else if (current$status == "loaded" && others$status == "none") {
    warn_for_loaded_packages_loaded_none(current)
  } else if (current$status == "locked" && others$status == "none") {
    warn_for_loaded_packages_locked_none(current)
  } else if (current$status == "none" && others$status == "locked") {
    warn_for_loaded_packages_none_locked(others)
  } else if (current$status == "loaded" && others$status == "locked") {
    warn_for_loaded_packages_loaded_locked(current, others)
  } else if (current$status == "locked" && others$status == "locked") {
    warn_for_loaded_packages_locked_locked(current, others)
  }

  invisible(status)
}

warn_for_loaded_packages_loaded_none <- function(current) {
  warn_for_loaded_packages_emit(current$loaded)
  NULL
}

warn_for_loaded_packages_locked_none <- function(current) {
  cli::cli_alert_warning(
    "{.pkg {current$locked}} {?is/are} loaded in the current \\
    R session and {?has/have} locked files in your library. The \\
    installation will probably fail, unless pak unloads {?it/them}, \\
    and also all other packages importing {?it/them}.",
    wrap = TRUE
  )

  cli::cli_div(
    theme = list(
      body = list("margin-left" = 2L),
      par = list("margin-top" = 1L),
      ol = list("margin-top" = 1L)
    )
  )
  cli::cli_par()
  cli::cli_text("What do you want to do?")
  cli::cli_ol(c(
    "Have pak unload them before the installation. (Safest option.)",
    "Try the installation without unloading them",
    "Abort the installation."
  ))
  c("1", "2", "3")
}

warn_for_loaded_packages_none_locked <- function(others) {
  pcs <- others$users[!duplicated(others$users$pid), , drop = FALSE]
  sess <- sprintf("%s %s", pcs$name, pcs$pid)
  apps <- ifelse(
    pcs$pid == pcs$app_pid,
    "",
    sprintf(" in %s (%s)", pcs$app_name, pcs$app_pid)
  )
  sess <- paste0(sess, apps)
  cli::cli_alert_warning(
    "{.pkg {others$locked}} {?is/are} loaded in other R sessions and \\
    {?has/have} locked files in your library. The installation will \\
    probably fail, unless you quit from {cli::qty(length(sess))} \\
    {?this/these} R session{?s}:",
    wrap = TRUE
  )

  cli::cli_div(
    theme = list(
      body = list("margin-left" = 2L),
      par = list("margin-top" = 1L),
      ol = list("margin-top" = 1L),
      ul = list("margin-top" = 1L, "margin-bottom" = 1L)
    )
  )
  cli::cli_par()
  cli::cli_ul(sess)
  cli::cli_text("What do you want to do?")
  cli::cli_ol(c(
    "Quit these R sessions and try the installation again \\
    (Safest option.)",
    "Terminate the listed R sessions. They may lose data!",
    "Try the installation anyway.",
    "Abort the installation."
  ))
  c("1", "2", "3", "4")
}

warn_for_loaded_packages_loaded_locked <- function(current, others) {
  warn_for_loaded_packages_emit(current$loaded)
  cli::cli_verbatim("")
  warn_for_loaded_packages_none_locked(others)
}

warn_for_loaded_packages_locked_locked <- function(current, others) {
  pcs <- others$users[!duplicated(others$users$pid), , drop = FALSE]
  sess <- sprintf("%s (%s)", pcs$name, pcs$pid)
  apps <- ifelse(
    pcs$pid == pcs$app_pid,
    "",
    sprintf(", in %s (%s)", pcs$app_name, pcs$app_pid)
  )
  sess <- paste0(sess, apps)
  if (identical(sort(current$locked), sort(others$locked))) {
    cli::cli_alert_warning(
      "{.pkg {current$locked}} {?is/are} loaded in the current R \\
      session and in other R sessions and {?has/have} locked files \\
      in your library. The installation will probably fail unless \\
      pak unloads {?it/them} from the current R session and you
      quit from {cli::qty(length(sess))} {?this/these} R session{?s}:",
      wrap = TRUE
    )
  } else {
    cli::cli_alert_warning(
      "{.pkg {current$locked}} {?is/are} loaded in the current R \\
      session and {?has/have} locked files in your library. The \\
      installation will fail unless pak unloads it.",
      wrap = TRUE
    )
    cli::cli_verbatim("")
    cli::cli_alert_warning(
      "{.pkg {others$locked}} {?is/are} loaded in other R sessions \\
      and {?has/have} locked files in your library. The installation \\
      will fail unless you quit from {cli::qty(length(sess))} \\
      {?this/these} R session{?s}:",
      wrap = TRUE
    )
  }

  cli::cli_div(
    theme = list(
      body = list("margin-left" = 2L),
      par = list("margin-top" = 1L),
      ol = list("margin-top" = 1L),
      ul = list("margin-top" = 1L, "margin-bottom" = 1L)
    )
  )
  cli::cli_par()
  cli::cli_ul(sess)
  cli::cli_text("What do you want to do?")
  cli::cli_ol(c(
    "Quit the listed R sessions and then have pak unload the \\
    packages from the current R session (Safest option.)",
    "Terminate the listed R sessions (they may lose data!) and have \\
    pak unload the packages from the current R session.",
    "Try the installation anyway.",
    "Abort the installation."
  ))
  c("1", "2", "3", "4")
}

r_process_names <- function() {
  c("Rterm.exe", "Rgui.exe", "rsession.exe")
}

#' @noRd

r_app_names <- function() {
  c(
    "Rgui" = "Rgui.exe",
    "RStudio" = "rstudio.exe",
    "VScode" = "Code.exe",
    "Windows Terminal" = "WindowsTerminal.exe",
    "Emacs" = "emacs.exe"
  )
}

#' @noRd

guess_r_app <- function(ancestry) {
  names <- vcapply(ancestry, function(p) {
    tryCatch(ps::ps_name(p), error = function(e) NA_character_)
  })

  good <- which(!is.na(names) & names %in% r_app_names())[1]

  # No known app, take the last PowerShell process.
  if (is.na(good) && any(na_omit(names) == "powershell.exe")) {
    good <- which(!is.na(names) & names == "powershell.exe")[1]
  }

  # No known app, take the first cmd process (because R.exe
  # also uses cmd.exe to start Rterm.exe).
  if (is.na(good) && any(na_omit(names) == "cmd.exe")) {
    good <- rev(which(!is.na(names) & names == "cmd.exe"))[1]
  }

  if (!is.na(good)) ancestry[[good]] else ancestry[[1]]
}

#' Get the locked DLLs of a set of packages
#'
#' @param lib Package library.
#' @param pkgs Package names.
#' @return Data frame, similar to the return value of
#' `ps::ps_shared_lib_users()`:
#' * `dll`: DLL file name.
#' * `path`: Full DLL path.
#' * `pid`: Process id of locking process.
#' * `name`: Name of locking process.
#' * `username`: User of the locking process.
#' * `ps_handle`: `ps_handle` object of locking process.
#' * `package`: Package name, extracted from the path.
#'
#' @noRd

get_locked_libs <- function(lib, pkgs) {
  libs <- file.path(lib, pkgs, "libs", .Platform$r_arch)
  dlls <- dir(libs, pattern = "\\.dll$", full.names = TRUE)
  users <- ps::ps_shared_lib_users(
    dlls,
    user = NULL,
    filter = r_process_names()
  )
  package <- dirname(dirname(users$path))
  if (nzchar(.Platform$r_arch)) package <- dirname(package)
  users$package <- basename(package)
  users
}

#' DLL status for an R session
#'
#' This runs in the sub-process, but it works on the loaded packages
#' of the main process.
#'
#' @param pkgs Packages that will be installed/updated.
#' @param loaded List of loaded packages in the R session.
#' @param locked Data frame returned by [get_locked_libs()].
#' @param pid Process id of the R process. By default the pid of the
#' parent process.
#' @return List with entries:
#' * `status`: One of `"locked"`, `"loaded"` or `"clean"`.
#' * `locked`: List of loaded packages in `pkgs` locking a DLL.
#' * `loaded`: List of packages loaded in `pkgs`, but not locking a
#'   DLL.
#'
#' @noRd

loaded_status_current <- function(pkgs, loaded, locked, pid = NULL) {
  bad <- intersect(pkgs, loaded)
  bad <- setdiff(bad, "pak")
  pid <- pid %||% ps::ps_ppid()
  cur_locked <- locked$package[
    locked$package %in% bad & locked$pid == pid
  ]
  cur_loaded <- setdiff(bad, cur_locked)
  status <- if (length(cur_locked) > 0) {
    "locked"
  } else if (length(cur_loaded) > 0) {
    "loaded"
  } else {
    "none"
  }

  list(
    status = status,
    loaded = unique(cur_loaded),
    locked = unique(cur_locked)
  )
}

#' DLL status for the other R sessions
#'
#' @param locked Return value of [get_locked_libs()].
#' @param pid Process id of the _current_ R session, to exclude its
#' locked packages from the result.
#' @return List with entries:
#' * `status`: One of `"locked"` or `"none"`.
#' * `locked`: List of packages locking a DLL.
#' * `users`: Information about the locking processes. Same as the
#'   `locked` argument, except that rows referring the process with
#'   `pid` are dropped and it has no `ps_handle` column.
#'   It also has extra columns:
#'   * `ps_create_time`: The creation time of the process.
#'   * `app_pid`: The process id of the app running the R session.
#'   * `app_name`: The name of the process of the app running the R
#'     session. See [r_app_names()].
#'   * `app_create_time`: The creation time of the app processes.
#' The creation times can be used to re-create the `ps::ps_handle`
#' objects, in a way that is safe for pid reuse.
#'
#' @noRd

loaded_status_others <- function(locked, pid = NULL) {
  pid <- pid %||% ps::ps_ppid()
  oth_locked <- locked[locked$pid != pid, , drop = FALSE]

  oth_pids <- unique(oth_locked$pid)
  descent <- lapply(oth_pids, function(pid) {
    ps <- oth_locked$ps_handle[[match(pid, oth_locked$pid)]]
    tryCatch(ps::ps_descent(ps), error = function(e) list(ps))
  })
  app_handle <- lapply(descent, guess_r_app)
  app_pid <- viapply(app_handle, function(p) {
    tryCatch(ps::ps_pid(p), error = function(e) NA_integer_)
  })
  app_name <- vcapply(app_handle, function(p) {
    tryCatch(ps::ps_name(p), error = function(e) NA_character_)
  })

  map <- match(oth_locked$pid, oth_pids)
  oth_locked$app_pid <- app_pid[map]
  oth_locked$app_name <- app_name[map]
  oth_locked$app_handle <- app_handle[map]

  # If ps_name() was an error, the the process has finished, drop it
  oth_locked <- oth_locked[!is.na(oth_locked$app_name), , drop = FALSE]

  status <- if (nrow(oth_locked) > 0) "locked" else "none"

  oth_locked$create_time <- vdapply(oth_locked$ps_handle, ps::ps_create_time)
  oth_locked$app_create_time <- vdapply(
    oth_locked$app_handle,
    ps::ps_create_time
  )
  oth_locked$ps_handle <- NULL
  oth_locked$app_handle <- NULL

  list(
    status = status,
    locked = unique(oth_locked$package),
    users = oth_locked
  )
}

loaded_packages_response <- function(status, response) {
  switch(
    status$status,
    "locked-none" = {
      switch(
        response,
        "1" = {
          # Unload
          unload(status$current$locked)
          list(status = "go-on", unloaded = status$current$locked)
        },
        "2" = {
          # Try anyway, nothing to do
          list(status = "go-on")
        },
        "3" = {
          # Abort
          stop("Aborted.", call. = FALSE)
        },
        stop("Invalid reponse, internal pak error")
      )
    },
    "none-locked" = ,
    "loaded-locked" = ,
    "locked-locked" = {
      switch(
        response,
        "1" = {
          # Unload (if needed), try again
          if (length(status$current$locked) > 0) {
            unload(status$current$locked)
          }
          list(status = "try-again", unloaded = status$current$locked)
        },
        "2" = {
          # Unload (if needed), terminate
          if (length(status$current$locked) > 0) {
            unload(status$current$locked)
          }
          terminate(status$others)
          list(status = "go-on", unloaded = status$current$locked)
        },
        "3" = {
          # Try anyway
          list(status = "go-on")
        },
        "4" = {
          # Abort
          stop("Aborted.", call. = FALSE)
        },
        stop("Invalid response, internal pak error")
      )
    }
  )
}
