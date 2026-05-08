#' @useDynLib cli, .registration=TRUE
NULL

## nocov start

dummy <- function() {
}

cli_timer_dynamic <- 200L
cli_timer_non_dynamic <- 3000L

clienv <- new.env(parent = emptyenv())
clienv$pid <- Sys.getpid()
clienv$globalenv <- format(.GlobalEnv)
clienv$status <- list()
clienv$progress <- list()
clienv$progress_ids <- list()

clienv$load_time <- NULL
clienv$speed_time <- 1.0
clienv$tick_time <- 200L

task_callback <- NULL

clienv$unloaded <- FALSE

rstudio_r_fix <- 0

.onLoad <- function(libname, pkgname) {
  err$onload_hook()

  # Try to restore cursor as much as we can
  if (Sys.getenv("R_CLI_HIDE_CURSOR") != "false" && isatty(stdout())) {
    reg.finalizer(clienv, function(e) cli::ansi_show_cursor(), TRUE)
    task_callback <<- addTaskCallback(
      function(...) {
        cli::ansi_show_cursor()
        TRUE
      },
      "cli-show-cursor"
    )
  }

  # https://github.com/r-lib/cli/issues/352
  rstudio_r_fix <<- (Sys.getenv("RSTUDIO") == 1) + 0L

  pkgenv <- environment(dummy)

  clienv$load_time <- Sys.time()

  clienv$speed_time <- as.double(Sys.getenv("CLI_SPEED_TIME", "1.0"))

  tt <- as.integer(Sys.getenv("CLI_TICK_TIME", NA_character_))
  if (is.na(tt)) {
    tt <- if (interactive() || is_dynamic_tty()) {
      cli_timer_dynamic
    } else {
      cli_timer_non_dynamic
    }
  }

  clienv$tick_time <- as.integer(tt)
  .Call(
    clic_start_thread,
    pkgenv,
    clienv$tick_time,
    clienv$speed_time
  )

  # For valgrind: https://github.com/r-lib/cli/issues/311
  reg.finalizer(asNamespace("cli"), function(x) x$unload(), TRUE)

  if (getRversion() >= "3.5.0") {
    `__cli_update_due` <<- .Call(clic_make_timer)
  } else {
    rm("__cli_update_due", envir = pkgenv)
    makeActiveBinding(
      "__cli_update_due",
      function() .Call(clic_update_due),
      pkgenv
    )
  }

  ccli_tick_reset <<- clic_tick_reset

  makeActiveBinding(
    "symbol",
    function() {
      ## If `cli.unicode` is set we use that
      opt <- getOption("cli.unicode", NULL)
      if (!is.null(opt)) {
        if (isTRUE(opt)) {
          return(symbol_utf8)
        } else {
          return(symbol_ascii)
        }
      }

      ## Otherwise we try to auto-detect
      rst <- rstudio$detect()$type
      rok <- c("rstudio_console", "rstudio_console_starting")
      if (is_utf8_output()) {
        symbol_utf8
      } else if (is_latex_output()) {
        symbol_ascii
      } else {
        symbol_ascii
      }
    },
    pkgenv
  )

  makeActiveBinding("pb_bar", cli__pb_bar, pkgenv)
  makeActiveBinding("pb_current", cli__pb_current, pkgenv)
  makeActiveBinding("pb_current_bytes", cli__pb_current_bytes, pkgenv)
  makeActiveBinding("pb_elapsed", cli__pb_elapsed, pkgenv)
  makeActiveBinding("pb_elapsed_clock", cli__pb_elapsed_clock, pkgenv)
  makeActiveBinding("pb_elapsed_raw", cli__pb_elapsed_raw, pkgenv)
  makeActiveBinding("pb_eta", cli__pb_eta, pkgenv)
  makeActiveBinding("pb_eta_raw", cli__pb_eta_raw, pkgenv)
  makeActiveBinding("pb_eta_str", cli__pb_eta_str, pkgenv)
  makeActiveBinding("pb_extra", cli__pb_extra, pkgenv)
  makeActiveBinding("pb_id", cli__pb_id, pkgenv)
  makeActiveBinding("pb_name", cli__pb_name, pkgenv)
  makeActiveBinding("pb_percent", cli__pb_percent, pkgenv)
  makeActiveBinding("pb_pid", cli__pb_pid, pkgenv)
  makeActiveBinding("pb_rate", cli__pb_rate, pkgenv)
  makeActiveBinding("pb_rate_raw", cli__pb_rate_raw, pkgenv)
  makeActiveBinding("pb_rate_bytes", cli__pb_rate_bytes, pkgenv)
  makeActiveBinding("pb_spin", cli__pb_spin, pkgenv)
  makeActiveBinding("pb_status", cli__pb_status, pkgenv)
  makeActiveBinding("pb_timestamp", cli__pb_timestamp, pkgenv)
  makeActiveBinding("pb_total", cli__pb_total, pkgenv)
  makeActiveBinding("pb_total_bytes", cli__pb_total_bytes, pkgenv)

  if (is.null(getOption("callr.condition_handler_cli_message"))) {
    options(callr.condition_handler_cli_message = cli__default_handler)
  }
}

unload <- function() {
  if (!clienv$unloaded) .Call(clic_unload)
  clienv$unloaded <- TRUE
}

.onUnload <- function(libpath) {
  tryCatch(removeTaskCallback(task_callback), error = function(e) NULL)
  tryCatch(cli_progress_cleanup(), error = function(e) NULL)
  tryCatch(ansi_show_cursor(), error = function(e) NULL)
  unload()
}

## nocov end
