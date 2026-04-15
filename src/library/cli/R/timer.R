#' @export
`__cli_update_due` <- FALSE

#' @export

cli_tick_reset <- function() {
  .Call(clic_tick_reset)
}

#' @export

ccli_tick_reset <- NULL

cli_tick_set <- function(tick_time = NULL, speed_time = NULL) {
  tick_time <- tick_time %||% clienv$tick_time
  speed_time <- speed_time %||% clienv$speed_time

  clienv$speed_time <- as.double(speed_time)
  clienv$tick_time <- as.integer(tick_time)
  .Call(clic_tick_set, clienv$tick_time, clienv$speed_time)
  invisible()
}

cli_tick_pause <- function(state = TRUE) {
  .Call(clic_tick_pause, state)
}

cli_tick_resume <- function(state = TRUE) {
  .Call(clic_tick_resume, state)
}

cli_with_ticks <- function(expr) {
  on.exit(cli_tick_resume(TRUE), add = TRUE)
  opts <- options(cli.progress_show_after = 0)
  on.exit(options(opts), add = TRUE)
  cli_tick_pause(TRUE)
  expr
}

cli_without_ticks <- function(expr) {
  on.exit(cli_tick_resume(TRUE), add = TRUE)
  opts <- options(cli.progress_show_after = 0)
  on.exit(options(opts), add = TRUE)
  cli_tick_pause(FALSE)
  expr
}
