
#' Add a progress bar to a mapping function or for loop
#'
#' @description
#' Note that this function is currently experimental!
#'
#' Use `cli_progress_along()` in a mapping function or in a for loop, to add a
#' progress bar. It uses [cli_progress_bar()] internally.
#'
#' @details
#'
#' ## `for` loop
#'
#' A `for` loop with `cli_progress_along()` looks like this:
#'
#' ```r
#' for (i in cli_progress_along(seq)) {
#'   ...
#' }
#' ```
#'
#' A complete example:
#'
#' ```{asciicast progress-along-1}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciciast_cursor = FALSE
#' clifun <- function() {
#'   for (i in cli_progress_along(1:100, "Downloading")) {
#'      Sys.sleep(4/100)
#'   }
#' }
#' clifun()
#' ```
#'
#' ## `lapply()` and other mapping functions
#'
#' They will look like this:
#'
#' ```r
#' lapply(cli_progress_along(X), function(i) ...)
#' ```
#'
#' A complete example:
#'
#' ```{asciicast progress-along-2}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' res <- lapply(cli_progress_along(1:100, "Downloading"), function(i) {
#'   Sys.sleep(4/100)
#' })
#' ```
#'
#' ## Custom format string
#'
#' ```{asciicast progress-along-3}
#' #| asciicast_at = "all",
#' #| asciicast_knitr_output = "svg",
#' #| asciicast_cursor = FALSE
#' clifun <- function() {
#'   for (i in cli_progress_along(1:100,
#'       format = "Downloading data file {cli::pb_current}")) {
#'      Sys.sleep(4/100)
#'   }
#' }
#' clifun()
#' ```
#'
#' ## Breaking out of loops
#'
#' Note that if you use `break` in the `for` loop, you probably want to
#' terminate the progress bar explicitly when breaking out of the loop,
#' or right after the loop:
#'
#' ```r
#' for (i in cli_progress_along(seq)) {
#'   ...
#'   if (cond) cli_progress_done() && break
#'   ...
#' }
#' ```
#'
#' @param x Sequence to add the progress bar to.
#' @param name Name of the progress bar, a label, passed to
#'   [cli_progress_bar()].
#' @param total Passed to [cli_progress_bar()].
#' @param ... Passed to [cli_progress_bar()].
#' @param .envir Passed to [cli_progress_bar()].
#'
#' @return An index vector from 1 to `length(x)` that triggers progress
#' updates as you iterate over it.
#'
#' @seealso This function supports [inline markup][inline-markup].
#' @seealso [cli_progress_bar()] and the traditional progress bar API.
#' @family progress bar functions
#' @family functions supporting inline markup
#' @export

cli_progress_along <- function(x,
                       name = NULL,
                       total = length(x),
                       ...,
                       .envir = parent.frame()) {

  name; total; .envir; list(...)

  if (getRversion() < "3.5.0") return(seq_along(x))
  id <- cli_progress_bar(name = name, total = total, ...,
                         .auto_close = FALSE, .envir = .envir)
  closeenv <- sys.frame(-1)
  if (format(closeenv) != clienv$globalenv) {
    defer(
      cli_progress_done(id = id, .envir = .envir, result = "auto"),
      envir = closeenv
    )
  }
  sax <- seq_along(x)
  clienv$progress[[id]]$caller <- .envir
  .Call(clic_progress_along, sax, clienv$progress[[id]])
}

progress_altrep_update <- function(pb) {
  tryCatch({
    cli_tick_reset()
    caller <- pb$caller
    pb$tick <- pb$tick + 1L

    if (is.null(pb$format)) {
      pb$format <- pb__default_format(pb$type, pb$total)
    }

    opt <- options(cli__pb = pb)
    on.exit(options(opt), add = TRUE)

    handlers <- cli_progress_select_handlers(pb, caller)
    if (is.null(pb$added)) {
      pb$added <- TRUE
      for (h in handlers) {
        if ("add" %in% names(h)) h$add(pb, .envir = caller)
      }
    } else {
      for (h in handlers) {
        if ("set" %in% names(h)) h$set(pb, .envir = caller)
      }
    }
  }, error = function(err) {
    if (!isTRUE(pb$warned)) {
      warning("cli progress bar update failed: ", conditionMessage(err),
              immediate. = TRUE)
    }
    pb$warned <- TRUE
  })

  NULL
}
