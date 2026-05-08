#' @export

ticking <- function(cond, name = NULL, ..., .envir = parent.frame()) {
  val <- force(cond)

  new <- is.null(clienv$progress_ids[[format(.envir)]])

  if (new && val) cli_progress_bar(name = name, ..., .envir = .envir)

  if (val) {
    cli_progress_update(.envir = .envir)
  } else {
    cli_progress_done(.envir = .envir)
  }

  val
}
