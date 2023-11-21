mkdirp <- function(dir, msg = NULL) {
  s <- vlapply(dir, dir.create, recursive = TRUE, showWarnings = FALSE)
  if (any(s) && !is.null(msg)) {
    cli::cli_alert_info(c(msg, ": {.path {dir[s]}}"))
  }
  invisible(s)
}
