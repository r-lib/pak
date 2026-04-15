cli_server_default <- function(msg) {
  cli_server_default_safe(msg)
}

cli_server_default_safe <- function(msg) {
  type <- as.character(msg$type)[1]
  app <- default_app() %||% start_app(.auto_close = FALSE)
  do.call(app[[type]], msg$args)
}

cli_server_callr_handler <- function(msg) {
  cli_server_default(msg)
}
