
# Helper functions from cli, for the ANSI color detection

#' @include aab-num-ansi-colors.R

get_real_output <- function(output) {
  cli_output_connection()
}

cli_output_connection <- function() {
  if (is_interactive() && no_sink()) stdout() else stderr()
}

is_interactive <- function() {
  opt <- getOption("rlib_interactive")
  if (isTRUE(opt)) {
    TRUE
  } else if (identical(opt, FALSE)) {
    FALSE
  } else if (tolower(getOption("knitr.in.progress", "false")) == "true") {
    FALSE
  } else if (tolower(getOption("rstudio.notebook.executing", "false")) == "true") {
    FALSE
  } else if (identical(Sys.getenv("TESTTHAT"), "true")) {
    FALSE
  } else {
    interactive()
  }
}

no_sink <- function() {
  sink.number() == 0 && sink.number("message") == 2
}

`%||%` <- function(l, r) if (is.null(l)) r else l

clienv <- new.env(parent = emptyenv())
