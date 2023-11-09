
process_format <- function(self, private) {

  state <- if (self$is_alive()) {
    pid <- self$get_pid()
    paste0("running, pid ", paste(pid, collapse = ", "), ".")
  } else {
    "finished."
  }

  paste0(
    "PROCESS ",
    "'", private$get_short_name(), "', ",
    state,
    "\n"
  )
}

process_print <- function(self, private) {
  cat(process_format(self, private))
  invisible(self)
}

process_get_short_name <- function(self, private) {
  basename(private$command)
}
