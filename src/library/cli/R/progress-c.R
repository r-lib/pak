
progress_c_update <- function(pb, auto_done = TRUE) {
  cli_tick_reset()

  caller <- pb$caller %||% sys.frame(sys.nframe() - 1L)

  pb$tick <- pb$tick + 1L

  if (is.null(pb$format)) {
    pb$format <- pb__default_format(pb$type, pb$total)
  }

  if (pb$auto_terminate && auto_done && !is.na(pb$total) &&
      pb$current == pb$total) {
    progress_c_done(pb, caller = caller)
    return(NULL)
  }

  opt <- options(cli__pb = pb)
  on.exit(options(opt), add = TRUE)

  handlers <- cli_progress_select_handlers(pb, caller)
  if (is.null(pb$added)) {
    pb$added <- TRUE
    for (h in handlers) {
      if ("add" %in% names(h)) h$add(pb, .envir = caller)
    }
  }

  for (h in handlers) {
    if ("set" %in% names(h)) h$set(pb, .envir = caller)
  }

  NULL
}

progress_c_done <- function(pb, caller = NULL) {
  if (isTRUE(pb$done)) return()

  caller <- caller %||% pb$caller %||% sys.frame(sys.nframe() - 1L)

  opt <- options(cli__pb = pb)
  on.exit(options(opt), add = TRUE)

  handlers <- cli_progress_select_handlers()
  for (h in handlers) {
    if ("complete" %in% names(h)) {
      h$complete(pb, .envir = caller, result = "done")
    }
  }

  if (!is.null(pb$id)) clienv$progress[[pb$id]] <- NULL
  if (!is.null(pb$envkey)) clienv$progress_ids[[pb$envkey]] <- NULL

  pb$done <- TRUE

  NULL
}
