terminate <- function(others, msg = "  Terminating %s (%i) ...") {
  load_private_packages()
  ps <- pkg_data$ns$ps
  pcs <- others$users
  pcs <- pcs[!duplicated(pcs$pid), , drop = FALSE]

  procs <- lapply(seq_len(nrow(pcs)), function(l) {
    proc <- ps$ps_handle(pcs$pid[l], .POSIXct(pcs$create_time[l]))
    message(sprintf(msg, pcs$name[l], pcs$pid[l]))
    tryCatch(ps$ps_kill(proc), error = function(e) NULL)
    proc
  })

  # TODO: better wait, with a proper ps_wait() function
  bad <- which(vlapply(procs, ps$ps_is_running))
  limit <- Sys.time() + 1
  while (length(bad) > 0 && Sys.time() < limit) {
    Sys.sleep(0.05)
    bad <- which(vlapply(procs, ps$ps_is_running))
  }

  # TODO: summary of processes killed

  if (length(bad)) {
    str <- paste0(
      pcs$name[bad],
      " (",
      pcs$pid[bad],
      ")",
      collapse = ", "
    )
    stop("Failed to kill some processes: ", str)
  }

  message()
}
