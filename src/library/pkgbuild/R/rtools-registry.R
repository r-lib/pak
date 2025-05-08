read_registry <- function(...) {
  tryCatch(
    utils::readRegistry(...),
    error = function(e) NULL
  )
}

scan_registry_for_rtools <- function(debug = FALSE) {
  if (debug) cat("Scanning registry...\n")

  keys <- c(
    read_registry(
      "SOFTWARE\\R-core\\Rtools",
      hive = "HCU",
      view = "32-bit",
      maxdepth = 2
    ),
    read_registry(
      "SOFTWARE\\R-core\\Rtools",
      hive = "HLM",
      view = "32-bit",
      maxdepth = 2
    ),
    read_registry(
      "SOFTWARE\\R-core\\Rtools",
      hive = "HLM",
      view = "64-bit",
      maxdepth = 2
    )
  )

  if (is.null(keys)) {
    return(NULL)
  }

  rts <- vector("list", length(keys))

  for (i in seq_along(keys)) {
    version <- names(keys)[[i]]
    key <- keys[[version]]
    if (!is.list(key) || is.null(key$InstallPath)) next
    install_path <- normalizePath(
      key$InstallPath,
      mustWork = FALSE,
      winslash = "/"
    )

    if (debug) cat("Found", install_path, "for", version, "\n")
    rts[[i]] <- rtools(install_path, version)
  }

  Filter(Negate(is.null), rts)
}
