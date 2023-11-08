# First check if gcc set by BINPREF/CC is valid and use that if so
scan_config_for_rtools <- function(debug = FALSE) {
  if (debug) {
    cat("Scanning R CMD config CC...\n")
  }

  if (!is_R4() && !using_gcc49()) {
    return()
  }

  cc_path <- gsub("\n", "", callr::rcmd_safe("config", "CC")$stdout)
  # remove '-m64' from tail if it exists
  cc_path <- sub("[[:space:]]+-m[[:digit:]]+$", "", cc_path)

  if (debug) {
    cat("cc_path:", cc_path, "\n")
  }

  cc_path <- find_arch_exe(cc_path, debug = debug)
  if (cc_path == "") {
    NULL
  } else {
    install_path <- dirname(dirname(dirname(cc_path)))
    if (debug) {
      cat("install_path:", install_path, "\n")
    }
    rtools(install_path, "custom", valid_binpref = TRUE)
  }
}

is_R4 <- function() {
  R.Version()$major >= "4"
}
