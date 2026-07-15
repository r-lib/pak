threaded_unzip <- function(
  zipfiles,
  exdirs = ".",
  num_threads = NULL,
  passwords = NULL
) {
  passwords <- passwords %||% ""
  stopifnot(
    length(exdirs) == 1L || length(exdirs) == length(zipfiles),
    length(passwords) == 1L || length(passwords) == length(zipfiles)
  )
  if (length(exdirs) == 1L) {
    exdirs <- rep_len(exdirs, length(zipfiles))
  }
  if (length(passwords) == 1L) {
    passwords <- rep_len(passwords, length(zipfiles))
  }
  num_threads <- num_threads %||% get_num_threads()
  ret <- .Call(
    c_R_threaded_unzip,
    as.character(zipfiles),
    as.character(exdirs),
    as.integer(num_threads),
    as.character(passwords)
  )
  failed <- ret[[1]] != 0L
  if (any(failed)) {
    msgs <- paste0(
      "  ",
      zipfiles[failed],
      ": ",
      ret[[2]][failed],
      collapse = "\n"
    )
    stop(
      "Failed to unzip ",
      sum(failed),
      " file",
      if (sum(failed) > 1) "s" else "",
      ":\n",
      msgs,
      call. = FALSE
    )
  }
  results <- Map(
    function(zf, ed) {
      lst <- zip_list(zf)
      lst$path <- file.path(normalizePath(ed), lst$filename)
      lst$encryption <- NULL
      lst
    },
    zipfiles,
    exdirs
  )
  invisible(do.call(rbind, results))
}

get_num_threads <- function() {
  opt <- getOption("zip_threads")
  if (!is.null(opt)) {
    if (!is.numeric(opt) || length(opt) != 1L || opt < 1L) {
      stop(
        "Invalid value for 'zip_threads' option, must be a positive integer."
      )
    }
    return(as.integer(opt))
  }
  ev <- Sys.getenv("ZIP_THREADS", NA_character_)
  if (!is.na(ev)) {
    evval <- suppressWarnings(as.integer(ev))
    if (is.na(evval) || evval < 1L) {
      stop(
        "Invalid value for ZIP_THREADS environment variable, ",
        "must be a positive integer."
      )
    }
    return(evval)
  }

  ncpus <- getOption("Ncpus")
  if (!is.null(ncpus)) {
    if (!is.numeric(ncpus) || length(ncpus) != 1L || ncpus < 1L) {
      stop(
        "Invalid value for 'Ncpus' option, must be a positive integer."
      )
    }
    return(as.integer(ncpus))
  }

  2L
}
