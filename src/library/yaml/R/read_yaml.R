`read_yaml` <-
function(file, fileEncoding = "UTF-8", text, error.label, readLines.warn=TRUE, ...) {
  if (missing(file) && !missing(text)) {
    if (missing(error.label)) {
      error.label <- NULL
    }
    file <- textConnection(text, encoding = "UTF-8")
    on.exit(close(file))
  }
  else if (is.character(file)) {
    if (missing(error.label)) {
      error.label <- file
    }
    file <- if (nzchar(fileEncoding))
      file(file, "rt", encoding = fileEncoding)
    else file(file, "rt")
    on.exit(close(file))
  }
  else if (inherits(file, "connection")) {
    if (missing(error.label)) {
      # try to guess filename
      s <- try(summary(file), silent = TRUE)
      if (!inherits(s, "try-error") && is.list(s) && "description" %in% names(s)) {
        error.label <- s$description
      }
    }

    if (!isOpen(file, "rt")) {
      open(file, "rt")
      on.exit(close(file))
    }
  } else {
    stop("'file' must be a character string or connection")
  }

  string <- paste(readLines(file,warn=readLines.warn), collapse="\n")
  yaml.load(string, error.label = error.label, ...)
}
