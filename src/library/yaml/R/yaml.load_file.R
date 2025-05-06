`yaml.load_file` <-
function(input, error.label, readLines.warn=TRUE, ...) {
  if (missing(error.label)) {
    if (inherits(input, "connection")) {
      # try to guess filename
      s <- try(summary(input), silent = TRUE)
      if (!inherits(s, "try-error") && is.list(s) && "description" %in% names(s)) {
        error.label <- s$description
      }
    }
    else if (is.character(input) && nzchar(input[1])) {
      error.label <- input[1]
    }
    else {
      error.label <- NULL
    }
  }

  if (is.character(input)) {
    con <- file(input, encoding = 'UTF-8')
    on.exit(close(con), add = TRUE)
  } else {
    con <- input
  }
  yaml.load(readLines(con, warn=readLines.warn),
            error.label = error.label, ...)
}
