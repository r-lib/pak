#' Read a YAML file
#'
#' Read a YAML document from a file and create an R object from it
#'
#' This function is a convenient wrapper for [yaml.load()] and is a
#' nicer alternative to [yaml.load_file()].
#'
#' You can specify a label to be prepended to error messages via the
#' `error.label` argument.  If `error.label` is missing,
#' `read_yaml` will make an educated guess for the value of
#' `error.label` by either using the specified filename (when `file`
#' is a character vector) or using the description of the supplied connection
#' object (via the `summary` function).  If `text` is used, the
#' default value of `error.label` will be `NULL`.
#'
#' @param file Either a character string naming a file or a [connection]
#'   open for reading.
#' @param fileEncoding Character string: if non-empty declares the encoding
#'   used on a file (not a connection) so the character data can be re-encoded.
#'   See [file()].
#' @param text Character string: if `file` is not supplied and this is,
#'   then data are read from the value of `text` via a text connection.
#'   Notice that a literal string can be used to include (small) data sets
#'   within R code.
#' @param error.label A label to prepend to error messages (see Details).
#' @param readLines.warn Logical (default: TRUE). Suppress warnings from
#'   readLines used inside read_yaml.
#' @param ... Arguments to pass to [yaml.load()].
#' @return If the root YAML object is a map, a named list or list with an
#' attribute of 'keys' is returned.  If the root object is a sequence, a list
#' or vector is returned, depending on the contents of the sequence.  A vector
#' of length 1 is returned for single objects.
#' @author Jeremy Stephens <jeremy.f.stephens@@vumc.org>
#' @seealso [yaml.load()], [write_yaml()],
#' [yaml.load_file()]
#' @references YAML: http://yaml.org
#'
#' libyaml: https://pyyaml.org/wiki/LibYAML
#' @keywords programming data manip
#' @export
#' @examples
#'
#' \dontrun{
#'   # reading from a file connection
#'   filename <- tempfile()
#'   cat("test: data\n", file = filename)
#'   con <- file(filename, "r")
#'   read_yaml(con)
#'   close(con)
#'
#'   # using a filename to specify input file
#'   read_yaml(filename)
#' }
#'
#'   # reading from a character vector
#'   read_yaml(text="- hey\n- hi\n- hello")
#'
read_yaml <- function(
  file,
  fileEncoding = "UTF-8",
  text,
  error.label,
  readLines.warn = TRUE,
  ...
) {
  if (missing(file) && !missing(text)) {
    if (missing(error.label)) {
      error.label <- NULL
    }
    file <- textConnection(text, encoding = "UTF-8")
    on.exit(close(file))
  } else if (is.character(file)) {
    if (missing(error.label)) {
      error.label <- file
    }
    file <- if (nzchar(fileEncoding)) {
      file(file, "rt", encoding = fileEncoding)
    } else {
      file(file, "rt")
    }
    on.exit(close(file))
  } else if (inherits(file, "connection")) {
    if (missing(error.label)) {
      # try to guess filename
      s <- try(summary(file), silent = TRUE)
      if (
        !inherits(s, "try-error") && is.list(s) && "description" %in% names(s)
      ) {
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

  string <- paste(readLines(file, warn = readLines.warn), collapse = "\n")
  yaml.load(string, error.label = error.label, ...)
}
