#' Write a YAML file
#'
#' Write the YAML representation of an R object to a file
#'
#' If `file` is a non-open connection, an attempt is made to open it and
#' then close it after use.
#'
#' This function is a convenient wrapper around [as.yaml()].
#'
#' @param x The object to be converted.
#' @param file Either a character string naming a file or a [connection]
#'   open for writing.
#' @param fileEncoding Character string: if non-empty declares the encoding to
#'   be used on a file (not a connection) so the character data can be
#'   re-encoded as they are written. See [file()].
#' @param \dots Arguments to [as.yaml()].
#' @author Jeremy Stephens <jeremy.f.stephens@@vumc.org>
#' @seealso [as.yaml()], [read_yaml()],
#' [yaml.load_file()]
#' @export
#' @keywords data manip
#' @examples
#'
#' \dontrun{
#'   # writing to a file connection
#'   filename <- tempfile()
#'   con <- file(filename, "w")
#'   write_yaml(data.frame(a=1:10, b=letters[1:10], c=11:20), con)
#'   close(con)
#'
#'   # using a filename to specify output file
#'   write_yaml(data.frame(a=1:10, b=letters[1:10], c=11:20), filename)
#' }
#'
write_yaml <- function(x, file, fileEncoding = "UTF-8", ...) {
  result <- as.yaml(x, ...)

  if (is.character(file)) {
    file <-
      if (nzchar(fileEncoding)) {
        file(file, "w", encoding = fileEncoding)
      } else {
        file(file, "w")
      }
    on.exit(close(file))
  } else if (!isOpen(file, "w")) {
    open(file, "w")
    on.exit(close(file))
  }
  if (!inherits(file, "connection")) {
    stop("'file' must be a character string or connection")
  }

  cat(result, file = file, sep = "")
}
