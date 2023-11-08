#' Read/write JSON
#'
#' These functions are similar to [toJSON()] and [fromJSON()] except they
#' explicitly distinguish between path and literal input, and do not simplify
#' by default.
#'
#' @export
#' @rdname read_json
#' @param path file on disk
#' @param simplifyVector simplifies nested lists into vectors and data frames. See [fromJSON()].
#' @seealso [fromJSON()], [stream_in()]
#' @examples tmp <- tempfile()
#' write_json(iris, tmp)
#'
#' # Nested lists
#' read_json(tmp)
#'
#' # A data frame
#' read_json(tmp, simplifyVector = TRUE)
read_json <- function(path, simplifyVector = FALSE, ...){
  parse_json(file(path), simplifyVector = simplifyVector, ...)
}

#' @export
#' @rdname read_json
#' @param json string with literal json or connection object to read from
parse_json <- function(json, simplifyVector = FALSE, ...){
  parse_and_simplify(json, simplifyVector = simplifyVector, ...)
}

#' @export
#' @rdname read_json
#' @param x an object to be serialized to JSON
#' @param ... additional conversion arguments, see also [toJSON()] or [fromJSON()]
write_json <- function(x, path, ...) {
  json <- jsonlite::toJSON(x, ...)
  writeLines(json, path, useBytes = TRUE)
}
