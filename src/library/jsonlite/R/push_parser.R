#' @useDynLib jsonlite R_parse_connection
parse_con <- function(con, bigint_as_char) {
  stopifnot(inherits(con, "connection"))
  if (!isOpen(con)) {
    on.exit(close(con)) # also destroy con if 'open' fails
    open(con, "rb")
  }
  .Call(R_parse_connection, con, bigint_as_char)
}
