#' Uncompress a raw GZIP stream
#'
#' @param buffer Raw vector, containing the data to uncompress.
#' @param pos Start position of data to uncompress in `buffer`.
#' @param size Uncompressed size estimate, or `NULL`. If not given, or too
#'   small, the output buffer is resized multiple times.
#' @return Named list with three entries:
#'   - `output`: raw vector, the uncompressed data,
#'   - `bytes_read`: number of bytes used from `buffer`,
#'   - `bytes_written`: number of bytes written to the output buffer.
#'
#' @seealso [base::memDecompress()] does the same with `type = "gzip"`,
#'   but it does not tell you the number of bytes read from the input.
#'
#' @export
#' @examples
#' data_gz <- deflate(charToRaw("Hello world!"))
#' inflate(data_gz$output)

inflate <- function(buffer, pos = 1L, size = NULL) {
  stopifnot(
    is.raw(buffer),
    is_count(pos),
    is.null(size) || is_count(size)
  )
  if (!is.null(size)) size <- as.integer(size)
  .Call(c_R_inflate, buffer, as.integer(pos), size)
}

#' Compress a raw GZIP stream
#'
#' @param buffer Raw vector, containing the data to compress.
#' @param level Compression level, integer between 1 (fatest) and 9 (best).
#' @param pos Start position of data to compress in `buffer`.
#' @param size Compressed size estimate, or `NULL`. If not given, or too
#'   small, the output buffer is resized multiple times.
#' @return Named list with three entries:
#'   - `output`: raw vector, the compressed data,
#'   - `bytes_read`: number of bytes used from `buffer`,
#'   - `bytes_written`: number of bytes written to the output buffer.
#'
#' @seealso [base::memCompress()] does the same with `type = "gzip"`,
#'   but it does not tell you the number of bytes read from the input.
#'
#' @export
#' @examples
#' data_gz <- deflate(charToRaw("Hello world!"))
#' inflate(data_gz$output)

deflate <- function(buffer, level = 6L, pos = 1L, size = NULL) {
  stopifnot(
    is.raw(buffer),
    is_count(level),
    level >= 1L && level <= 9L,
    is_count(pos),
    is.null(size) || is_count(size)
  )
  if (!is.null(size)) size <- as.integer(size)
  .Call(c_R_deflate, buffer, as.integer(level), as.integer(pos), size)
}
