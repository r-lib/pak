#' Gzipped JSON
#'
#' Wrapper to generate and parse gzipped JSON, in order to save some disk or
#' network space. This is mainly effective for larger json objects with many
#' repeated keys, as is common in serialized data frames.
#'
#' The [as_gzjson_raw] and [parse_gzjson_raw] functions work with raw (binary)
#' vectors of compressed data. To use this in a place where only text is allowed
#' you can wrap the output again in [base64] as done by [as_gzjson_b64] and
#' [parse_gzjson_b64]. This increases the size again with about 33%.
#'
#'
#' @param x R data object to be converted to JSON
#' @param ... passed down to [toJSON] or [fromJSON]
#' @export
#' @name gzjson
#' @rdname gzjson
#' @examples str <- as_gzjson_b64(iris[1:5,])
#' cat(str)
#' parse_gzjson_b64(str)
as_gzjson_raw <- function(x, ...){
  json <- toJSON(x = x, ...)
  memCompress(json, 'gzip')
}

#' @export
#' @rdname gzjson
as_gzjson_b64 <- function(x, ...){
  buf <- as_gzjson_raw(x = x, ...)
  base64_enc(buf)
}

#' @export
#' @rdname gzjson
#' @param buf  raw vector with gzip compressed data
parse_gzjson_raw <- function(buf, ...){
  json <- rawToChar(memDecompress(buf, 'gzip'))
  fromJSON(json, ...)
}

#' @export
#' @rdname gzjson
#' @param b64 base64 encoded string containing gzipped json data
parse_gzjson_b64 <- function(b64, ...){
  parse_gzjson_raw(base64_dec(b64), ...)
}
