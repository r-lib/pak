
#' Base64 Encoding and Decoding
#'
#' @param x Raw vector to encode / decode.
#' @return Raw vector, result of the encoding / decoding.
#'
#' @export

base64_decode <- function(x) {
  if (is.character(x)) {
    x <- charToRaw(paste(gsub("\\s+", "", x), collapse = ""))
  }
  chain_call(c_processx_base64_decode, x)
}

#' @export
#' @rdname base64_decode

base64_encode <- function(x) {
  rawToChar(chain_call(c_processx_base64_encode, x))
}
