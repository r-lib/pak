#' Encode/decode base64
#'
#' Simple in-memory base64 encoder and decoder. Used internally for converting
#' raw vectors to text. Interchangeable with encoder from `base64enc` or
#' `openssl` package.
#'
#' The [base64url_enc] and [base64url_dec] functions use a variation of base64
#' that substitute characters `+/`  for `-_` respectively, such that the output
#' does not require URL-encoding. See also section 5 of rfc4648.
#'
#' @param input string or raw vector to be encoded/decoded
#' @export
#' @rdname base64
#' @name base64
#' @useDynLib jsonlite R_base64_decode
#' @examples str <- base64_enc(serialize(iris, NULL))
#' out <- unserialize(base64_dec(str))
#' stopifnot(identical(out, iris))
base64_dec <- function(input) {
  if(is.character(input)){
    input <- charToRaw(paste(input, collapse = "\n"))
  }
  stopifnot(is.raw(input))
  .Call(R_base64_decode, input)
}

#' @export
#' @rdname base64
#' @useDynLib jsonlite R_base64_encode
base64_enc <- function(input) {
  if(is.null(input))
    return(NA_character_)
  if(is.character(input)){
    input <- charToRaw(paste(input, collapse = "\n"))
  }
  stopifnot(is.raw(input))
  .Call(R_base64_encode, input)
}

#' @export
#' @rdname base64
base64url_enc <- function(input){
  text <- base64_enc(input)
  sub("=+$", "", chartr('+/', '-_', text))
}

#' @export
#' @rdname base64
base64url_dec <- function(input){
  text <- fix_padding(chartr('-_', '+/', input))
  base64_dec(text)
}

# Ensures base64 length is a multiple of 4
fix_padding <- function(text){
  text <- gsub("[\r\n]", "", text)[[1]]
  mod <- nchar(text) %% 4;
  if(mod > 0){
    padding <- paste(rep("=", (4 - mod)), collapse = "")
    text <- paste0(text, padding)
  }
  text
}
