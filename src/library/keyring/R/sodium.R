sodium_random <- function(n = 1) {
  assert_that(
    is.numeric(n),
    length(n) == 1,
    !is.na(n)
  )
  .Call(rsodium_randombytes_buf, as.integer(n))
}

sodium_bin2hex <- function(bin) {
  stopifnot(is.raw(bin))
  .Call(rsodium_bin2hex, bin)
}

sodium_hex2bin <- function(hex, ignore = ":") {
  assert_that(
    is.character(hex),
    length(hex) == 1,
    is.character(ignore),
    length(ignore) == 1
  )
  .Call(rsodium_hex2bin, hex, ignore)
}

sodium_data_encrypt <- function(msg, key, nonce = sodium_random(24)) {
  assert_that(
    is.raw(msg),
    is.raw(key)
  )
  out <- .Call(rsodium_crypto_secret_encrypt, msg, key, nonce)
  structure(out, nonce = nonce)
}

sodium_data_decrypt <- function(bin, key, nonce = attr(bin, "nonce")) {
  assert_that(
    is.raw(bin),
    is.raw(key)
  )
  .Call(rsodium_crypto_secret_decrypt, bin, key, nonce)
}

sodium_hash <- function(buf, key = NULL, size = 32) {
  assert_that(
    is.raw(buf),
    is.null(key) || is.raw(key)
  )
  .Call(rsodium_crypto_generichash, buf, size, key)
}
