#include <errno.h>
#include <stdlib.h>

#include <R.h>
#include <Rinternals.h>

#include "crypto_generichash.h"
#include "crypto_secretbox.h"
#include "sodium.h"

char *sodium_bin2hex(char *const hex, const size_t hex_maxlen,
                     const unsigned char *const bin, const size_t bin_len) {
  size_t i = (size_t)0U;
  unsigned int x;
  int b;
  int c;

  if (bin_len >= SIZE_MAX / 2 || hex_maxlen <= bin_len * 2U) {
    Rf_error("Internal sodium error");
  }
  while (i < bin_len) {
    c = bin[i] & 0xf;
    b = bin[i] >> 4;
    x = (unsigned char)(87U + c + (((c - 10U) >> 8) & ~38U)) << 8 |
        (unsigned char)(87U + b + (((b - 10U) >> 8) & ~38U));
    hex[i * 2U] = (char)x;
    x >>= 8;
    hex[i * 2U + 1U] = (char)x;
    i++;
  }
  hex[i * 2U] = 0U;

  return hex;
}

int sodium_hex2bin(unsigned char *const bin, const size_t bin_maxlen,
                   const char *const hex, const size_t hex_len,
                   const char *const ignore, size_t *const bin_len,
                   const char **const hex_end) {
  size_t bin_pos = (size_t)0U;
  size_t hex_pos = (size_t)0U;
  int ret = 0;
  unsigned char c;
  unsigned char c_acc = 0U;
  unsigned char c_alpha0, c_alpha;
  unsigned char c_num0, c_num;
  unsigned char c_val;
  unsigned char state = 0U;

  while (hex_pos < hex_len) {
    c = (unsigned char)hex[hex_pos];
    c_num = c ^ 48U;
    c_num0 = (c_num - 10U) >> 8;
    c_alpha = (c & ~32U) - 55U;
    c_alpha0 = ((c_alpha - 10U) ^ (c_alpha - 16U)) >> 8;
    if ((c_num0 | c_alpha0) == 0U) {
      if (ignore != NULL && state == 0U && strchr(ignore, c) != NULL) {
        hex_pos++;
        continue;
      }
      break;
    }
    c_val = (c_num0 & c_num) | (c_alpha0 & c_alpha);
    if (bin_pos >= bin_maxlen) {
      ret = -1;
      errno = ERANGE;
      break;
    }
    if (state == 0U) {
      c_acc = c_val * 16U;
    } else {
      bin[bin_pos++] = c_acc | c_val;
    }
    state = ~state;
    hex_pos++;
  }
  if (state != 0U) {
    hex_pos--;
    errno = EINVAL;
    ret = -1;
  }
  if (ret != 0) {
    bin_pos = (size_t)0U;
  }
  if (hex_end != NULL) {
    *hex_end = &hex[hex_pos];
  } else if (hex_pos != hex_len) {
    errno = EINVAL;
    ret = -1;
  }
  if (bin_len != NULL) {
    *bin_len = bin_pos;
  }
  return ret;
}

SEXP rsodium_randombytes_buf(SEXP length) {
  size_t size = Rf_asInteger(length);
  SEXP res = Rf_allocVector(RAWSXP, size);
  randombytes_buf(RAW(res), size);
  return res;
}

SEXP rsodium_bin2hex(SEXP bin) {
  size_t bin_len = LENGTH(bin);
  size_t hex_len = bin_len * 2 + 1;
  char *hex = malloc(hex_len);
  if (NULL == sodium_bin2hex(hex, hex_len, RAW(bin), bin_len)) {
    free(hex);
    Rf_error("Overflow error, failed to convert to hex");
  }
  SEXP res = Rf_mkString(hex);
  free(hex);
  return res;
}

SEXP rsodium_hex2bin(SEXP hex, SEXP ignore) {
  int hex_len = LENGTH(STRING_ELT(hex, 0));
  int max_len = hex_len / 2;
  unsigned char *bin = malloc(max_len);
  size_t bin_len;
  const char *hex_end;
  if (sodium_hex2bin(bin, max_len, CHAR(STRING_ELT(hex, 0)), hex_len,
                     CHAR(STRING_ELT(ignore, 0)), &bin_len, &hex_end)) {
    free(bin);
    Rf_error("Overflow error, failed to parse hex.");
  }
  SEXP res = Rf_allocVector(RAWSXP, bin_len);
  memcpy(RAW(res), bin, bin_len);
  free(bin);
  return res;
}

SEXP rsodium_crypto_secret_encrypt(SEXP message, SEXP key, SEXP nonce) {
  if (LENGTH(key) != crypto_secretbox_KEYBYTES) {
    Rf_error("Invalid key: must be exactly %d bytes",
             crypto_secretbox_KEYBYTES);
  }
  if (LENGTH(nonce) != crypto_secretbox_NONCEBYTES) {
    Rf_error("Invalid nonce: must be exactly %d bytes",
             crypto_secretbox_NONCEBYTES);
  }

  R_xlen_t mlen = XLENGTH(message);
  R_xlen_t clen = mlen + crypto_secretbox_MACBYTES;
  SEXP res = PROTECT(Rf_allocVector(RAWSXP, clen));

  if (crypto_secretbox_easy(RAW(res), RAW(message), mlen, RAW(nonce),
                            RAW(key))) {
    Rf_error("Failed to encrypt");
  }

  UNPROTECT(1);
  return res;
}

SEXP rsodium_crypto_secret_decrypt(SEXP cipher, SEXP key, SEXP nonce) {
  if (LENGTH(key) != crypto_secretbox_KEYBYTES) {
    Rf_error("Invalid key. Key must be exactly %d bytes",
             crypto_secretbox_KEYBYTES);
  }
  if (LENGTH(nonce) != crypto_secretbox_NONCEBYTES) {
    Rf_error("Invalid key. Key must be exactly %d bytes",
             crypto_secretbox_NONCEBYTES);
  }

  R_xlen_t clen = XLENGTH(cipher);
  R_xlen_t mlen = clen - crypto_secretbox_MACBYTES;
  SEXP res = PROTECT(Rf_allocVector(RAWSXP, mlen));

  if (crypto_secretbox_open_easy(RAW(res), RAW(cipher), clen, RAW(nonce),
                                 RAW(key))) {
    Rf_error("Failed to decrypt");
  }

  UNPROTECT(1);
  return res;
}

SEXP rsodium_crypto_generichash(SEXP buf, SEXP size, SEXP key) {
  int outlen = Rf_asInteger(size);
  if (outlen < crypto_generichash_BYTES_MIN ||
      outlen > crypto_generichash_BYTES_MAX) {
    Rf_error("Invalid output length, must be in between %d and %d",
             crypto_generichash_BYTES_MIN, crypto_generichash_BYTES_MAX);
  }

  unsigned char *keyval = NULL;
  int keysize = 0;
  if (key != R_NilValue) {
    keysize = LENGTH(key);
    keyval = RAW(key);
    if (keysize < crypto_generichash_KEYBYTES_MIN ||
        keysize > crypto_generichash_KEYBYTES_MAX) {
      Rf_error("Invalid key size, must be between %d and %d bytes",
               crypto_generichash_KEYBYTES_MIN,
               crypto_generichash_KEYBYTES_MAX);
    }
  }

  SEXP res = PROTECT(Rf_allocVector(RAWSXP, outlen));
  if (crypto_generichash(RAW(res), outlen, RAW(buf), XLENGTH(buf), keyval,
                         keysize)) {
    Rf_error("Failed to hash");
  }

  UNPROTECT(1);
  return res;
}
