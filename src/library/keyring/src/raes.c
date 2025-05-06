#include <string.h>

#include <Rinternals.h>

#include "mbedtls/private_access.h"
#include "mbedtls/aes.h"

// silence mbedtls functions
int mbedtls_printf(const char *format, ...) {
  return 0;
}

SEXP keyring_aes_cbc(SEXP msg, SEXP key, SEXP iv, int decrypt) {
  if (Rf_length(key) != 32) {
    Rf_error("Invalid 'key', must have 32 bytes");
  }
  if (Rf_length(iv) != 16) {
    Rf_error("Invalid 'iv', must have 16 bytes");
  }
  const unsigned char *ckey = (const unsigned char*) RAW(key);
  unsigned char civ[16];
  memcpy(civ, RAW(iv), sizeof(civ));

  size_t msglen = Rf_length(msg);
  int rem = msglen % 16;
  if (decrypt && rem != 0) {
    Rf_error("Invalid message length, must be multiple of 16");
  }
  int padlen = decrypt ? 0 : 16 - rem;

  size_t paddedlen = msglen + padlen;
  SEXP input = PROTECT(Rf_allocVector(RAWSXP, paddedlen));
  SEXP output = PROTECT(Rf_allocVector(RAWSXP, paddedlen));
  memcpy(RAW(input), RAW(msg), msglen);
  if (padlen > 0) {
    memset(RAW(input) + msglen, padlen, padlen);
  }

  mbedtls_aes_context aes;
  mbedtls_aes_init(&aes);
  int ret;
  if (decrypt) {
    ret = mbedtls_aes_setkey_dec(&aes, ckey, 256);
  } else {
    ret = mbedtls_aes_setkey_enc(&aes, ckey, 256);
  }
  if (ret) {
    Rf_error("'mbedtls_aes_setkey_enc' failure, internal keyring error");
  }
  ret = mbedtls_aes_crypt_cbc(
    &aes,
    decrypt ? MBEDTLS_AES_DECRYPT : MBEDTLS_AES_ENCRYPT,
    paddedlen,
    civ,
    RAW(input),
    RAW(output)
  );
  if (ret) {
    Rf_error("'mbedtls_aes_crypt_cbc' failure, internal keyring error");
  }

  if (decrypt) {
    padlen = RAW(output)[msglen - 1];
    if (padlen == 0 || padlen > 16 || padlen > msglen) {
      Rf_error("Cannot decrypt AES CBC, probably wrong key?");
    }
    output = PROTECT(Rf_lengthgets(output, msglen - padlen));
    UNPROTECT(3);
    return output;
  } else {
    UNPROTECT(2);
    return output;
  }
}

SEXP keyring_aes_cbc_encrypt(SEXP msg, SEXP key, SEXP iv) {
  return keyring_aes_cbc(msg, key, iv, /* decrypt= */ 0);
}

SEXP keyring_aes_cbc_decrypt(SEXP msg, SEXP key, SEXP iv) {
  return keyring_aes_cbc(msg, key, iv, /* decrypt= */ 1);
}
