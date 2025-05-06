
#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

SEXP keyring_macos_get(SEXP keyring, SEXP service, SEXP username);
SEXP keyring_macos_set(SEXP keyring, SEXP service, SEXP username,
                       SEXP password);
SEXP keyring_macos_delete(SEXP keyring, SEXP service, SEXP username);
SEXP keyring_macos_list(SEXP keyring, SEXP service);
SEXP keyring_macos_create(SEXP keyring, SEXP password);
SEXP keyring_macos_list_keyring(void);
SEXP keyring_macos_delete_keyring(SEXP keyring);
SEXP keyring_macos_lock_keyring(SEXP keyring);
SEXP keyring_macos_unlock_keyring(SEXP keyring, SEXP password);
SEXP keyring_macos_is_locked_keyring(SEXP keyring);

SEXP keyring_wincred_get(SEXP);
SEXP keyring_wincred_exists(SEXP);
SEXP keyring_wincred_set(SEXP, SEXP, SEXP, SEXP);
SEXP keyring_wincred_delete(SEXP);
SEXP keyring_wincred_enumerate(SEXP);

SEXP keyring_secret_service_is_available(SEXP);
SEXP keyring_secret_service_get(SEXP, SEXP, SEXP);
SEXP keyring_secret_service_set(SEXP, SEXP, SEXP, SEXP);
SEXP keyring_secret_service_delete(SEXP, SEXP, SEXP);
SEXP keyring_secret_service_list(SEXP, SEXP);
SEXP keyring_secret_service_create_keyring(SEXP);
SEXP keyring_secret_service_list_keyring(void);
SEXP keyring_secret_service_delete_keyring(SEXP);
SEXP keyring_secret_service_lock_keyring(SEXP);
SEXP keyring_secret_service_unlock_keyring(SEXP, SEXP);
SEXP keyring_secret_service_is_locked_keyring(SEXP);

SEXP rsodium_randombytes_buf(SEXP);
SEXP rsodium_bin2hex(SEXP bin);
SEXP rsodium_hex2bin(SEXP hex, SEXP ignore);
SEXP rsodium_crypto_secret_encrypt(SEXP message, SEXP key, SEXP nonce);
SEXP rsodium_crypto_secret_decrypt(SEXP cipher, SEXP key, SEXP nonce);
SEXP rsodium_crypto_generichash(SEXP buf, SEXP size, SEXP key);

SEXP keyring_aes_cbc_decrypt(SEXP msg, SEXP key, SEXP iv);
SEXP keyring_aes_cbc_encrypt(SEXP msg, SEXP key, SEXP iv);
SEXP keyring_base64_decode(SEXP array);
SEXP keyring_base64_encode(SEXP array);
SEXP keyring_sha256(SEXP x, SEXP raw);

SEXP win_path_(SEXP _folder);

static const R_CallMethodDef callMethods[]  = {
  { "keyring_macos_get",    (DL_FUNC) &keyring_macos_get,            3 },
  { "keyring_macos_set",    (DL_FUNC) &keyring_macos_set,            4 },
  { "keyring_macos_delete", (DL_FUNC) &keyring_macos_delete,         3 },
  { "keyring_macos_list",   (DL_FUNC) &keyring_macos_list,           2 },
  { "keyring_macos_create", (DL_FUNC) &keyring_macos_create,         2 },
  { "keyring_macos_list_keyring",
    (DL_FUNC) &keyring_macos_list_keyring,   0 },
  { "keyring_macos_delete_keyring",
    (DL_FUNC) &keyring_macos_delete_keyring, 1 },
  { "keyring_macos_lock_keyring",
    (DL_FUNC) &keyring_macos_lock_keyring,   1 },
  { "keyring_macos_unlock_keyring",
    (DL_FUNC) &keyring_macos_unlock_keyring, 2 },
  { "keyring_macos_is_locked_keyring",
    (DL_FUNC) &keyring_macos_is_locked_keyring, 1 },

  { "keyring_wincred_get",       (DL_FUNC) &keyring_wincred_get,       1 },
  { "keyring_wincred_exists",    (DL_FUNC) &keyring_wincred_exists,    1 },
  { "keyring_wincred_set",       (DL_FUNC) &keyring_wincred_set,       4 },
  { "keyring_wincred_delete",    (DL_FUNC) &keyring_wincred_delete,    1 },
  { "keyring_wincred_enumerate", (DL_FUNC) &keyring_wincred_enumerate, 1 },

  { "keyring_secret_service_is_available",
    (DL_FUNC) &keyring_secret_service_is_available, 1 },
  { "keyring_secret_service_get",
    (DL_FUNC) &keyring_secret_service_get, 3 },
  { "keyring_secret_service_set",
    (DL_FUNC) &keyring_secret_service_set, 4 },
  { "keyring_secret_service_delete",
    (DL_FUNC) &keyring_secret_service_delete, 3 },
  { "keyring_secret_service_list",
    (DL_FUNC) &keyring_secret_service_list, 2 },
  { "keyring_secret_service_create_keyring",
    (DL_FUNC) &keyring_secret_service_create_keyring, 1 },
  { "keyring_secret_service_list_keyring",
    (DL_FUNC) &keyring_secret_service_list_keyring, 0 },
  { "keyring_secret_service_delete_keyring",
    (DL_FUNC) &keyring_secret_service_delete_keyring, 1 },
  { "keyring_secret_service_lock_keyring",
    (DL_FUNC) &keyring_secret_service_lock_keyring, 1 },
  { "keyring_secret_service_unlock_keyring",
    (DL_FUNC) &keyring_secret_service_unlock_keyring, 2 },
  { "keyring_secret_service_is_locked_keyring",
    (DL_FUNC) &keyring_secret_service_is_locked_keyring, 1 },

  { "rsodium_randombytes_buf",
    (DL_FUNC) &rsodium_randombytes_buf, 1 },
  { "rsodium_bin2hex",
    (DL_FUNC) &rsodium_bin2hex, 1 },
  { "rsodium_hex2bin",
    (DL_FUNC) &rsodium_hex2bin, 2 },
  { "rsodium_crypto_secret_encrypt",
    (DL_FUNC) &rsodium_crypto_secret_encrypt, 3 },
  { "rsodium_crypto_secret_decrypt",
    (DL_FUNC) &rsodium_crypto_secret_decrypt, 3 },
  { "rsodium_crypto_generichash",
    (DL_FUNC) &rsodium_crypto_generichash, 3 },

  { "keyring_aes_cbc_decrypt",
    (DL_FUNC) keyring_aes_cbc_decrypt, 3 },
  { "keyring_aes_cbc_encrypt",
    (DL_FUNC) keyring_aes_cbc_encrypt, 3 },
  { "keyring_base64_decode",
    (DL_FUNC) keyring_base64_decode, 1 },
  { "keyring_base64_encode",
    (DL_FUNC) keyring_base64_encode, 1 },
  { "keyring_sha256",
    (DL_FUNC) keyring_sha256, 2 },

  { "win_path_", (DL_FUNC) win_path_, 1 },

  { NULL, NULL, 0 }
};

void R_init_keyring(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
