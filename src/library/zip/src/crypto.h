#ifndef ZIP_CRYPTO_H
#define ZIP_CRYPTO_H

#include <stddef.h>
#include <stdint.h>

/* Fill `buf` with `len` cryptographically random bytes from the operating
   system CSPRNG (/dev/urandom on Unix, BCryptGenRandom on Windows). Used to
   generate per-entry WinZip AES salts. Returns 0 on success, non-zero on
   failure. */
int zip_rand_bytes(unsigned char *buf, size_t len);

/* WinZip AES key-length / salt-length for a given strength byte.
   strength: 1 = AES-128, 2 = AES-192, 3 = AES-256.
   Return the relevant length in bytes, or -1 for an invalid strength. */
int zip_winzip_key_len(int strength);  /* 16 / 24 / 32 */
int zip_winzip_salt_len(int strength); /*  8 / 12 / 16 */

/* WinZip AES counter (CTR) mode keystream XOR. Encryption and decryption are
   the same operation. The counter is a 128-bit little-endian integer that
   starts at 1 for the first 16-byte block (matching WinZip / Dr Gladman's
   fcrypt), incremented per block, and the AES-encrypted counter blocks form
   the keystream that is XORed with the data.

   `keybits` must be 128, 192 or 256. `in`/`out` may alias.
   Returns 0 on success, non-zero on an mbedtls error. */
int zip_aes_ctr_crypt(const unsigned char *key, int keybits,
                      const unsigned char *in, unsigned char *out, size_t len);

/* PBKDF2-HMAC-SHA1 (RFC 2898 / RFC 6070). Writes `dklen` derived bytes to
   `out`. Returns 0 on success, non-zero on an mbedtls error. */
int zip_pbkdf2_sha1(const unsigned char *pw, size_t pwlen,
                    const unsigned char *salt, size_t saltlen,
                    unsigned int iterations, unsigned char *out, size_t dklen);

/* HMAC-SHA1 (RFC 2104 / RFC 2202). `out` must hold 20 bytes.
   Returns 0 on success, non-zero on an mbedtls error. */
int zip_hmac_sha1(const unsigned char *key, size_t keylen,
                  const unsigned char *data, size_t datalen,
                  unsigned char out[20]);

/* WinZip AES key block derivation: PBKDF2-HMAC-SHA1 with 1000 iterations over
   the password and salt, producing (encryption key || authentication key ||
   2-byte password verifier). `enc_key` and `mac_key` must each hold
   zip_winzip_key_len(strength) bytes; `verifier` holds 2 bytes.
   Returns 0 on success, -1 for an invalid strength, non-zero otherwise. */
int zip_winzip_aes_keys(const unsigned char *pw, size_t pwlen,
                        const unsigned char *salt, size_t saltlen,
                        int strength,
                        unsigned char *enc_key,
                        unsigned char *mac_key,
                        unsigned char verifier[2]);

/* ZipCrypto (Traditional PKWARE) stream cipher.
   Cryptographically weak — use only for legacy compatibility. */

typedef struct {
  uint32_t k0, k1, k2;
} zip_zipcrypto_keys_t;

/* Initialize key state from a password. */
void zip_zipcrypto_init(zip_zipcrypto_keys_t *keys,
                        const unsigned char *pw, size_t pwlen);

/* Encrypt `len` bytes in-place (keystream XOR; key updated with plaintext). */
void zip_zipcrypto_encrypt(zip_zipcrypto_keys_t *keys,
                           unsigned char *buf, size_t len);

/* Decrypt `len` bytes in-place (keystream XOR; key updated with plaintext).
   Unlike encrypt, the key is updated with the recovered plaintext byte. */
void zip_zipcrypto_decrypt(zip_zipcrypto_keys_t *keys,
                           unsigned char *buf, size_t len);

#endif /* ZIP_CRYPTO_H */
