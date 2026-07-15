/* Minimal Mbed TLS configuration for the `zip` package.
 *
 * Only the pieces needed for WinZip AES (method 99) are enabled:
 *   - AES        (AES-128/192/256, runtime key size)  -> MBEDTLS_AES_C
 *   - SHA-1      (for HMAC-SHA1 and PBKDF2)            -> MBEDTLS_SHA1_C
 *   - MD layer   (provides HMAC-SHA1)                  -> MBEDTLS_MD_C
 *   - PKCS#5     (provides PBKDF2-HMAC-SHA1)           -> MBEDTLS_PKCS5_C
 *   - Platform   (required on Windows/MinGW, see below)-> MBEDTLS_PLATFORM_C
 *
 * MBEDTLS_PLATFORM_C is mandated by check_config.h on MinGW/old MSVC, where
 * Mbed TLS auto-enables the snprintf/vsnprintf "ALT" wrappers implemented in
 * platform.c. With this minimal config platform.c is effectively empty on
 * other platforms.
 *
 * Deliberately NOT enabled: PSA crypto, ASN.1, cipher/OID layers, TLS, x509,
 * bignum, ECC, RSA, etc. This keeps the compiled subset to a handful of files
 * (see src/Makevars) and the PBES2/ASN.1 paths in pkcs5.c compiled out.
 *
 * Selected via -DMBEDTLS_CONFIG_FILE='"zip_mbedtls_config.h"'.
 */
#ifndef ZIP_MBEDTLS_CONFIG_H
#define ZIP_MBEDTLS_CONFIG_H

#define MBEDTLS_AES_C
#define MBEDTLS_SHA1_C
#define MBEDTLS_MD_C
#define MBEDTLS_PKCS5_C
#define MBEDTLS_PLATFORM_C

#endif /* ZIP_MBEDTLS_CONFIG_H */
