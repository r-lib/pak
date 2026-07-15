# Vendored Mbed TLS subset

This directory contains a **trimmed subset** of [Mbed TLS](https://github.com/Mbed-TLS/mbedtls),
used only for WinZip AES (ZIP compression method 99): AES (runtime 128/192/256),
SHA-1, HMAC-SHA1 and PBKDF2-HMAC-SHA1. See `man/internal/ENCRYPTION.md`.

- **Upstream:** https://github.com/Mbed-TLS/mbedtls (branch `mbedtls-3.6`)
- **Version:** 3.6.6 (see `include/mbedtls/build_info.h`)
- **License:** Apache-2.0 OR GPL-2.0-or-later (we use Apache-2.0). See `LICENSE`.
- **Local changes:** none to upstream files. The only added file is
  `include/zip_mbedtls_config.h`, our minimal configuration, selected via
  `-DMBEDTLS_CONFIG_FILE='"zip_mbedtls_config.h"'` (see `src/Makevars`).

## What is included

Compiled (listed in `src/Makevars` `MBEDTLS_SRC`):

    library/aes.c  library/sha1.c  library/md.c  library/pkcs5.c  library/platform_util.c

Plus the exact transitive set of headers those files need. PSA crypto, ASN.1,
cipher/OID, TLS, x509, bignum, ECC and RSA are NOT enabled in the config, so the
PBES2/ASN.1 code in `pkcs5.c` compiles out and no further `.c` files are required.

## How to refresh to a new upstream version

```sh
git clone --depth 1 -b mbedtls-3.6 https://github.com/Mbed-TLS/mbedtls /tmp/mbedtls
cd path/to/zip/src

# 1. copy the compiled sources + all headers, then prune to the closure below
#    (the 5 .c files above are the full compiled set; do not add others without
#     re-checking the config)

# 2. determine the exact header closure with the compiler and keep only those:
DEF='-DMBEDTLS_CONFIG_FILE="zip_mbedtls_config.h"'
for f in aes sha1 md pkcs5 platform_util; do
  cc -Imbedtls/include -Imbedtls/library "$DEF" -MM -MG mbedtls/library/$f.c
done

# 3. verify against test vectors (AES NIST SP800-38A, PBKDF2 RFC 6070,
#    HMAC-SHA1 RFC 2202) before committing.
```

Keep `include/zip_mbedtls_config.h` across refreshes; check `check_config.h` does
not newly require options we have disabled.
