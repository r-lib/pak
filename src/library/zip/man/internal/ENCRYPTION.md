# Password / encryption support in the `zip` package

This is the internal reference for how the package encrypts and (eventually)
decrypts ZIP entries. It documents the on-disk format, the C and R layers, and
the design decisions behind them.

## Scheme: WinZip AES (method 99)

The package implements **WinZip AES** encryption, the de-facto standard read by
7-Zip, WinZip, modern Info-ZIP and macOS Archive Utility. It is real crypto:

- **AES-128/192/256 in CTR mode** for the data,
- key derived with **PBKDF2-HMAC-SHA1, 1000 iterations** from the password and a
  per-entry random salt,
- an **HMAC-SHA1** authentication tag (truncated to 10 bytes) over the
  ciphertext.

The legacy traditional-PKWARE cipher ("ZipCrypto") is **not** implemented;
`encryption = "zipcrypto"` raises an error in R. It is cryptographically broken
and only kept on the roadmap for completeness.

### On-disk layout of an encrypted entry

In the ZIP format, encryption is applied to the *already-compressed* bytes, so
the pipeline is just an extra stage around miniz's deflate/inflate:

```
write:  plaintext --deflate(miniz)--> compressed --encrypt--> entry payload
read:   entry payload --decrypt--> compressed --inflate(miniz)--> plaintext
```

A WinZip AES entry uses **compression method 99 (0x63)** and sets **bit 0** of
the general-purpose flag word (the "encrypted" bit; we also set the UTF-8 bit
0x800). The real compression method (0 = store, 8 = deflate) and the AES
parameters live in an **extra field with header id `0x9901`**, present in *both*
the local header and the central directory record:

```
0x9901 | datasize(2)=7 | vendor version(2) | vendor id "AE"(2) |
        AES strength(1: 1=128, 2=192, 3=256) | real compression method(2)
```

The entry payload (what follows the local header + filename + extra field) is:

```
salt (8/12/16 bytes for AES-128/192/256) || 2-byte password verifier ||
ciphertext || 10-byte HMAC-SHA1 authentication code
```

We emit **AE-2** (vendor version 2), which stores the CRC-32 as 0 in the headers
and relies on the HMAC for integrity. (AE-1 would instead keep the real CRC.)

## Code map

### Crypto primitives — [crypto.c](crypto.c) / [crypto.h](crypto.h)

Built on the vendored Mbed TLS subset (see [mbedtls/VENDORING.md](mbedtls/VENDORING.md));
no hand-written crypto. R-free, so it can also link into `cmdzip`/`cmdunzip`.

- `zip_aes_ctr_crypt` — WinZip AES-CTR (128/192/256; little-endian counter
  starting at 1, keystream from `mbedtls_aes_crypt_ecb`; encrypt == decrypt).
- `zip_pbkdf2_sha1`, `zip_hmac_sha1` — PBKDF2-HMAC-SHA1 and HMAC-SHA1.
- `zip_winzip_aes_keys` (+ `zip_winzip_key_len` / `zip_winzip_salt_len`) —
  one PBKDF2(1000) call producing `enc key || auth key || 2-byte verifier`.
- `zip_rand_bytes` — OS CSPRNG for per-entry salts (`/dev/urandom` on Unix,
  `BCryptGenRandom` on Windows; the latter needs `-lbcrypt`, set in
  `Makevars.win`).

Hidden `.Call` test shims (`R_crypto_*` in [rzip.c](rzip.c)) exercise these
against published vectors in `tests/testthat/test-crypto.R`.

### miniz primitive — [miniz.c](miniz.c) (`mz_zip_writer_add_mem_raw`)

miniz's own writer hardcodes the compression method and never sets the encrypted
bit, so it cannot emit a method-99 entry. The added function stores a
caller-framed payload verbatim with an explicit method, general-purpose bit
flags, CRC-32 and uncompressed size, plus the local + central `0x9901` extra
field. Unlike `mz_zip_writer_add_mem_ex_v2` it writes the real sizes into the
local header and emits **no data descriptor**. This is a local patch to the
vendored miniz, tracked in [../tools/extra/miniz.patch](../tools/extra/miniz.patch).

### Writer — [zip.c](zip.c) (`zip_writer_add_aes`, `zip_winzip_extra_field`)

For each file entry (when a password is set): raw-deflate the plaintext (falling
back to *stored* if it does not shrink, mirroring miniz), generate a random
salt, derive the keys + verifier, AES-CTR encrypt, compute the HMAC auth code,
assemble the payload, and write it via `mz_zip_writer_add_mem_raw` with method 99
and the encrypted flag. Key material is scrubbed from the stack afterwards.
Directory entries carry no data and stay unencrypted. Both the create path and
the in-place/rebuild append path are covered.

`zip_zip()` takes `cpassword` / `cpassword_len` / `cencryption` (see
`zip_encryption_t` in [zip.h](zip.h): 0 = none, 1/2/3 = AES-128/192/256).

### R layer — [../R/utils.R](../R/utils.R), [../R/zip.R](../R/zip.R)

- `resolve_password()` turns the `password` argument (string, raw vector, or a
  function/callback returning one — so it need not sit in a variable) into raw
  bytes. **Password byte contract:** text passwords are encoded as **UTF-8**
  bytes, matching WinZip. `NULL` means no encryption.
- `encryption_code()` maps `"aes256"`/`"aes128"` to the C scheme code (AES-192,
  code 2, is intentionally not offered; `"zipcrypto"` errors).
- These are threaded through `zip_internal()` into `.Call(c_R_zip_zip, ...)`.

## Testing

- **Unit:** crypto vectors in `tests/testthat/test-crypto.R`.
- **Writer:** `tests/testthat/test-zip-encrypt.R` decrypts our own output
  in-process via the `R_crypto_*` shims (always runs) and, when 7-Zip is
  installed, checks that `7zz` extracts what we produce and rejects wrong
  passwords.
- **Inbound interop fixtures:** archives encrypted by *external* tools live in
  `tests/testthat/fixtures/` (regenerate with `make-encrypted-fixtures.sh`),
  for the reader/extract path.

## Status / TODO

- [x] Crypto primitives, miniz `add_mem_raw`, WinZip AES writer, R plumbing
  down to `zip_internal()`.
- [ ] Reader / extract path (decrypt method-99 entries on `unzip()`).
- [ ] `encryption` column in `zip_list()`.
- [ ] Expose `password` / `encryption` on the public `zip()` / `zipr()` /
  `*_append()` functions, and on the `cmdzip` / `cmdunzip` background variants.
- [ ] ZipCrypto (legacy), if ever wanted.
