
# Notes for `keyring` developers

   * [Introduction](#introduction)
   * [Backend implementation details](#backend-implementation-details)
      * [macOS Keychain](#macos-keychain)
      * [Windows Credential Store](#windows-credential-store)
         * [Multiple keyrings](#multiple-keyrings)
      * [Secret Service API](#secret-service-api)
         * [Unloading the package](#unloading-the-package)
      * [Environment variable backend](#environment-variable-backend)
   * [Limits on secret sizes](#limits-on-secret-sizes)
      * [macOS Keychain](#macos-keychain-1)
      * [Windows Credential Store](#windows-credential-store-1)
      * [Linux Secret Service](#linux-secret-service)
   * [Testing the package](#testing-the-package)
      * [Test keyrings and keyring items](#test-keyrings-and-keyring-items)

## Introduction

This document is aimed at developers that want to improve the
existing keyring backends, or want to implement new backends.
You don't need it for using `keyring`.

## Backend implementation details

### macOS Keychain

On macOS, the keyrings are store in files. We handle the user's keyrings,
these are in `~/Library/Keychains`. They are files with extension
`.keychain` (before Sierra) or `.keychain-db` (starting from Sierra).

Whenever a keyring is specified, it can be a symbolic name (e.g. `login`),
or an absolute filename (e.g. `/Users/gaborcsardi/Library/Keychains/login.keychain`).
If a symbolic name is specified, we look for both `.keychain` and
`.keychain-db` files:
* If the `.keychain` file exists, we use that.
* Otherwise, if the `.keychain-db` file exists, we use that.
* Otherwise, if the system is Sierra or later, we use `.keychain-db`.
* Otherwise we use the `.keychain` file.

### Windows Credential Store

We use the *old* API to the Windows Credential Store. See e.g.
https://msdn.microsoft.com/en-us/library/windows/desktop/aa374804%28v%3Dvs.85%29.aspx
and also the `CREDENTIAL`, `CredWrite`, `CredDelete`, `CredEnumerate` there.
The reason for this is that MinGW does not provide a wrapper to the
*new* API, introduced in Windows 8.x. This also means that we don't have
access to the secrets stored by recent Windows web browsers (IE and Edge).

#### Multiple keyrings

The *old* Windows Credential Store does not support multiple keyrings,
and it does not support locking and unlocking the (single) keyring, either.

For every (non-default) keyring, we create a credential, with target name
`keyring::`. This credential contains metadata about the keyring. It
currently has the following (DCF, Debian Control File) format:

```
Version: 1.0.0
Verify: NgF+vkkNsOoSnXVXt249u6xknskhDasMIhE8Uuzpl/w=
Salt: some random salt
```

The `Verify` tag is used to check if the keyring password that was
specified to unlock the keyring, was correct.

The `Salt` tag is used to salt the SHA256 hash, to make it more secure.
It is generated randomly when the keyring is created.

When a keyring is unlocked, the user specifies the pass phrase of the
keyring. We create the SHA256 hash of this pass phrase, and this will be the
AES key to encrypt/decrypt the items in the keyring. When unlocking a
keyring, we use the `Verify` field, to see if the supplied password indeed
hashes to the correct AES key. If it can decrypt the `Verify` string, then
it is correct.

We also store the AES key in the keyring, in a session credential with
target name `keyring::unlocked`. A session credential's life time is the
life time of a single login session. The AES key is stored in a base64
encoded form, e.g.:

```
JvL7srqc0X1vVnqbSayFnIkJZoe2xMOWoDh+aBR9DJc=
```

The credentials of the keyring itself have target names as
`keyring:service:username`, where the username may be empty.
If keyring is empty, then the credential is considered to be
on the default keyring, and it is not encrypted. Credentials on
other keyrings are encrypted using the AES key of the keyring.
The random initialization vector of the encryption is stored as the
first 16 bytes of the keyring item.

When we `set` a key, we need to:

1. Check if the key is on the default keyring.
2. If 1. is TRUE, then just set the key, using target name
   `:service:username` (username might be empty, servicename not),
   and finish.
3. Check if the keyring exists.
4. If 3. is FALSE, then error and finish.
5. Check that the keyring is unlocked.
6. If 5. is FALSE, then prompt the user and unlock the keyring.
7. Encrypt the key with the AES key, and store the encrypted
   key using target name `keyring:service:username` (again, username
   might be empty, service name not).

When we `get` a key, we need to:

1. Check if the key is on the default keyring.
2. If 1. is TRUE, then we just get the key, using target name
   `:service:username`.
3. Check if the keyring is locked.
4. If 3. is TRUE, then prompt the user and unlock the keyring.
5. Get the AES key from the unlocked keyring.
6. Get the key and use the AES key to decrypt it.

To unlock a keyring we need to:

1. Get the keyring password and SHA256 hash it.
2. Try to decrypt the `Verify` field of the keyring metadata,
   to see if the password was correct.
3. If the password was not correct, then error out.
4. If the password was correct, then store the AES key under target
   name `keyring::unlocked`.

The C functions for this backend do not know about multiple
keyrings at all, we just use them to get/set/delete/list "regular"
credentials in the credential store.

### Secret Service API

The Secret Service API works on DBUS, and multiple daemons support it.
On GNOME based systems, typically gnome-keyring, and on KDE based
systems typically KWallet is used.

Linux systems lacking a GUI typically do not run a secret service daemon,
and `keyring` cannot use the OS credential store on these systems.
The default backend is the environment variable based one (`backend_env`)
on these systems.

#### Prompts

For the Secret Service backend, it is currently not possible to specify
a password for some functions. Instead, the password is read in
interactively on this backend, by the system. This currently happens
when a new keyring is created, and when a keyring is unlocked.

#### Unloading the package

`libsecret` uses `libglib`, and `libglib` does not allow unloading
the package. More precisely, if you unload the `keyring` package on
Linux, then some `libglib` threads will stay around. If you then
reload the package, R will crash.

This is a known limitation of `libglib`, and there is currently no
way around it. For `keyring` a workaround would be to use `libdbus`
to communicate with the Secret Service daemon directly. `libdbus` is
a standalone DBUS library, it does not use `libglib`. There is an
issue for this in our issue tracker, but no plans currently about when
it would happen: https://github.com/r-lib/keyring/issues/15

This limitation is especially annoying for development with `devtools`,
because `devtools::load_all()`, `devtools::test()`, etc. try to
unload and reload the package, which results in crashes. A workaround
is to run the tests from the command line:

```
R -e 'devtools::test()'
```

### Environment variable backend

When an item has a username on this backend, the service name and the
username is separated with a colon (`:`) character in the created
environment variable:

```
service:username
```

Note that shells typically cannot read or write these environment
variables. Programming languages, i.e. `Sys.getenv()` and `Sys.setenv()`
in R, or the similar functions in Python, C, etc. can read and write
them just fine.

## Limits on secret sizes

Various backends (and platforms) have various limits on the size of
the secrets they can store. In general, the safest to assume that the
secret can be maximum about 400 bytes long. If you want to store something
longer, encrypt it with AES, and store it in a regular file, and store the
AES key in the keyring. See the `openssl` package for AES encryption
and decryption.

### macOS Keychain

The macOS keychain can store very big secrets. In our experiments it had
no problems with a secret of 1GB, although that is already a little slow
to encrypt and decrypt. The real limit is probably (much) higher, and it
could be the limit on the keychain file's size.

### Windows Credential Store

According to the documentation in the Windows Credential Store the
maximum secret size is 512 bytes:
https://msdn.microsoft.com/en-us/library/windows/desktop/aa374788%28v%3Dvs.85%29.aspx

In practice, on Windows 8.1 we managed to store secrets of up to 2560 bytes.
Other Windows versions might have other limits, though. Note that if you
use the non-default keyring, then the actual secret stored by `keyring` is
longer, because it is encrypted manually, and also BASE64 encoded. Hence the
advised 400 bytes limit.

### Linux Secret Service

According to our tests with the Secret Service, it can store secrets of at
least 25MB. We used an Ubuntu 16.04 system and the gnome-keyring daemon
for this. Note, however, that these large secrets only work if you first
create a small keyring item, and then update its contents. Creating a
large keyring item straight away gives an error.

## Testing the package

Testing the package on CIs or local machines is tricky. Here are some
notes.

### Test keyrings and keyring items

When running tests (and examples), the package only creates keyrings
with a name prefix `Rkeyringtest`, and on other keyrings it only creates
services with the name prefix `R-keyring-test-service-`. This makes
it easier to remove the leftover keyrings and keyring items manually
from the system keyring.
