# Query or set repository password in the system credential store

Use pak's keyring functions to query or set a repository password in the
system credential store.

## Usage

``` r
repo_auth_key_get(url, username = NULL)

repo_auth_key_set(url, password, username = NULL)

repo_auth_unlock(keyring_password)
```

## Arguments

- url:

  Repository URL. It may contain a username, in which case `username`
  may be `NULL`.

- username:

  User name, if it is not included in `url`.

- password:

  Password (key) to set.

- keyring_password:

  Password to unlock the keyring.

## Value

`repo_auth_key_get()` returns a single string, the repository password.

## Details

`repo_auth_key_get()` retrieves a password from the default keyring. It
errors if it cannot find the credentials for `url`.

`repo_auth_key_set()` adds or updates a password in the system
credential store.

`repo_auth_unlock()` unlocks the default keyring, if it is locked. You
might need this if the keyring is locked. If you are using ecrypted
files to store the keys, then you typically need to call this function
in each session. You typically don't need to do that if you are using
the native Windows, macOS or Linux (Secret Service) backends.

## See also

Other authenticated repositories:
[`Authenticated repositories`](https://pak.r-lib.org/dev/reference/repo-auth.md),
[`repo_auth()`](https://pak.r-lib.org/dev/reference/repo_auth.md)
