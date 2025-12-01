# Authenticated repositories

pak supports HTTP basic authentication when interacting with CRAN-like
repositories. To use authentication, include a username in the repo URL:

    https://<username>@<repo-host>/<repo-path>

## Usage

``` r
repo_auth(
  r_version = getRversion(),
  bioc = NULL,
  cran_mirror = NULL,
  check_credentials = TRUE
)
```

## Arguments

- r_version:

  R version to use to determine the correct Bioconductor version, if
  `bioc = TRUE`.

- bioc:

  Whether to automatically add the Bioconductor repositories to the
  result.

- cran_mirror:

  CRAN mirror to use. Leave it at `NULL` to use the mirror in
  `getOption("repos")` or an automatically selected one.

- check_credentials:

  Whether to check that credentials are available for authenticated
  repositories.

## Value

Data frame with columns:

- all columns from the output of
  [`repo_get()`](https://pak.r-lib.org/reference/repo_get.md),

- `auth_domains`: authentication domains. pak tries to find the
  credentials for these domains, until the search is successful or all
  domains fail.

- `auth_domain`: if the credential lookup is successful, then this is
  the authentication domain that was used to get the credentials.

- `auth_source`: where the credentials were found. E.g.
  `keyring:<backend>` means it was in the default macos keyring.

- `auth_error`: for failed credential searches this is the description
  of why the search failed. E.g. maybe the keyring package is not
  installed, or pak found no credentials for any of the authentication
  domains.

## Details

pak will look up the password for this url and username from the the
user's `.netrc` file and from the system credential store using the
keyring package.

### \`.netrc“ files

First pak searches in the `.netrc` file. If the `NETRC` environment
variable is set, pak uses its value to determine the location of the
`netrc` file.

Otherwise pak looks for the `netrc` file in current user's home
directory, at `~/.netrc`. On Windows it also looks for `~/_netrc` if the
file starting with a dot does not exist.

If you create a `netrc` file, make sure that is only readable by you.
E.g. on Unix run

    chmod 600 ~/.netrc

`netrc` files are simple text files that can store passwords for
multiple hosts. They may contain three types of tokens:

#### `machine <hostname>`

A host name, without the protocol. Subsequent `login` and `password`
tokens belong to this host, until another `machine` token is found, or
the end of file.

#### `login <username>`

User name. It must be preceded by a `machine` token.

#### `password <password>`

Password. It must be preceded by a `machine` and a `login` token.

Whitespace is ignored in `netrc` files. You may include multiple tokens
on the same line, or have one token per line. Here is an example:

    machine myhost.mydomain.com login myuser password secret
    machine myhost2.mydomain.com
    login myuser
    password secret
    login anotheruser
    password stillsecret

If you need to include whitespace in a password, put the password in
double quotes.

### The system credential store

pak currently supports the following keyring backends:

- Windows credential store,

- macOS Keychain,

- Linux Secret Service via libsecret, if built with libsecret support,

- environment variables.

For the URL above it tries the following keyring keys, in this order:

    https://<username>@repo-host/<repo-path>
    https://repo-host/<repo-path>
    https://<username>@repo-host
    https://repo-host

To add an authenticated repository use
[`repo_add()`](https://pak.r-lib.org/reference/repo_add.md) with the
`username` argument. Alternatively, you can set the `repos` option
directly using [`base::options()`](https://rdrr.io/r/base/options.html)
and including the username in the repository URL.

`repo_auth()` lists authentication information for all configured
repositories.

## See also

[Authenticated
repositories](https://pak.r-lib.org/reference/repo-auth.md).

Other authenticated repositories:
[`Authenticated repositories`](https://pak.r-lib.org/reference/repo-auth.md),
[`repo_auth_key_get()`](https://pak.r-lib.org/reference/repo_auth_key_get.md)
