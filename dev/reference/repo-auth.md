# Authenticated repositories

pak supports HTTP basic authentication when interacting with CRAN-like
repositories.

### Configuring authenticated repositories

To use authentication you need to include a user name in the repository
URL. You can set the repository URL in the `repos` option with
[`base::options()`](https://rdrr.io/r/base/options.html) as usual, or
you can use
[`repo_add()`](https://pak.r-lib.org/dev/reference/repo_add.md).

For testing purposes pak includes a web app that creates an
authenticated proxy to CRAN. This is how to run the proxy in a
subprocess:

    repo <- webfakes::new_app_process(pak:::auth_proxy_app())
    repo$url()
    #> [1] "http://127.0.0.1:59571/"

(This needs the webfakes and callr packages.)

Next, we configure the proxy as the main CRAN repository. The default
username of the proxy is `"username"` and the default password is
`"token"`. We want to replace the default CRAN repository with the
proxy, so we name it `CRAN`:

    repo_add(CRAN = repo$url(), username = "username")
    repo_get()
    #> x Did not find credentials for repo <http://username@127.0.0.1:59571/>, keyring
    #> lookup failed (macos backend).
    #> # A data frame: 6 x 7
    #>   name          url           type  r_version bioc_version username has_password
    #> * <chr>         <chr>         <chr> <chr>     <chr>        <chr>    <lgl>
    #> 1 CRAN          http://usern~ cran  *         <NA>         username FALSE
    #> 2 BioCsoft      https://bioc~ bioc  4.4.2     3.20         <NA>     NA
    #> 3 BioCann       https://bioc~ bioc  4.4.2     3.20         <NA>     NA
    #> 4 BioCexp       https://bioc~ bioc  4.4.2     3.20         <NA>     NA
    #> 5 BioCworkflows https://bioc~ bioc  4.4.2     3.20         <NA>     NA
    #> 6 BioCbooks     https://bioc~ bioc  4.4.2     3.20         <NA>     NA

Note that the output includes a `username` and a `has_password` column.
These are only present if at least one configured repository needs
authentication. `has_password` is `FALSE` here, because pak did not find
the credentials for this repository.

[`repo_get()`](https://pak.r-lib.org/dev/reference/repo_get.md) also
displays a message if it cannot find the credentials for an
authenticated repository.

Next we are going to store the credentials in a place where pak can find
them.

### Credential lookup

pak can look up credentials from two sources:

1.  The current user's `netrc` file.

2.  The system credential store via the keyring package. pak comes with
    its own copy of the keyring package, you don't need to install it
    separately.

#### `netrc` files

If the `NETRC` environment variable is set, pak uses its value to
determine the location of the `netrc` file.

Otherwise pak looks for the `netrc` file in current user's home
directory, at `~/.netrc`. On Windows it also looks for `~/_netrc` if the
file starting with a dot does not exist.

If you create a `netrc` file, make sure that is only readable by you.
E.g. on Unix run

    chmod 600 ~/.netrc

`netrc` files are simple text files that can store passwords for
multiple hosts. They may contain three types of tokens:

##### `machine <hostname>`

A host name, without the protocol. Subsequent `login` and `password`
tokens belong to this host, until another `machine` token is found, or
the end of file.

##### `login <username>`

User name. It must be preceded by a `machine` token.

##### `password <password>`

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

#### The credential store

pak uses the keyring package to query the system credential store (or an
alternative keyring credential store) to find credentials for
authenticated repositories. pak comes with a copy of the keyring
package, so you don't need to install it separately.

To store a repository password in the system credential store use the
[`repo_auth_key_set()`](https://pak.r-lib.org/dev/reference/repo_auth_key_get.md)
function. If you want to use a non-default keyring backend, set the
`keyring_backend` option. In this manual we will use the backend that
stores secrets in environment variables. This is an ephemeral store that
is destroyed when the R process terminates.

To continue our example from above:

    options(keyring_backend = "env")
    repo_auth_key_set(repo$url(), username = "username", password = "token")

Use
[`repo_auth_key_get()`](https://pak.r-lib.org/dev/reference/repo_auth_key_get.md)
to check that the key is properly set:

    repo_auth_key_get(repo$url(), username = "username")
    #> [1] "token"

[`repo_get()`](https://pak.r-lib.org/dev/reference/repo_get.md) now does
not show a warning message, and also sets the `has_password` column to
`TRUE`, because pak could find the credentials for our CRAN proxy:

    repo_get()
    #> x Did not find credentials for repo <http://username@127.0.0.1:59571/>, keyring
    #> lookup failed (macos backend).
    #> # A data frame: 6 x 7
    #>   name          url           type  r_version bioc_version username has_password
    #> * <chr>         <chr>         <chr> <chr>     <chr>        <chr>    <lgl>
    #> 1 CRAN          http://usern~ cran  *         <NA>         username FALSE
    #> 2 BioCsoft      https://bioc~ bioc  4.4.2     3.20         <NA>     NA
    #> 3 BioCann       https://bioc~ bioc  4.4.2     3.20         <NA>     NA
    #> 4 BioCexp       https://bioc~ bioc  4.4.2     3.20         <NA>     NA
    #> 5 BioCworkflows https://bioc~ bioc  4.4.2     3.20         <NA>     NA
    #> 6 BioCbooks     https://bioc~ bioc  4.4.2     3.20         <NA>     NA

#### Repo vs. host credentials

pak handles credentials for repositories and hosts. A repository
credential's key is a URL with a non-empty path:

    https://repo.host.com/repos/repo1

A host credential's is an URL with an empty path:

    https://repo.host.com

pak always looks for repository credentials first. If it does not find
any credentials for a repository then it drops the path and looks for
host credentials.

Because `netrc` files only store domain names and not URLs, they can
only contain host credentials.

### Testing

To test that authentication works, use the
[`repo_status()`](https://pak.r-lib.org/dev/reference/repo_status.md)
function:

    repo_status()
    #> x Did not find credentials for repo <http://username@127.0.0.1:59571/>, keyring
    #> lookup failed (macos backend).
    #> # A data frame: 12 x 12
    #>    name  url   type  bioc_version username has_password platform path  r_version
    #>    <chr> <chr> <chr> <chr>        <chr>    <lgl>        <chr>    <chr> <chr>
    #>  1 CRAN  http~ cran  <NA>         username FALSE        source   src/~ 4.4
    #>  2 CRAN  http~ cran  <NA>         username FALSE        aarch64~ bin/~ 4.4
    #>  3 BioC~ http~ bioc  3.20         <NA>     NA           source   src/~ 4.4
    #>  4 BioC~ http~ bioc  3.20         <NA>     NA           aarch64~ bin/~ 4.4
    #>  5 BioC~ http~ bioc  3.20         <NA>     NA           source   src/~ 4.4
    #>  6 BioC~ http~ bioc  3.20         <NA>     NA           aarch64~ bin/~ 4.4
    #>  7 BioC~ http~ bioc  3.20         <NA>     NA           source   src/~ 4.4
    #>  8 BioC~ http~ bioc  3.20         <NA>     NA           aarch64~ bin/~ 4.4
    #>  9 BioC~ http~ bioc  3.20         <NA>     NA           source   src/~ 4.4
    #> 10 BioC~ http~ bioc  3.20         <NA>     NA           aarch64~ bin/~ 4.4
    #> 11 BioC~ http~ bioc  3.20         <NA>     NA           source   src/~ 4.4
    #> 12 BioC~ http~ bioc  3.20         <NA>     NA           aarch64~ bin/~ 4.4
    #> # i 3 more variables: ok <lgl>, ping <dbl>, error <list>

The output of
[`repo_status()`](https://pak.r-lib.org/dev/reference/repo_status.md)
has extra columns, compared to
[`repo_get()`](https://pak.r-lib.org/dev/reference/repo_get.md), and it
also has a separate row for each platform. If everything works, then the
`has_password` column is `TRUE` for authenticated repositories, and the
`ok` column is `TRUE` if
[`repo_status()`](https://pak.r-lib.org/dev/reference/repo_status.md)
was able to perform an (authenticated) HTTP HEAD request to the metadata
file of a platform in a repository.

If you need even more information about repo authentication, e.g.
because
[`repo_status()`](https://pak.r-lib.org/dev/reference/repo_status.md)
shows some failures, then use the
[`repo_auth()`](https://pak.r-lib.org/dev/reference/repo_auth.md)
function:

    repo_auth()
    #> v Found credentials for repo <http://username@127.0.0.1:59571/> (keyring:env).
    #> # A data frame: 1 x 11
    #>   name  url              type  r_version bioc_version username has_password auth_domains auth_domain
    #> * <chr> <chr>            <chr> <chr>     <chr>        <chr>    <lgl>        <I<list>>    <chr>
    #> 1 CRAN  http://username~ cran  *         <NA>         username TRUE         <chr [4]>    http://127~
    #> # i 2 more variables: auth_source <chr>, auth_error <chr>

The output of
[`repo_auth()`](https://pak.r-lib.org/dev/reference/repo_auth.md) has
the following extra columns:

- `auth_domains`: these are the URLs that pak tries to use as the
  `service` when looking for credentials in the keyring. For our proxy,
  it tries these URLs:

      repo_auth()$auth_domains
      #> v Found credentials for repo <http://username@127.0.0.1:59571/> (keyring:env).
      #> [[1]]
      #> [1] "http://username@127.0.0.1:59571/" "http://127.0.0.1:59571/"
      #> [3] "http://username@127.0.0.1:59571"  "http://127.0.0.1:59571"

- `auth_domain`: one of `auth_domains`, the URL for which pak found
  credentials in the keyring. If no credentials were found, then this is
  `NA`.

- `auth_source`: a short string that explains where pak found the
  credentials (or `NA` if not credentials were found). For examples
  `netrc` means the user's `netrc` file, and `keyring:macos` means the
  macOS system credential store.

- `auth_error`: `NA` for successful credential search, otherwise a short
  error message on why the search failed. Typically it would fail is the
  credentials are not in the credential store.

### Usage

Once you set up your authenticated repositories, and stored the required
passwords in the system credential store, you can use them like any
other repository. Operations that need authentication will always
include reassuring messages for successful authentications, and warning
messages for failed ones. Function calls that do not perform any HTTP
requests, e.g. because they list cached data, do not display such
messages.

    meta_update()
    #> Checking for 15 new metadata files
    #> ! Cannot find credentials for URL <http://username@127.0.0.1:59571//bin/macosx/big-sur-arm64/contrib/4.4/PACKAGES.gz>, credential lookup
    #> failed. Keyring backend: "macos".
    #> Checking for 15 new metadata files
    x Did not find credentials for repo <http://username@127.0.0.1:59571/>, keyring
    #> lookup failed (macos backend).
    #>
    #> i R 4.4 aarch64-apple-darwin20 packages are missing from CRAN: Failed to connect to 127.0.0.1 port 59571 after 0 ms: Couldn't connect to server and Failed to connect to 127.0.0.1 port 59571 after 0 ms: Couldn't connect to server
    #> i source packages are missing from CRAN: Failed to connect to 127.0.0.1 port 59571 after 0 ms: Couldn't connect to server and Failed to connect to 127.0.0.1 port 59571 after 0 ms: Couldn't connect to server
    #> i Updating metadata database
    #> v Updating metadata database ... done

    meta_list()
    #> # A data frame: 50,701 x 33
    #>    package    version depends suggests license md5sum sha256sum needscompilation
    #>    <chr>      <chr>   <chr>   <chr>    <chr>   <chr>  <chr>     <chr>
    #>  1 A3         1.0.0   R (>= ~ randomF~ GPL (>~ 929a4~ "\n     ~ no
    #>  2 AATtools   0.0.3   R (>= ~ <NA>     GPL-3   de2ec~ "\n     ~ no
    #>  3 ABACUS     1.0.0   R (>= ~ rmarkdo~ GPL-3   28795~ "\n     ~ no
    #>  4 ABC.RAP    0.9.0   R (>= ~ knitr, ~ GPL-3   0158e~ "\n     ~ no
    #>  5 ABCanalys~ 1.2.1   R (>= ~ <NA>     GPL-3   4cbe1~ "\n     ~ no
    #>  6 ABCoptim   0.15.0  <NA>    testtha~ MIT + ~ a294d~ "\n     ~ yes
    #>  7 ABCp2      1.2     MASS    <NA>     GPL-2   d049b~  <NA>     no
    #>  8 ABHgenoty~ 1.0.1   <NA>    knitr, ~ GPL-3   fce25~ "\n     ~ no
    #>  9 ABM        0.4.3   <NA>    <NA>     GPL (>~ 7aaae~ "\n     ~ yes
    #> 10 ABPS       0.3     <NA>    testthat GPL (>~ d3f00~ "\n     ~ no
    #> # i 50,691 more rows
    #> # i 25 more variables: imports <chr>, linkingto <chr>, archs <chr>,
    #> #   enhances <chr>, license_restricts_use <chr>, priority <chr>, os_type <chr>,
    #> #   license_is_foss <chr>, repodir <chr>, rversion <chr>, platform <chr>,
    #> #   ref <chr>, type <chr>, direct <lgl>, status <chr>, target <chr>,
    #> #   mirror <chr>, sources <list>, filesize <int>, sha256 <chr>, sysreqs <chr>,
    #> #   built <chr>, published <dttm>, deps <list>, path <chr>

E.g. here
[`meta_update()`](https://pak.r-lib.org/dev/reference/metadata.md)
outputs an authentication message, but
[`meta_list()`](https://pak.r-lib.org/dev/reference/metadata.md) does
not.

## See also

Other authenticated repositories:
[`repo_auth()`](https://pak.r-lib.org/dev/reference/repo_auth.md),
[`repo_auth_key_get()`](https://pak.r-lib.org/dev/reference/repo_auth_key_get.md)
