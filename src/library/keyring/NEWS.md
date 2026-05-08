# keyring 1.4.1

* keyring now compiles on FreeBSD, OpenBSD, NetSBD and DragonFlyBSD.

# keyring 1.4.0

* Now the "file" backend will only be selected as the default backend
  (via `default_backend()`) if the system keyring exists for this backend.
  If you want to use the "file" backend without a system keyring, then
  you'll need to select it explicitly. See `?default_backend`.

* keyring now does not depend on the assertthat, openssl, rappdirs and
  sodium packages.

* New `key_list_raw()` method to return keys as raw vectors (#159).

# keyring 1.3.2

* keyring uses safer `*printf()` format strings (Secret Service backend).

# keyring 1.3.1

* No user visible changes.

# keyring 1.3.0

* `keyring_create()` and also all backends that support multiple keyrings
  now allow passing the password when creating a new keyring (#114).

* `key_set()` can now use a custom prompt (@pnacht, #112).

* keyring now handled better the 'Cancel' button when requesting a password
  in RStudio, and an error is thrown in this case (#106).

# keyring 1.2.0

* It is now possible to specify the encoding of secrets on Windows
  (#88, @awong234).

* The `get_raw()` method of the Secret Service backend works now (#87).

* Now the file backend is selected by default on Unix systems if
  Secret Service is not available or does not work (#95, @nwstephens).

* The file backend now works with keys that do not have a username.

* All backends use the value of the `keyring_username` option, if set,
  as the default username (#60).

# keyring 1.1.0

* File based backend (#53, @nbenn).

* Fix bugs in `key_set()` on Linux (#43, #51).

* Windows: support non-ascii characters and spaces in `key_list()`
  `service` and `keyring` (#48, #49, @javierluraschi).

* Add support for listing service keys for env backend
  (#58, @javierluraschi).

* keyring is now compatible with R 3.1.x and R 3.2.x.

* libsecret is now optional on Linux. If not available, keyring is built
  without the Secret Service backend (#55).

* Fix the `get_raw()` method on Windows.

* Windows: `get()` tries the UTF-16LE encoding if the sting has embedded
  zero bytes. This allows getting secrets that were
  set in Credential Manager (#56).

* Windows: fix `list()` when some secrets have no `:` at all
  (these were probably set externally) (#44).

# keyring 1.0.0

First public release.
