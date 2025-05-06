#' Operations on keys
#'
#' These functions manipulate keys in a keyring. You can think of a keyring
#' as a secure key-value store.
#'
#' `key_get` queries a key from the keyring.
#'
#' `key_get_raw` queries a key and returns it as a raw vector.
#' Most credential stores allow storing a byte sequence with embedded null
#' bytes, and these cannot be represented as traditional null bytes
#' terminated strings. If you don't know whether the key contains an
#' embedded null, it is best to query it with `key_get_raw` instead of
#' `key_get`.
#'
#' `key_set` sets a key in the keyring. The contents of the key is read
#' interactively from the terminal.
#'
#' `key_set_with_value` is the non-interactive pair of `key_set`, to set
#' a key in the keyring.
#'
#' `key_set_raw_with_value` sets a key to a byte sequence from a raw
#' vector.
#'
#' `key_delete` deletes a key.
#'
#' `key_list` lists all keys of a keyring, or the keys for a certain
#' service (if `service` is not `NULL`).
#'
#' `key_list_raw()` is like `key_list()` but also returns the keys as raw
#' values. This is useful if your keys have bytes that cannot appear
#' in R strings, e.g. a zero byte.
#'
#' ## Encodings
#'
#' On Windows, if required, an encoding can be specified using either
#' an R option (`keyring.encoding_windows`) or environment variable
#' (`KEYRING_ENCODING_WINDOWS`). This will be applied when both
#' getting and setting keys. The option takes precedence over the
#' environment variable, if both are set.
#'
#' This is reserved primarily for compatibility with keys set with
#' other software, such as Python's implementation of keyring. For a
#' list of encodings, use [iconvlist()], although it should be noted
#' that not _every_ encoding can be properly converted, even for
#' trivial cases. For best results, use UTF-8 if you can.
#'
#' @param service Service name, a character scalar.
#' @param username Username, a character scalar, or `NULL` if the key
#'   is not associated with a username.
#' @param password The secret to store. For `key_set`, it is read from
#'   the console, interactively. `key_set_with_value` can be also used
#'   in non-interactive mode.
#' @param keyring For systems that support multiple keyrings, specify
#'   the name of the keyring to use here. If `NULL`, then the default
#'   keyring is used. See also [has_keyring_support()].
#' @param prompt The character string displayed when requesting the secret
#'
#' @return `key_get` returns a character scalar, the password or other
#'   confidential information that was stored in the key.
#'
#'   `key_list` returns a list of keys, i.e. service names and usernames,
#'   in a data frame with column names `service` and `username`. If a
#'   service or user name contains a zero byte, which is not allowed in an
#'   R string, that entry is shown as `NA` and a warning (of class
#'   `keyring_warn_zero_byte_keys`) is thrown.  You can use the
#'   `key_list_raw()` function to query these keys.
#'
#'   `key_list_raw` is similar to `key_list` but returns service and
#'   usernames as raw vectors. This is useful if some service or user
#'   names) contain zero bytes. All column names: `service`, `username`,
#'   `service_raw`, `username_raw`.
#'
#' @export
#' @examples
#' # These examples use the default keyring, and they are interactive,
#' # so, we don't run them by default
#' \dontrun{
#' key_set("R-keyring-test-service", "donaldduck")
#' key_get("R-keyring-test-service", "donaldduck")
#' if (has_keyring_support()) key_list(service = "R-keyring-test-service")
#' key_delete("R-keyring-test-service", "donaldduck")
#'
#' ## This is non-interactive, assuming that that default keyring
#' ## is unlocked
#' key_set_with_value("R-keyring-test-service", "donaldduck",
#'                    password = "secret")
#' key_get("R-keyring-test-service", "donaldduck")
#' if (has_keyring_support()) key_list(service = "R-keyring-test-service")
#' key_delete("R-keyring-test-service", "donaldduck")
#'
#' ## This is interactive using backend_file
#' ## Set variables to be used in keyring
#' kr_name <- "my_keyring"
#' kr_service <- "my_database"
#' kr_username <- "my_username"
#'
#' ## Create a keyring and add an entry using the variables above
#' kb <- keyring::backend_file$new()
#' ## Prompt for the keyring password, used to unlock keyring
#' kb$keyring_create(kr_name)
#' ## Prompt for the secret/password to be stored in the keyring
#' kb$set(kr_service, username=kr_username, keyring=kr_name)
#' # Lock the keyring
#' kb$keyring_lock(kr_name)
#'
#' ## The keyring file is stored at ~/.config/r-keyring/ on Linux
#'
#' ## Output the stored password
#' keyring::backend_file$new()$get(service = kr_service,
#'   user = kr_username,
#'   keyring = kr_name)
#' }

key_get <- function(service, username = NULL, keyring = NULL) {
  assert_that(is_non_empty_string(service))
  assert_that(is_string_or_null(username))
  default_backend()$get(service, username, keyring = keyring)
}

#' @export
#' @rdname key_get

key_get_raw <- function(service, username = NULL, keyring = NULL) {
  assert_that(is_non_empty_string(service))
  assert_that(is_string_or_null(username))
  default_backend()$get_raw(service, username, keyring = keyring)
}

#' @export
#' @rdname key_get

key_set <- function(
  service,
  username = NULL,
  keyring = NULL,
  prompt = "Password: "
) {
  assert_that(is_non_empty_string(service))
  assert_that(is_string_or_null(username))
  default_backend()$set(service, username, keyring = keyring, prompt = prompt)
}

#' @export
#' @rdname key_get

key_set_with_value <- function(
  service,
  username = NULL,
  password = NULL,
  keyring = NULL
) {
  assert_that(is_non_empty_string(service))
  assert_that(is_string(password))
  default_backend()$set_with_value(
    service,
    username,
    password,
    keyring = keyring
  )
}

#' @export
#' @rdname key_get

key_set_with_raw_value <- function(
  service,
  username = NULL,
  password = NULL,
  keyring = NULL
) {
  assert_that(is_non_empty_string(service))
  assert_that(is.raw(password))
  default_backend()$set_with_raw_value(
    service,
    username,
    password,
    keyring = keyring
  )
}

#' @export
#' @rdname key_get

key_delete <- function(service, username = NULL, keyring = NULL) {
  assert_that(is_non_empty_string(service))
  assert_that(is_string_or_null(username))
  default_backend()$delete(service, username, keyring = keyring)
}

#' @export
#' @rdname key_get

key_list <- function(service = NULL, keyring = NULL) {
  assert_that(is_non_empty_string_or_null(service))
  default_backend()$list(service, keyring = keyring)
}

#' @export
#' @rdname key_get

key_list_raw <- function(service = NULL, keyring = NULL) {
  assert_that(is_non_empty_string_or_null(service))
  default_backend()$list_raw(service, keyring = keyring)
}

#' Operations on keyrings
#'
#' On most platforms `keyring` supports multiple keyrings. This includes
#' Windows, macOS and Linux (Secret Service) as well. A keyring is a
#' collection of keys that can be treated as a unit. A keyring typically
#' has a name and a password to unlock it. Once a keyring is unlocked,
#' it remains unlocked until the end of the user session, or until it is
#' explicitly locked again.
#'
#' Platforms typically have a default keyring, which is unlocked
#' automatically when the user logs in. This keyring does not need to be
#' unlocked explicitly.
#'
#' You can configure the keyring to use via R options or environment
#' variables (see [default_backend()]), or you can also specify it
#' directly in the [default_backend()] call, or in the individual
#' `keyring` calls.
#'
#' `has_keyring_support` checks if a backend supports multiple keyrings.
#'
#' `keyring_create` creates a new keyring. It asks for a password if no
#' password is specified.
#'
#' `keyring_list` lists all existing keyrings.
#'
#' `keyring_delete` deletes a keyring. Deleting a non-empty keyring
#' requires confirmation, and the default keyring can only be deleted if
#' specified explicitly. On some backends (e.g. Windows Credential Store),
#' the default keyring cannot be deleted at all.
#'
#' `keyring_lock` locks a keyring. On some backends (e.g. Windows
#' Credential Store), the default keyring cannot be locked.
#'
#' `keyring_unlock` unlocks a keyring. If a password is not specified,
#' it will be read in interactively.
#'
#' `keyring_is_locked` queries whether a keyring is locked.
#'
#' @param keyring The name of the keyring to create or to operate on.
#'   For functions other than `keyring_create`, it can also be `NULL` to
#'   select the default keyring.
#' @param password The initial password or the password to unlock the
#'   keyring. If not specified or `NULL`, it will be read from the console.
#'
#' @export
#' @examples
#' default_backend()
#' has_keyring_support()
#' backend_env$new()$has_keyring_support()
#'
#' ## This might ask for a password, so we do not run it by default
#' ## It only works if the default backend supports multiple keyrings
#' \dontrun{
#' keyring_create("foobar")
#' key_set_with_value("R-test-service", "donaldduck", password = "secret",
#'                    keyring = "foobar")
#' key_get("R-test-service", "donaldduck", keyring = "foobar")
#' key_list(keyring = "foobar")
#' keyring_delete(keyring = "foobar")
#' }

has_keyring_support <- function() {
  default_backend()$has_keyring_support()
}

#' @export
#' @rdname has_keyring_support

keyring_create <- function(keyring, password = NULL) {
  assert_that(
    is_string(keyring),
    is_string_or_null(password)
  )
  default_backend()$keyring_create(keyring, password)
}

#' @export
#' @rdname has_keyring_support

keyring_list <- function() {
  default_backend()$keyring_list()
}

#' @export
#' @rdname has_keyring_support

keyring_delete <- function(keyring = NULL) {
  assert_that(is_string_or_null(keyring))
  default_backend()$keyring_delete(keyring)
}

#' @export
#' @rdname has_keyring_support

keyring_lock <- function(keyring = NULL) {
  assert_that(is_string_or_null(keyring))
  default_backend()$keyring_lock(keyring)
}

#' @export
#' @rdname has_keyring_support

keyring_unlock <- function(keyring = NULL, password = NULL) {
  assert_that(is_string_or_null(keyring))
  default_backend()$keyring_unlock(keyring, password)
}

#' @export
#' @rdname has_keyring_support

keyring_is_locked <- function(keyring = NULL) {
  assert_that(is_string_or_null(keyring))
  default_backend()$keyring_is_locked(keyring)
}
