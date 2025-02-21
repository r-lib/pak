
#' About the keyring package
#'
#' Platform independent API to many system credential store
#' implementations. Currently supported:
#' * Keychain on macOS,
#' * Credential Store on Windows,
#' * the Secret Service API on Linux, and
#' * environment variables on other platforms.
#'
#' @section Configuring an OS-specific backend:
#'
#' - The default is operating system specific, and is described in
#'   [default_backend()]. In most cases you don't have to configure this.
#' - MacOS: [backend_macos]
#' - Linux: [backend_secret_service]
#' - Windows: [backend_wincred]
#' - Or store the secrets in environment variables on other operating
#'   systems: [backend_env]
#'
#' @section Query secret keys in a keyring:
#'
#' Each keyring can contain one or many secrets (keys). A key is defined by
#' a service name and a password. Once a key is defined, it persists in the
#' keyring store of the operating system. This means the keys persist beyond
#' the termination of and R session. Specifically, you can define a key
#' once, and then read the key value in completely independent R sessions.
#'
#' - Setting a secret interactively: [key_set()].
#' - Setting a secret from a script, i.e. non-interactively:
#'   [key_set_with_value()].
#' - Reading a secret: [key_get()], [key_get_raw()].
#' - Listing secrets: [key_list()], [key_list_raw()].
#' - Deleting a secret: [key_delete()].
#'
#' @section Managing keyrings:
#'
#' A keyring is a collection of keys that can be treated as a unit.
#' A keyring typically has a name and a password to unlock it.
#'
#' - [keyring_create()]
#' - [keyring_delete()]
#' - [keyring_list()]
#' - [keyring_lock()]
#' - [keyring_unlock()]
#'
#' Note that all platforms have a default keyring, and `key_get()`, etc.
#' will use that automatically. The default keyring is also convenient,
#' because the OS unlocks it automatically when you log in, so secrets
#' are available immediately.
#'
#' You only need to explicitly deal with keyrings and the `keyring_*`
#' functions if you want to use a different keyring.
#'
#' @useDynLib keyring, .registration = TRUE
"_PACKAGE"
