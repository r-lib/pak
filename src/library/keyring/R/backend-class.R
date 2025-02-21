
abstract_method <- function() {
  stop("An abstract keyring method is called. This is an internal error. ",
       "It most likely happends because of a broken keyring backend that ",
       "does not implement all keyring functions.")
}

#' Abstract class of a minimal keyring backend
#'
#' To implement a new keyring backend, you need to inherit from this
#' class and then redefine the `get`, `set`, `set_with_value` and `delete`
#' methods. Implementing the `list` method is optional. Additional methods
#' can be defined as well.
#'
#' These are the semantics of the various methods:
#'
#' ```
#' get(service, username = NULL, keyring = NULL)
#' get_raw(service, username = NULL, keyring = NULL)
#' set(service, username = NULL, keyring = NULL, prompt = "Password: ")
#' set_with_value(service, username = NULL, password = NULL,
#'                keyring = NULL)
#' set_with_raw_value(service, username = NULL, password = NULL,
#'                keyring = NULL)
#' delete(service, username = NULL, keyring = NULL)
#' list(service = NULL, keyring = NULL)
#' list_raw(service = NULL, keyring = NULL)
#' ```
#'
#' What these functions do:
#'
#' * `get()` queries the secret in a keyring item.
#' * `get_raw()` is similar to `get()`, but returns the result as a raw
#'   vector.
#' * `set()` sets the secret in a keyring item. The secret itself is read
#'   in interactively from the keyboard.
#' * `set_with_value()` sets the secret in a keyring item to the specified
#'   value.
#' * `set_with_raw_value()` sets the secret in keyring item to the
#'   byte sequence of a raw vector.
#' * `delete()` remotes a keyring item.
#' * `list()` lists keyring items.
#' * `list_raw()` lists keyring items, also as raw vectors.
#'
#' The arguments:
#' * `service` String, the name of a service. This is used to find the
#'   secret later.
#' * `username` String, the username associated with a secret. It can be
#'   `NULL`, if no username belongs to the secret. It uses the value of
#'   the `keyring_username`, if set.
#' * `keyring` String, the name of the keyring to work with. This only makes
#'   sense if the platform supports multiple keyrings. `NULL` selects the
#'   default (and maybe only) keyring.
#' * `password` The value of the secret, typically a password, or other
#'   credential.
#' * `prompt` String, the text to be displayed above the textbox.
#'
#' @family keyring backend base classes
#' @importFrom R6 R6Class
#' @name backend
NULL

#' @export

backend <- R6Class(
  "backend",
  public = list(
    name = "abstract backend",

    has_keyring_support = function() FALSE,

    get = function(service, username = NULL, keyring = NULL)
      abstract_method(),
    get_raw = function(service, username = NULL, keyring = NULL)
      charToRaw(self$get(service, username, keyring)),
    set = function(service, username = NULL, keyring = NULL,
                   prompt = "Password: ")
      abstract_method(),
    set_with_value = function(service, username = NULL, password = NULL,
      keyring = NULL) abstract_method(),
    set_with_raw_value = function(service, username = NULL, password = NULL,
      keyring = NULL) self$set_with_value(service, username,
        rawToChar(password), keyring),
    delete = function(service, username = NULL, keyring = NULL)
      abstract_method(),
    list = function(service = NULL, keyring = NULL)
      stop("Backend does not implement 'list'"),
    list_raw = function(service = NULL, keyring = NULL) {
      keys <- self$list(service, keyring)
      keys$service_raw <- lapply(keys$service, charToRaw)
      keys$username_raw <- lapply(keys$username, charToRaw)
      keys
    },

    print = function(...) {
      d <- self$docs()
      cat0("<keyring backend: ", sQuote(self$name), ">\n")
      cat0(d[[1]], "\n\n")
      cat0(paste0(" $", format(names(d[-1])), "  ", d[-1]), sep = "\n")
      invisible(self)
    },

    ## This should be 'protected', really, but not possible in R6
    confirm_delete_keyring = function(keyring) {
      if (is.null(keyring)) {
        stop("Cannot delete the default keyring. ",
             "You need to specify the name of the keyring explicitly.")
      }
      list <- self$keyring_list()
      if (keyring %in% list$keyring &&
          list$num_secrets[match(keyring, list$keyring)] > 0) {
        confirmation(
          "The keyring is not empty, type 'yes' to delete it",
          "yes"
        )
      }
    },

    docs = function() {
      list(
        . = "Inherit from this class to implement a basic backend.",
        get = "query a key from the keyring",
        set = "set a key in the keyring (interactive)",
        set_with_value = "set a key in the keyring",
        delete = "delete a key",
        list = "list keys in a keyring",
        list_raw = "list keys in a keyring as raw vectors",
        has_keyring_support = "TRUE if multiple keyrings are supported"
      )
    }
  )
)

#' Abstract class of a backend that supports multiple keyrings
#'
#' To implement a new keyring that supports multiple keyrings, you need to
#' inherit from this class and redefine the `get`, `set`, `set_with_value`,
#' `delete`, `list` methods, and also the keyring management methods:
#' `keyring_create`, `keyring_list`, `keyring_delete`, `keyring_lock`,
#' `keyring_unlock`, `keyring_is_locked`, `keyring_default` and
#' `keyring_set_default`.
#'
#' See [backend] for the first set of methods. This is the semantics of the
#' keyring management methods:
#'
#' ```
#' keyring_create(keyring)
#' keyring_list()
#' keyring_delete(keyring = NULL)
#' keyring_lock(keyring = NULL)
#' keyring_unlock(keyring = NULL, password = NULL)
#' keyring_is_locked(keyring = NULL)
#' keyring_default()
#' keyring_set_default(keyring = NULL)
#' ```
#'
#' * `keyring_create()` creates a new keyring.
#' * `keyring_list()` lists all keyrings.
#' * `keyring_delete()` deletes a keyring. It is a good idea to protect
#'    the default keyring, and/or a non-empty keyring with a password or
#'    a confirmation dialog.
#' * `keyring_lock()` locks a keyring.
#' * `keyring_unlock()` unlocks a keyring.
#' * `keyring_is_locked()` checks whether a keyring is locked.
#' * `keyring_default()` returns the default keyring.
#' * `keyring_set_default()` sets the default keyring.
#'
#' Arguments:
#' * `keyring` is the name of the keyring to use or create. For some
#'   methods in can be `NULL` to select the default keyring.
#' * `password` is the password of the keyring.
#'
#' @family keyring backend base classes
#' @name backend_keyrings
NULL

#' @export

backend_keyrings <- R6Class(
  "backend_keyrings",
  inherit = backend,
  public = list(
    has_keyring_support = function() TRUE,

    get = function(service, username = NULL, keyring = NULL)
      abstract_method(),
    set = function(service, username = NULL, keyring = NULL)
      abstract_method(),
    set_with_value = function(service, username = NULL, password = NULL,
      keyring = NULL)
      abstract_method(),
    delete = function(service, username = NULL)
      abstract_method(),
    list = function(service = NULL, keyring = NULL)
      abstract_method(),

    keyring_create = function(keyring, password) abstract_method(),
    keyring_list = function() abstract_method(),
    keyring_delete = function(keyring = NULL) abstract_method(),
    keyring_lock = function(keyring = NULL) abstract_method(),
    keyring_unlock = function(keyring = NULL, password = NULL)
      abstract_method(),
    keyring_is_locked = function(keyring = NULL) abstract_method(),
    keyring_default = function() abstract_method(),
    keyring_set_default = function(keyring = NULL) abstract_method(),

    docs = function() {
      modifyList(super$docs(), list(
        . = "Inherit from this class for a new backend with multiple keyrings.",
        keyring_create = "create new keyring",
        keyring_list = "list all keyrings",
        keyring_delete = "delete a keyring",
        keyring_lock = "lock a keyring",
        keyring_unlock = "unlock a keyring",
        keyring_is_locked = "check if a keyring is locked",
        keyring_default = "query the default keyring",
        keyring_set_default = "set the default keyring"
      ))
    }
  )
)
