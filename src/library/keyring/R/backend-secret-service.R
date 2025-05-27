#' Linux Secret Service keyring backend
#'
#' This backend is the default on Linux. It uses the libsecret library,
#' and needs a secret service daemon running (e.g. Gnome Keyring, or
#' KWallet). It uses DBUS to communicate with the secret service daemon.
#'
#' This backend supports multiple keyrings.
#'
#' See [backend] for the documentation of the individual methods.
#' The `is_available()` method checks is a Secret Service daemon is
#' running on the system, by trying to connect to it. It returns a logical
#' scalar, or throws an error, depending on its argument:
#' ```
#' is_available = function(report_error = FALSE)
#' ```
#'
#' Argument:
#' * `report_error` Whether to throw an error if the Secret Service is
#'   not available.
#'
#' @family keyring backends
#' @export
#' @examples
#' \dontrun{
#' ## This only works on Linux, typically desktop Linux
#' kb <- backend_secret_service$new()
#' kb$keyring_create("foobar")
#' kb$set_default_keyring("foobar")
#' kb$set_with_value("service", password = "secret")
#' kb$get("service")
#' kb$delete("service")
#' kb$delete_keyring("foobar")
#' }

backend_secret_service <- R6Class(
  "backend_secret_service",
  inherit = backend_keyrings,
  public = list(
    name = "secret service",
    initialize = function(keyring = NULL) b_ss_init(self, private, keyring),

    get = function(service, username = NULL, keyring = NULL)
      b_ss_get(self, private, service, username, keyring),
    get_raw = function(service, username = NULL, keyring = NULL)
      b_ss_get_raw(self, private, service, username, keyring),
    set = function(
      service,
      username = NULL,
      keyring = NULL,
      prompt = "Password: "
    ) b_ss_set(self, private, service, username, keyring, prompt),
    set_with_value = function(
      service,
      username = NULL,
      password = NULL,
      keyring = NULL
    ) b_ss_set_with_value(self, private, service, username, password, keyring),
    set_with_raw_value = function(
      service,
      username = NULL,
      password = NULL,
      keyring = NULL
    )
      b_ss_set_with_raw_value(
        self,
        private,
        service,
        username,
        password,
        keyring
      ),
    delete = function(service, username = NULL, keyring = NULL)
      b_ss_delete(self, private, service, username, keyring),
    list = function(service = NULL, keyring = NULL)
      b_ss_list(self, private, service, keyring),

    keyring_create = function(keyring, password = NULL)
      b_ss_keyring_create(self, private, keyring, password),
    keyring_list = function() b_ss_keyring_list(self, private),
    keyring_delete = function(keyring = NULL)
      b_ss_keyring_delete(self, private, keyring),
    keyring_lock = function(keyring = NULL)
      b_ss_keyring_lock(self, private, keyring),
    keyring_unlock = function(keyring = NULL, password = NULL)
      b_ss_keyring_unlock(self, private, keyring, password),
    keyring_is_locked = function(keyring = NULL)
      b_ss_keyring_is_locked(self, private, keyring),
    keyring_default = function() b_ss_keyring_default(self, private),
    keyring_set_default = function(keyring = NULL)
      b_ss_keyring_set_default(self, private, keyring),
    is_available = function(report_error = FALSE)
      b_ss_is_available(self, private, report_error),

    docs = function() {
      modifyList(
        super$docs(),
        list(
          . = "Store secrets using the Secret Service library and daemon.",
          is_available = "check is Secret Service is available"
        )
      )
    }
  ),

  private = list(
    keyring = NULL,
    keyring_create_direct = function(keyring, password = NULL)
      b_ss_keyring_create_direct(self, private, keyring, password)
  )
)

b_ss_init <- function(self, private, keyring) {
  private$keyring <- keyring
  invisible(self)
}

b_ss_get <- function(self, private, service, username, keyring) {
  res <- b_ss_get_raw(self, private, service, username, keyring)
  if (any(res == 0)) {
    stop("Key contains embedded null bytes, use get_raw()")
  }
  rawToChar(res)
}

b_ss_get_raw <- function(self, private, service, username, keyring) {
  username <- username %||% getOption("keyring_username")
  keyring <- keyring %||% private$keyring
  res <- .Call(keyring_secret_service_get, keyring, service, username)
  res
}

b_ss_set <- function(self, private, service, username, keyring, prompt) {
  username <- username %||% getOption("keyring_username")
  password <- get_pass(prompt)
  if (is.null(password)) stop("Aborted setting keyring key")
  b_ss_set_with_value(self, private, service, username, password, keyring)
  invisible(self)
}

b_ss_set_with_value <- function(
  self,
  private,
  service,
  username,
  password,
  keyring
) {
  username <- username %||% getOption("keyring_username")
  keyring <- keyring %||% private$keyring
  .Call(
    keyring_secret_service_set,
    keyring,
    service,
    username,
    charToRaw(password)
  )
  invisible(self)
}

b_ss_set_with_raw_value <- function(
  self,
  private,
  service,
  username,
  password,
  keyring
) {
  username <- username %||% getOption("keyring_username")
  keyring <- keyring %||% private$keyring
  .Call(keyring_secret_service_set, keyring, service, username, password)
  invisible(self)
}

b_ss_delete <- function(self, private, service, username, keyring) {
  username <- username %||% getOption("keyring_username")
  keyring <- keyring %||% private$keyring
  .Call(keyring_secret_service_delete, keyring, service, username)
  invisible(self)
}

b_ss_list <- function(self, private, service, keyring) {
  keyring <- keyring %||% private$keyring
  res <- .Call(keyring_secret_service_list, keyring, service)
  data.frame(
    service = res[[1]],
    username = res[[2]],
    stringsAsFactors = FALSE
  )
}

b_ss_keyring_create <- function(self, private, keyring, password) {
  password <- password %||% get_pass()
  if (is.null(password)) stop("Aborted creating keyring")
  private$keyring_create_direct(keyring, password)
  invisible(self)
}

b_ss_keyring_list <- function(self, private) {
  res <- .Call(keyring_secret_service_list_keyring)
  data.frame(
    keyring = res[[1]],
    num_secrets = res[[2]],
    locked = res[[3]],
    stringsAsFactors = FALSE
  )
}

b_ss_keyring_delete <- function(self, private, keyring) {
  self$confirm_delete_keyring(keyring)
  keyring <- keyring %||% private$keyring
  .Call(keyring_secret_service_delete_keyring, keyring)
  invisible()
}

b_ss_keyring_lock <- function(self, private, keyring) {
  keyring <- keyring %||% private$keyring
  .Call(keyring_secret_service_lock_keyring, keyring)
  invisible()
}

b_ss_keyring_unlock <- function(self, private, keyring, password) {
  keyring <- keyring %||% private$keyring
  if (!is.null(password))
    warning("password ignored, will be read interactively")
  .Call(keyring_secret_service_unlock_keyring, keyring, password)
  invisible()
}

b_ss_keyring_is_locked <- function(self, private, keyring) {
  keyring <- keyring %||% private$keyring
  .Call(keyring_secret_service_is_locked_keyring, keyring)
}

b_ss_keyring_default <- function(self, private) {
  private$keyring
}

b_ss_keyring_set_default <- function(self, private, keyring) {
  private$keyring <- keyring
  invisible(self)
}

b_ss_is_available <- function(self, private, report_error) {
  .Call(keyring_secret_service_is_available, report_error)
}

b_ss_keyring_create_direct <- function(self, private, keyring, password) {
  if (!is.null(password)) {
    warning("Password ignored, will be read interactively")
  }
  keyring <- keyring %||% private$keyring
  .Call(keyring_secret_service_create_keyring, keyring)
  invisible(self)
}
