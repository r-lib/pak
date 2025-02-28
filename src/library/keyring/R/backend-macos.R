
#' macOS Keychain keyring backend
#'
#' This backend is the default on macOS. It uses the macOS native Keychain
#' Service API.
#'
#' It supports multiple keyrings.
#'
#' See [backend] for the documentation of the individual methods.
#'
#' @family keyring backends
#' @include backend-class.R
#' @export
#' @examples
#' \dontrun{
#' ## This only works on macOS
#' kb <- backend_macos$new()
#' kb$keyring_create("foobar")
#' kb$set_default_keyring("foobar")
#' kb$set_with_value("service", password = "secret")
#' kb$get("service")
#' kb$delete("service")
#' kb$delete_keyring("foobar")
#' }

backend_macos <- R6Class(
  "backend_macos",
  inherit = backend_keyrings,
  public = list(
    name = "macos",
    initialize = function(keyring = NULL)
      b_macos_init(self, private, keyring),

    get = function(service, username = NULL, keyring = NULL)
      b_macos_get(self, private, service, username, keyring),
    get_raw = function(service, username = NULL, keyring = NULL)
      b_macos_get_raw(self, private, service, username, keyring),
    set = function(service, username = NULL, keyring = NULL,
                   prompt = "Password: ")
      b_macos_set(self, private, service, username, keyring, prompt),
    set_with_value = function(service, username = NULL, password = NULL,
      keyring = NULL)
      b_macos_set_with_value(self, private, service, username, password,
                             keyring),
    set_with_raw_value = function(service, username = NULL, password = NULL,
      keyring = NULL)
      b_macos_set_with_raw_value(self, private, service, username, password,
                                 keyring),
    delete = function(service, username = NULL, keyring = NULL)
      b_macos_delete(self, private, service, username, keyring),
    list = function(service = NULL, keyring = NULL)
      b_macos_list(self, private, service, keyring),
    list_raw = function(service = NULL, keyring = NULL)
      b_macos_list_raw(self, private, service, keyring),

    keyring_create = function(keyring, password = NULL)
      b_macos_keyring_create(self, private, keyring, password),
    keyring_list = function()
      b_macos_keyring_list(self, private),
    keyring_delete = function(keyring = NULL)
      b_macos_keyring_delete(self, private, keyring),
    keyring_lock = function(keyring = NULL)
      b_macos_keyring_lock(self, private, keyring),
    keyring_unlock = function(keyring = NULL, password = NULL)
      b_macos_keyring_unlock(self, private, keyring, password),
    keyring_is_locked = function(keyring = NULL)
      b_macos_keyring_is_locked(self, private, keyring),
    keyring_default = function()
      b_macos_keyring_default(self, private),
    keyring_set_default = function(keyring = NULL)
      b_macos_keyring_set_default(self, private, keyring),

    docs = function() {
      modifyList(super$docs(), list(
        . = "Store secrets in the macOS Keychain."
      ))
    }
  ),

  private = list(
    keyring = NULL,
    keyring_file = function(name)
      b_macos_keyring_file(self, private, name),
    keyring_create_direct = function(keyring, password)
      b_macos_keyring_create_direct(self, private, keyring, password)
  )
)

b_macos_init <- function(self, private, keyring) {
  private$keyring <- keyring
  invisible(self)
}

b_macos_get <- function(self, private, service, username, keyring) {
  username <- username %||% getOption("keyring_username")
  keyring <- private$keyring_file(keyring %||% private$keyring)
  res <- .Call(keyring_macos_get, utf8(keyring), utf8(service),
               utf8(username))
  if (any(res == 0)) {
    stop("Key contains embedded null bytes, use get_raw()")
  }
  rawToChar(res)
}

b_macos_get_raw <- function(self, private, service, username, keyring) {
  username <- username %||% getOption("keyring_username")
  keyring <- private$keyring_file(keyring %||% private$keyring)
  .Call(keyring_macos_get, utf8(keyring), utf8(service), utf8(username))
}

b_macos_set <- function(self, private, service, username, keyring, prompt) {
  username <- username %||% getOption("keyring_username")
  password <- get_pass(prompt)
  if (is.null(password)) stop("Aborted setting keyring key")
  b_macos_set_with_value(self, private, service, username, password, keyring)
  invisible(self)
}

b_macos_set_with_value <- function(self, private, service, username,
                                   password, keyring) {
  username <- username %||% getOption("keyring_username")
  keyring <- private$keyring_file(keyring %||% private$keyring)
  .Call(keyring_macos_set, utf8(keyring), utf8(service),
        utf8(username), charToRaw(password))
  invisible(self)
}

b_macos_set_with_raw_value <- function(self, private, service, username,
                                       password, keyring) {
  username <- username %||% getOption("keyring_username")
  keyring <- private$keyring_file(keyring %||% private$keyring)
  .Call(keyring_macos_set, utf8(keyring), utf8(service),
        utf8(username), password)
  invisible(self)
}

b_macos_delete <- function(self, private, service, username, keyring) {
  username <- username %||% getOption("keyring_username")
  keyring <- private$keyring_file(keyring %||% private$keyring)
  .Call(keyring_macos_delete, utf8(keyring), utf8(service),
        utf8(username))
  invisible(self)
}

b_macos_list <- function(self, private, service, keyring) {
  keyring <- private$keyring_file(keyring %||% private$keyring)
  res <- .Call(keyring_macos_list, utf8(keyring), utf8(service))
  df <- data.frame(
    service = res[[1]],
    username = res[[2]],
    stringsAsFactors = FALSE
  )
  srv_na <- anyNA(df[["service"]])
  usr_na <- anyNA(df[["username"]])
  if (srv_na | usr_na) {
    cnd <- structure(
      list(message = paste0(
        "Some ",
        if (srv_na) "service names ",
        if (srv_na && usr_na) "and some ",
        if (usr_na) "user names ",
        "contain zero bytes. These are shown as NA. ",
        "Use `key_list_raw()` to see them."
      )),
      class = "keyring_warn_zero_byte_keys"
    )
    warning(cnd)
  }
  df
}

b_macos_list_raw <- function(self, private, service, keyring) {
  keyring <- private$keyring_file(keyring %||% private$keyring)
  res <- .Call(keyring_macos_list, utf8(keyring), utf8(service))
  df <- data.frame(
    service = res[[1]],
    username = res[[2]],
    stringsAsFactors = FALSE
  )
  df[["service_raw"]] <- res[[3]]
  df[["username_raw"]] <- res[[4]]
  df
}

b_macos_keyring_create <- function(self, private, keyring, password) {
  password <- password %||% get_pass()
  if (is.null(password)) stop("Aborted creating keyring")
  private$keyring_create_direct(keyring, password)
  invisible(self)
}

b_macos_keyring_list <- function(self, private) {
  res <- .Call(keyring_macos_list_keyring)
  data.frame(
    keyring = sub("\\.keychain(-db)?$", "", basename(res[[1]])),
    num_secrets = res[[2]],
    locked = res[[3]],
    stringsAsFactors = FALSE
  )
}

b_macos_keyring_delete <- function(self, private, keyring) {
  self$confirm_delete_keyring(keyring)
  keyring <- private$keyring_file(keyring %||% private$keyring)
  .Call(keyring_macos_delete_keyring, utf8(keyring))
  invisible(self)
}

b_macos_keyring_lock <- function(self, private, keyring) {
  keyring <- private$keyring_file(keyring %||% private$keyring)
  .Call(keyring_macos_lock_keyring, utf8(keyring))
  invisible(self)
}

b_macos_keyring_unlock <- function(self, private, keyring, password) {
  password <- password %||% get_pass()
  if (is.null(password)) stop("Aborted unlocking keyring")
  keyring <- private$keyring_file(keyring %||% private$keyring)
  .Call(keyring_macos_unlock_keyring, utf8(keyring), password)
  invisible(self)
}

b_macos_keyring_is_locked <- function(self, private, keyring) {
  keyring <- private$keyring_file(keyring %||% private$keyring)
  .Call(keyring_macos_is_locked_keyring, utf8(keyring))
}

b_macos_keyring_default <- function(self, private) {
  private$keyring
}

b_macos_keyring_set_default <- function(self, private, keyring) {
  private$keyring <- keyring
  invisible(self)
}

## --------------------------------------------------------------------
## Private

b_macos_keyring_file <- function(self, private, name) {
  if (is.null(name)) {
    name

  } else if (substr(name, 1, 1) == "/" || substr(name, 1, 2) == "./") {
    normalizePath(name, mustWork = FALSE)

  } else {
    files <- normalizePath(
      paste0("~/Library/Keychains/", name, c(".keychain", ".keychain-db")),
      mustWork = FALSE
    )
    if (file.exists(files[1])) {
      files[1]
    } else if (file.exists(files[2])) {
      files[2]
    } else if (darwin_version() >= "16.0.0") {
      files[2]
    } else {
      files[1]
    }
  }
}

b_macos_keyring_create_direct <- function(self, private, keyring, password) {
  keyring <- private$keyring_file(keyring)
  .Call(keyring_macos_create, utf8(keyring), password)
  invisible(self)
}
