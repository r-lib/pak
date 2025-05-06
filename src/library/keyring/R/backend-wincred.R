## The windows credential store does not support multiple keyrings,
## so we emulate them. See the inst/development-notes.md file for a
## complete description on how this is done.

b_wincred_protocol_version <- "1.0.0"

## This is a low level API

b_wincred_i_get <- function(target) {
  .Call(keyring_wincred_get, target)
}

b_wincred_i_set <- function(
  target,
  password,
  username = NULL,
  session = FALSE
) {
  username <- username %||% getOption("keyring_username")
  .Call(keyring_wincred_set, target, password, username, session)
}

b_wincred_i_delete <- function(target) {
  .Call(keyring_wincred_delete, target)
}

b_wincred_i_exists <- function(target) {
  .Call(keyring_wincred_exists, target)
}

b_wincred_i_enumerate <- function(filter) {
  .Call(keyring_wincred_enumerate, filter)
}

b_wincred_i_escape <- function(x) {
  if (is.null(x)) x else URLencode(x)
}

#' @importFrom utils URLdecode

b_wincred_i_unescape <- function(x) {
  URLdecode(x)
}

b_wincred_target <- function(keyring, service, username) {
  username <- username %||% getOption("keyring_username")
  keyring <- if (is.null(keyring)) "" else b_wincred_i_escape(keyring)
  service <- b_wincred_i_escape(service)
  username <- if (is.null(username)) "" else b_wincred_i_escape(username)
  paste0(keyring, ":", service, ":", username)
}

## For the username we need a workaround, because
## strsplit("foo::")[[1]] gives c("foo", ""), i.e. the third empty element
## is cut off.

b_wincred_i_parse_target <- function(target) {
  parts <- lapply(strsplit(target, ":"), lapply, b_wincred_i_unescape)
  extract <- function(x, i) x[i][[1]] %||% ""
  res <- data.frame(
    stringsAsFactors = FALSE,
    keyring = vapply(parts, extract, "", 1),
    service = vapply(parts, extract, "", 2),
    username = vapply(parts, extract, "", 3)
  )
}

b_wincred_target_keyring <- function(keyring) {
  b_wincred_target(keyring, "", "")
}

b_wincred_target_lock <- function(keyring) {
  b_wincred_target(keyring, "", "unlocked")
}

b_wincred_parse_keyring_credential <- function(target) {
  value <- rawToChar(b_wincred_i_get(target))
  con <- textConnection(value)
  on.exit(close(con), add = TRUE)
  as.list(read.dcf(con)[1, ])
}

b_wincred_write_keyring_credential <- function(target, data) {
  con <- textConnection(NULL, open = "w")
  mat <- matrix(unlist(data), nrow = 1)
  colnames(mat) <- names(data)
  write.dcf(mat, con)
  value <- paste0(paste(textConnectionValue(con), collapse = "\n"), "\n")
  close(con)
  b_wincred_i_set(target, password = charToRaw(value))
}

#' @importFrom utils head tail

b_wincred_get_encrypted_aes <- function(str) {
  r <- base64_decode(str)
  structure(tail(r, -16), iv = head(r, 16))
}

## 1. Try to get the unlock credential
## 2. If it exists, return AES key
## 3. If not, then get the credential of the keyring
## 4. Ask for the keyring password
## 5. Hash the password to get the AES key
## 6. Verify that the AES key is correct, using the Verify field of
##    the keyring credential
## 7. Create a SESSION credential, with the decrypted AES key
## 8. Return the decrypted AES key

b_wincred_unlock_keyring_internal <- function(keyring, password = NULL) {
  target_lock <- b_wincred_target_lock(keyring)
  if (b_wincred_i_exists(target_lock)) {
    base64_decode(rawToChar(b_wincred_i_get(target_lock)))
  } else {
    target_keyring <- b_wincred_target_keyring(keyring)
    keyring_data <- b_wincred_parse_keyring_credential(target_keyring)
    if (is.null(password)) {
      message(
        "keyring ",
        sQuote(keyring),
        " is locked, enter password to unlock"
      )
      password <- get_pass()
      if (is.null(password)) stop("Aborted unlocking keyring")
    }
    aes <- sha256(charToRaw(password), key = keyring_data$Salt)
    verify <- b_wincred_get_encrypted_aes(keyring_data$Verify)
    tryCatch(
      aes_cbc_decrypt(verify, key = aes),
      error = function(e) {
        stop("Invalid password, cannot unlock keyring")
      }
    )
    b_wincred_i_set(
      target_lock,
      charToRaw(base64_encode(aes)),
      session = TRUE
    )
    aes
  }
}

b_wincred_is_locked_keyring_internal <- function(keyring) {
  target_lock <- b_wincred_target_lock(keyring)
  !b_wincred_i_exists(target_lock)
}

## -----------------------------------------------------------------------

#' Windows Credential Store keyring backend
#'
#' This backend is the default on Windows. It uses the native Windows
#' Credential API, and needs at least Windows XP to run.
#'
#' This backend supports multiple keyrings. Note that multiple keyrings
#' are implemented in the `keyring` R package, using some dummy keyring
#' keys that represent keyrings and their locked/unlocked state.
#'
#' See [backend] for the documentation of the individual methods.
#'
#' @family keyring backends
#' @export
#' @examples
#' \dontrun{
#' ## This only works on Windows
#' kb <- backend_wincred$new()
#' kb$keyring_create("foobar")
#' kb$set_default_keyring("foobar")
#' kb$set_with_value("service", password = "secret")
#' kb$get("service")
#' kb$delete("service")
#' kb$delete_keyring("foobar")
#' }

backend_wincred <- R6Class(
  "backend_wincred",
  inherit = backend_keyrings,
  public = list(
    name = "windows credential store",
    initialize = function(keyring = NULL)
      b_wincred_init(self, private, keyring),

    get = function(service, username = NULL, keyring = NULL)
      b_wincred_get(self, private, service, username, keyring),
    get_raw = function(service, username = NULL, keyring = NULL)
      b_wincred_get_raw(self, private, service, username, keyring),
    set = function(
      service,
      username = NULL,
      keyring = NULL,
      prompt = "Password: "
    ) b_wincred_set(self, private, service, username, keyring, prompt),
    set_with_value = function(
      service,
      username = NULL,
      password = NULL,
      keyring = NULL
    )
      b_wincred_set_with_value(
        self,
        private,
        service,
        username,
        password,
        keyring
      ),
    set_with_raw_value = function(
      service,
      username = NULL,
      password = NULL,
      keyring = NULL
    )
      b_wincred_set_with_value(
        self,
        private,
        service,
        username,
        password,
        keyring
      ),
    delete = function(service, username = NULL, keyring = NULL)
      b_wincred_delete(self, private, service, username, keyring),
    list = function(service = NULL, keyring = NULL)
      b_wincred_list(self, private, service, keyring),

    keyring_create = function(keyring, password = NULL)
      b_wincred_keyring_create(self, private, keyring, password),
    keyring_list = function() b_wincred_keyring_list(self, private),
    keyring_delete = function(keyring = NULL)
      b_wincred_keyring_delete(self, private, keyring),
    keyring_lock = function(keyring = NULL)
      b_wincred_keyring_lock(self, private, keyring),
    keyring_unlock = function(keyring = NULL, password = NULL)
      b_wincred_keyring_unlock(self, private, keyring, password),
    keyring_is_locked = function(keyring = NULL)
      b_wincred_keyring_is_locked(self, private, keyring),
    keyring_default = function() b_wincred_keyring_default(self, private),
    keyring_set_default = function(keyring = NULL)
      b_wincred_keyring_set_default(self, private, keyring),

    docs = function() {
      modifyList(
        super$docs(),
        list(
          . = "Store secrets in the Windows Credential Store."
        )
      )
    }
  ),

  private = list(
    keyring = NULL,
    keyring_create_direct = function(keyring, password)
      b_wincred_keyring_create_direct(self, private, keyring, password)
  )
)

b_wincred_init <- function(self, private, keyring) {
  private$keyring <- keyring
  invisible(self)
}

#' Get a key from a Wincred keyring
#'
#' @param service Service name. Must not be empty.
#' @param username Username. Might be empty.
#'
#' 1. We check if the key is on the default keyring.
#' 2. If yes, we just return it.
#' 3. Otherwise check if the keyring is locked.
#' 4. If locked, then unlock it.
#' 5. Get the AES key from the keyring.
#' 6. Decrypt the key with the AES key.
#'
#' Additionally, users may specify an encoding to use when converting the
#' password from a byte-string, for compatibility with other software such as
#' python's keyring package. This is done via an option, or an environment variable.
#'
#' @keywords internal

b_wincred_get <- function(self, private, service, username, keyring) {
  password <- self$get_raw(service, username, keyring)
  encoding <- get_encoding_opt()
  b_wincred_decode(password, encoding = encoding)
}

b_wincred_get_raw <- function(self, private, service, username, keyring) {
  keyring <- keyring %||% private$keyring
  target <- b_wincred_target(keyring, service, username)
  password <- b_wincred_i_get(target)
  if (!is.null(keyring)) {
    ## If it is encrypted, we need to decrypt it
    aes <- b_wincred_unlock_keyring_internal(keyring)
    enc <- b_wincred_get_encrypted_aes(rawToChar(password))
    password <- aes_cbc_decrypt(enc, key = aes)
  }
  password
}

#' Decode a raw password obtained by b_wincred_get_raw
#'
#' Defaults to 'auto' encoding, which uses `b_wincred_decode_auto` to
#' accomplish the decoding (this works with decoding either UTF-8 or
#' UTF-16LE encodings). In the case where an encoding is specified,
#' use that to convert the raw password.
#'
#' @param password A raw byte string returned from `b_wincred_get_raw`.
#' @param encoding A character value, specifying an encoding to use.
#'   Defaults to 'auto', which will decode either of UTF-8 or UTF-16LE.
#' @return A character value containing a password.
#'
#' @keywords internal

b_wincred_decode <- function(password, encoding = 'auto') {
  if (encoding == 'auto') {
    b_wincred_decode_auto(password)
  } else {
    password <- iconv(list(password), from = encoding, to = "")
    password
  }
}

#' Decode a raw password obtained by b_wincred_get_raw
#' (UTF-8 and UTF-16LE only)
#'
#' It attempts to use UTF-16LE conversion if there are 0 values in
#' the password.
#'
#' @param password Raw vector coming from the keyring.
#'
#' @keywords internal

b_wincred_decode_auto <- function(password) {
  if (any(password == 0)) {
    password <- iconv(list(password), from = "UTF-16LE", to = "")
    if (is.na(password)) {
      stop("Key contains embedded null bytes, use get_raw()")
    }
    password
  } else {
    rawToChar(password)
  }
}

b_wincred_set <- function(self, private, service, username, keyring, prompt) {
  password <- get_pass(prompt)
  if (is.null(password)) stop("Aborted setting keyring key")
  b_wincred_set_with_value(self, private, service, username, password, keyring)
  invisible(self)
}

b_wincred_set_with_value <- function(
  self,
  private,
  service,
  username,
  password,
  keyring
) {
  encoding <- get_encoding_opt()
  if (encoding != 'auto') {
    password <- enc2utf8(password)
    password <- iconv(
      x = password,
      from = 'UTF-8',
      to = encoding,
      toRaw = TRUE
    )[[1]]
  } else {
    password <- charToRaw(password)
  }
  b_wincred_set_with_raw_value(
    self,
    private,
    service,
    username,
    password,
    keyring
  )
}

#' Set a key on a Wincred keyring
#'
#' @param service Service name. Must not be empty.
#' @param username Username. Might be empty.
#' @param password The key text to store.
#'
#' 1. Check if we are using the default keyring.
#' 2. If yes, then just store the key and we are done.
#' 3. Otherwise check if keyring exists.
#' 4. If not, error and finish.
#' 5. If yes, check if it is locked.
#' 6. If yes, unlock it.
#' 7. Encrypt the key with the AES key, and store it.
#'
#' If required, an encoding can be specified using either an R option
#' (\code{keyring.encoding_windows}) or environment variable
#' (\code{KEYRING_ENCODING_WINDOWS}). To set, use one of:
#'
#' \code{options(keyring.encoding_windows = 'encoding-type')}
#' \code{Sys.setenv("KEYRING_ENCODING_WINDOWS" = 'encoding-type')}
#'
#' For a list of valid encodings, use \code{iconvlist()}
#'
#' @keywords internal

b_wincred_set_with_raw_value <- function(
  self,
  private,
  service,
  username,
  password,
  keyring
) {
  keyring <- keyring %||% private$keyring
  target <- b_wincred_target(keyring, service, username)
  if (is.null(keyring)) {
    b_wincred_i_set(target, password, username = username)
    return(invisible(self))
  }
  ## Not the default keyring, we need to encrypt it
  target_keyring <- b_wincred_target_keyring(keyring)
  aes <- b_wincred_unlock_keyring_internal(keyring)
  enc <- aes_cbc_encrypt(password, key = aes)
  password <- charToRaw(base64_encode(c(attr(enc, "iv"), enc)))
  b_wincred_i_set(target, password = password, username = username)
  invisible(self)
}

b_wincred_delete <- function(self, private, service, username, keyring) {
  keyring <- keyring %||% private$keyring
  target <- b_wincred_target(keyring, service, username)
  b_wincred_i_delete(target)
  invisible(self)
}

b_wincred_list <- function(self, private, service, keyring) {
  keyring <- keyring %||% private$keyring

  filter <- if (is.null(service)) {
    paste0(b_wincred_i_escape(keyring), ":*")
  } else {
    paste0(b_wincred_i_escape(keyring), ":", b_wincred_i_escape(service), ":*")
  }

  list <- b_wincred_i_enumerate(filter)

  ## Filter out the credentials that belong to the keyring or its lock
  list <- grep("(::|::unlocked)$", list, value = TRUE, invert = TRUE)

  parts <- b_wincred_i_parse_target(list)
  data.frame(
    service = parts$service,
    username = parts$username,
    stringsAsFactors = FALSE
  )
}

b_wincred_keyring_create <- function(self, private, keyring, password) {
  password <- password %||% get_pass()
  if (is.null(password)) stop("Aborted creating keyring")
  private$keyring_create_direct(keyring, password)
  invisible(self)
}

## 1. Check that the keyring does not exist, error if it does
## 2. Create salt.
## 3. SHA256 hash the password, with the salt, to get the AES key.
## 4. Generate 15 random bytes, encrypt it with the AES key, base64 encode it.
## 5. Write metadata to the keyring credential
## 6. Unlock the keyring immediately, create a keyring unlock credential

b_wincred_keyring_create_direct <- function(self, private, keyring, password) {
  target_keyring <- b_wincred_target_keyring(keyring)
  if (b_wincred_i_exists(target_keyring)) {
    stop("keyring ", sQuote(keyring), " already exists")
  }
  salt <- base64_encode(rand_bytes(32))
  aes <- sha256(charToRaw(password), key = salt)
  verify <- aes_cbc_encrypt(rand_bytes(15), key = aes)
  verify <- base64_encode(c(attr(verify, "iv"), verify))
  dcf <- list(
    Version = b_wincred_protocol_version,
    Verify = verify,
    Salt = salt
  )
  b_wincred_write_keyring_credential(target_keyring, dcf)
  b_wincred_unlock_keyring_internal(keyring, password)
  invisible(self)
}

b_wincred_keyring_list <- function(self, private) {
  list <- b_wincred_i_enumerate("*")
  parts <- b_wincred_i_parse_target(list)

  ## if keyring:: does not exist, then keyring is not a real keyring, assign it
  ## to the default
  default <- !paste0(parts$keyring, "::") %in% list
  if (length(list) > 0 && any(default)) {
    parts$username[default] <-
      paste0(parts$service[default], ":", parts$username[default])
    parts$service[default] <- parts$keyring[default]
    parts$keyring[default] <- ""
  }

  res <- data.frame(
    stringsAsFactors = FALSE,
    keyring = unname(unique(parts$keyring)),
    num_secrets = as.integer(unlist(tapply(
      parts$keyring,
      factor(parts$keyring, levels = unique(parts$keyring)),
      length,
      simplify = FALSE
    ))),
    locked = vapply(
      unique(parts$keyring),
      FUN.VALUE = TRUE,
      USE.NAMES = FALSE,
      function(x) {
        !any(parts$username[parts$keyring == x] == "unlocked")
      }
    )
  )

  ## Subtract keyring::unlocked and also keyring:: for the non-default keyring
  res$num_secrets <- res$num_secrets - (!res$locked) - (res$keyring != "")

  ## The default keyring cannot be locked
  if ("" %in% res$keyring) res$locked[res$keyring == ""] <- FALSE

  res
}

b_wincred_keyring_delete <- function(self, private, keyring) {
  self$confirm_delete_keyring(keyring)
  keyring <- keyring %||% private$keyring
  items <- self$list(keyring = keyring)

  ## Remove the keyring credential and the lock credential first
  target_keyring <- b_wincred_target_keyring(keyring)
  b_wincred_i_delete(target_keyring)
  target_lock <- b_wincred_target_lock(keyring)
  try(b_wincred_i_delete(target_lock), silent = TRUE)

  ## Then the items themselves
  for (i in seq_len(nrow(items))) {
    target <- b_wincred_target(keyring, items$service[i], items$username[i])
    try(b_wincred_i_delete(target), silent = TRUE)
  }

  invisible()
}

b_wincred_keyring_lock <- function(self, private, keyring) {
  keyring <- keyring %||% private$keyring
  if (is.null(keyring)) {
    warning("Cannot lock the default windows credential store keyring")
  } else {
    target_lock <- b_wincred_target_lock(keyring)
    try(b_wincred_i_delete(target_lock), silent = TRUE)
    invisible()
  }
}

b_wincred_keyring_unlock <- function(self, private, keyring, password = NULL) {
  keyring <- keyring %||% private$keyring
  if (is.null(password)) password <- get_pass()
  if (is.null(password)) stop("Aborted unlocking keyring")
  if (!is.null(keyring)) {
    b_wincred_unlock_keyring_internal(keyring, password)
  }
  invisible()
}

b_wincred_keyring_is_locked <- function(self, private, keyring) {
  keyring <- keyring %||% private$keyring
  if (is.null(keyring)) {
    FALSE
  } else {
    b_wincred_is_locked_keyring_internal(keyring)
  }
}

b_wincred_keyring_default <- function(self, private) {
  private$keyring
}

b_wincred_keyring_set_default <- function(self, private, keyring) {
  private$keyring <- keyring
  invisible(self)
}
