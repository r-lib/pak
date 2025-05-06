b_file_keyrings <- new.env(parent = emptyenv())

#' Encrypted file keyring backend
#'
#' This is a simple keyring backend, that stores/uses secrets in encrypted
#' files.
#'
#' It supports multiple keyrings.
#'
#' See [backend] for the documentation of the individual methods.
#'
#' @family keyring backends
#' @export
#' @include backend-class.R
#' @examples
#' \dontrun{
#' kb <- backend_file$new()
#' }

backend_file <- R6Class(
  "backend_file",
  inherit = backend_keyrings,
  public = list(
    name = "file",
    initialize = function(keyring = NULL) b_file_init(self, private, keyring),

    get = function(service, username = NULL, keyring = NULL)
      b_file_get(self, private, service, username, keyring),
    get_raw = function(service, username = NULL, keyring = NULL)
      b_file_get_raw(self, private, service, username, keyring),
    set = function(service, username = NULL, keyring = NULL, prompt = NULL)
      b_file_set(self, private, service, username, keyring, prompt),
    set_with_value = function(
      service,
      username = NULL,
      password = NULL,
      keyring = NULL
    )
      b_file_set_with_value(
        self,
        private,
        service,
        username,
        password,
        keyring
      ),

    delete = function(service, username = NULL, keyring = NULL)
      b_file_delete(self, private, service, username, keyring),
    list = function(service = NULL, keyring = NULL)
      b_file_list(self, private, service, keyring),

    keyring_create = function(keyring = NULL, password = NULL)
      b_file_keyring_create(self, private, keyring, password),
    keyring_delete = function(keyring = NULL)
      b_file_keyring_delete(self, private, keyring),

    keyring_lock = function(keyring = NULL)
      b_file_keyring_lock(self, private, keyring),
    keyring_unlock = function(keyring = NULL, password = NULL)
      b_file_keyring_unlock(self, private, keyring, password),
    keyring_is_locked = function(keyring = NULL)
      b_file_keyring_is_locked(self, private, keyring),
    keyring_list = function() b_file_keyring_list(self, private),

    keyring_default = function() b_file_keyring_default(self, private),
    keyring_set_default = function(keyring)
      b_file_keyring_set_default(self, private, keyring),

    docs = function() {
      modifyList(
        super$docs(),
        list(
          . = paste0(
            "Store secrets in encrypted files.\n",
            private$keyring
          )
        )
      )
    }
  ),

  private = list(
    keyring = NULL,

    keyring_create_direct = function(
      keyring = NULL,
      password = NULL,
      prompt = NULL
    ) b__file_keyring_create_direct(self, private, keyring, password, prompt),
    keyring_autocreate = function(keyring = NULL)
      b__file_keyring_autocreate(self, private, keyring),
    keyring_file = function(keyring = NULL)
      b__file_keyring_file(self, private, keyring),
    keyring_read_file = function(keyring = NULL)
      b__file_keyring_read_file(self, private, keyring),
    keyring_write_file = function(
      keyring = NULL,
      nonce = NULL,
      items = NULL,
      key = NULL
    ) b__file_keyring_write_file(self, private, keyring, nonce, items, key),

    get_keyring_pass = function(keyring = NULL)
      b__file_get_keyring_pass(self, private, keyring),
    set_keyring_pass = function(key = NULL, keyring = NULL)
      b__file_set_keyring_pass(self, private, key, keyring),
    unset_keyring_pass = function(keyring = NULL)
      b__file_unset_keyring_pass(self, private, keyring),
    is_set_keyring_pass = function(keyring = NULL)
      b__file_is_set_keyring_pass(self, private, keyring),

    update_cache = function(
      keyring = NULL,
      nonce = NULL,
      check = NULL,
      items = NULL
    ) b__file_update_cache(self, private, keyring, nonce, check, items),
    get_cache = function(keyring = NULL)
      b__file_get_cache(self, private, keyring)
  )
)

b_file_init <- function(self, private, keyring) {
  self$keyring_set_default(keyring %||% "system")
  invisible(self)
}

b_file_get <- function(self, private, service, username, keyring) {
  private$keyring_autocreate(keyring)

  username <- username %||% getOption("keyring_username")
  if (self$keyring_is_locked(keyring)) self$keyring_unlock(keyring)

  cached <- private$get_cache(keyring)
  all_items <- cached$items
  all_services <- vapply(all_items, `[[`, character(1L), "service_name")
  item_matches <- all_services %in% service

  if (!is.null(username)) {
    all_users <- vapply(
      all_items,
      function(x) x$user_name %||% NA_character_,
      character(1L)
    )
    item_matches <- item_matches & all_users %in% username
  }

  if (sum(item_matches) < 1L) {
    b_file_error(
      "cannot get secret",
      "The specified item could not be found in the keychain."
    )
  }

  vapply(
    lapply(all_items[item_matches], `[[`, "secret"),
    b_file_secret_decrypt,
    character(1L),
    cached$nonce,
    private$get_keyring_pass(keyring)
  )
}

b_file_set <- function(self, private, service, username, keyring, prompt) {
  username <- username %||% getOption("keyring_username")

  keyring <- keyring %||% private$keyring
  file <- private$keyring_file(keyring)
  ex <- file.exists(file)

  # We use a different prompt in this case, to give a heads up
  prompt <- prompt %||%
    if (!ex && interactive()) {
      paste0(
        "Note: the specified keyring does not exist, you'll have to ",
        "create it in the next step. Key password: "
      )
    } else {
      "Password: "
    }

  password <- get_pass(prompt)
  if (is.null(password)) stop("Aborted setting keyring key")

  private$keyring_autocreate()

  self$set_with_value(service, username, password, keyring)

  invisible(self)
}

b_file_set_with_value <- function(
  self,
  private,
  service,
  username,
  password,
  keyring
) {
  private$keyring_autocreate(keyring)

  username <- username %||% getOption("keyring_username")
  if (self$keyring_is_locked(keyring)) self$keyring_unlock(keyring)

  keyring_file <- private$keyring_file(keyring)
  kr_env <- b_file_keyring_env(keyring_file)

  with_lock(keyring_file, {
    cached <- private$get_cache(keyring)
    all_items <- cached$items

    services <- vapply(all_items, `[[`, character(1L), "service_name")
    users <- vapply(
      all_items,
      function(x) x$user_name %||% NA_character_,
      character(1)
    )
    existing <- if (!is.null(username)) {
      services %in% service & users %in% username
    } else {
      services %in% service & is.na(users)
    }
    if (length(existing)) all_items <- all_items[!existing]

    new_item <- list(
      service_name = service,
      user_name = username,
      secret = b_file_secret_encrypt(
        password,
        cached$nonce,
        private$get_keyring_pass(keyring)
      )
    )

    items <- c(all_items, list(new_item))
    private$keyring_write_file(keyring, items = items)
    kr_env$stamp <- file_stamp(keyring_file)
  })

  kr_env <- b_file_keyring_env(keyring_file)
  kr_env$items <- items

  invisible(self)
}

b_file_delete <- function(self, private, service, username, keyring) {
  username <- username %||% getOption("keyring_username")
  if (self$keyring_is_locked(keyring)) self$keyring_unlock(keyring)

  keyring_file <- private$keyring_file(keyring)
  kr_env <- b_file_keyring_env(keyring_file)

  with_lock(keyring_file, {
    cached <- private$get_cache(keyring)
    all_items <- cached$items

    services <- vapply(all_items, `[[`, character(1L), "service_name")
    users <- vapply(
      all_items,
      function(x) x$user_name %||% NA_character_,
      character(1)
    )
    existing <- if (!is.null(username)) {
      services %in% service & users %in% username
    } else {
      services %in% service & is.na(users)
    }
    if (length(existing) == 0) return(invisible(self))

    ## Remove
    items <- all_items[!existing]

    private$keyring_write_file(keyring, items = items)
    kr_env$stamp <- file_stamp(keyring_file)
  })

  kr_env$items <- items

  invisible(self)
}

b_file_list <- function(self, private, service, keyring) {
  private$keyring_autocreate(keyring)

  cached <- private$get_cache(keyring)
  all_items <- cached$items

  res <- data.frame(
    service = vapply(all_items, `[[`, character(1L), "service_name"),
    username = vapply(
      all_items,
      function(x) x$user_name %||% NA_character_,
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  if (!is.null(service)) {
    res[res[["service"]] == service, ]
  } else {
    res
  }
}

b_file_keyring_create <- function(self, private, keyring, password) {
  private$keyring_create_direct(keyring, password)
}

b_file_keyring_delete <- function(self, private, keyring) {
  self$keyring_lock(keyring)

  kr_file <- private$keyring_file(keyring)

  unlink(kr_file, recursive = TRUE, force = TRUE)

  invisible(self)
}

b_file_keyring_lock <- function(self, private, keyring) {
  keyring <- keyring %||% private$keyring
  file <- private$keyring_file(keyring)
  if (!file.exists(file)) {
    stop("The '", keyring, "' keyring does not exists, create it first!")
  }
  private$unset_keyring_pass(keyring)
  invisible(self)
}

b_file_keyring_unlock <- function(self, private, keyring, password) {
  file <- private$keyring_file(keyring)

  if (!file.exists(file)) {
    stop("Keyring `", keyring, "` does not exist")
  }

  private$set_keyring_pass(password, keyring)

  if (self$keyring_is_locked(keyring)) {
    private$unset_keyring_pass(keyring)
    b_file_error(
      "cannot unlock keyring",
      "The supplied password does not work."
    )
  }

  invisible(self)
}

b_file_keyring_is_locked <- function(self, private, keyring) {
  private$keyring_autocreate(keyring)

  keyring <- keyring %||% private$keyring
  file_name <- private$keyring_file(keyring)

  if (!file.exists(file_name)) {
    stop("Keyring `", keyring, "` does not exist")
  }

  if (!file.exists(file_name) || !private$is_set_keyring_pass(keyring)) {
    TRUE
  } else {
    tryCatch(
      {
        cached <- private$get_cache(keyring)
        b_file_secret_decrypt(
          cached$check,
          cached$nonce,
          private$get_keyring_pass(keyring)
        )
        FALSE
      },
      error = function(e) {
        if (conditionMessage(e) == "Failed to decrypt") TRUE else stop(e)
      }
    )
  }
}

b_file_keyring_list <- function(self, private) {
  kr_dir <- dirname(private$keyring_file(NULL))
  files <- dir(kr_dir, pattern = "\\.keyring$", full.names = TRUE)
  names <- sub("\\.keyring", "", basename(files))
  num_secrets <- vapply(
    files,
    function(f) length(yaml::yaml.load_file(f)$items),
    integer(1)
  )
  locked <- vapply(
    names,
    function(k) self$keyring_is_locked(keyring = k),
    logical(1)
  )
  data.frame(
    keyring = unname(names),
    num_secrets = unname(num_secrets),
    locked = unname(locked),
    stringsAsFactors = FALSE
  )
}

b_file_keyring_default <- function(self, private) {
  private$keyring
}

b_file_keyring_set_default <- function(self, private, keyring) {
  private$keyring <- keyring
  invisible(self)
}

## --------------------------------------------------------------------
## Private

b__file_keyring_create_direct <- function(
  self,
  private,
  keyring,
  password,
  prompt
) {
  keyring <- keyring %||% private$keyring
  prompt <- prompt %||% "Keyring password: "
  file_name <- private$keyring_file(keyring)

  if (file.exists(file_name)) {
    confirmation(
      paste(
        "are you sure you want to overwrite",
        file_name,
        "(type `yes` if so)"
      ),
      "yes"
    )
  }

  password <- password %||% get_pass(prompt)
  if (is.null(password)) stop("Aborted creating keyring")

  ## File need to exist for $set_keyring_pass() ...
  dir.create(dirname(file_name), recursive = TRUE, showWarnings = FALSE)
  cat("", file = file_name)
  key <- private$set_keyring_pass(password, keyring)

  with_lock(
    file_name,
    private$keyring_write_file(
      keyring,
      nonce = sodium_random(24L),
      items = list(),
      key = key
    )
  )

  invisible(self)
}

b__file_keyring_file <- function(self, private, keyring) {
  keyring <- keyring %||% private$keyring
  keyring_dir <- getOption(
    "keyring_file_dir",
    user_config_dir("r-keyring")
  )
  file.path(keyring_dir, paste0(keyring, ".keyring"))
}

b__file_keyring_read_file <- function(self, private, keyring) {
  keyring <- keyring %||% private$keyring
  file_name <- private$keyring_file(keyring)

  with_lock(file_name, {
    stamp <- file_stamp(keyring)
    yml <- yaml::yaml.load_file(file_name)
  })

  assert_that(
    is_list_with_names(yml, names = c("keyring_info", "items")),
    is_list_with_names(
      yml[["keyring_info"]],
      names = c("keyring_version", "nonce", "integrity_check")
    )
  )

  list(
    nonce = sodium_hex2bin(yml[["keyring_info"]][["nonce"]]),
    items = lapply(yml[["items"]], b__file_validate_item),
    check = yml[["keyring_info"]][["integrity_check"]],
    stamp = stamp
  )
}

b__file_keyring_write_file <- function(
  self,
  private,
  keyring,
  nonce,
  items,
  key
) {
  keyring <- keyring %||% private$keyring
  file_name <- private$keyring_file(keyring)
  nonce <- nonce %||% private$get_cache(keyring)$nonce

  with_lock(
    file_name,
    yaml::write_yaml(
      list(
        keyring_info = list(
          keyring_version = as.character(getNamespaceVersion(.packageName)),
          nonce = sodium_bin2hex(nonce),
          integrity_check = b_file_secret_encrypt(
            paste(sample(letters, 22L, replace = TRUE), collapse = ""),
            nonce,
            key %||% private$get_keyring_pass(keyring)
          )
        ),
        items = items %||% private$get_cache(keyring)$items
      ),
      file_name
    )
  )

  invisible(self)
}

b__file_get_keyring_pass <- function(self, private, keyring) {
  kr_env <- b_file_keyring_env(private$keyring_file(keyring))

  if (is.null(kr_env$key)) {
    key <- private$set_keyring_pass(keyring = keyring)
  } else {
    key <- kr_env$key
  }

  assert_that(is.raw(key), length(key) > 0L)

  key
}

b__file_unset_keyring_pass <- function(self, private, keyring) {
  kr_env <- b_file_keyring_env(private$keyring_file(keyring))

  kr_env$key <- NULL

  invisible(kr_env)
}

b__file_is_set_keyring_pass <- function(self, private, keyring) {
  !is.null(b_file_keyring_env(private$keyring_file(keyring))$key)
}


b__file_set_keyring_pass <- function(self, private, key, keyring) {
  key <- key %||% get_pass("Keyring password: ")
  if (is.null(key)) stop("Aborted setting keyring password")
  assert_that(is_string(key))
  key <- sodium_hash(charToRaw(key))

  kr_env <- b_file_keyring_env(private$keyring_file(keyring))

  kr_env$key <- key
}

b__file_update_cache <- function(self, private, keyring, nonce, check, items) {
  kr_env <- b_file_keyring_env(private$keyring_file(keyring))

  kr <- private$keyring_read_file(keyring)

  nonce <- nonce %||% kr[["nonce"]]
  assert_that(is.raw(nonce), length(nonce) > 0L)
  kr_env$nonce <- nonce

  check <- check %||% kr[["check"]]
  assert_that(is.character(check), length(check) > 0L)
  kr_env$check <- check

  kr_env$items <- lapply(items %||% kr[["items"]], b__file_validate_item)

  kr_env$stamp <- kr$stamp

  kr_env
}

b__file_get_cache <- function(self, private, keyring) {
  keyring_file <- private$keyring_file(keyring)
  kr_env <- b_file_keyring_env(keyring_file)

  if (
    is.null(kr_env$nonce) ||
      is.null(kr_env$stamp) ||
      is.na(kr_env$stamp) ||
      file_stamp(keyring_file) != kr_env$stamp
  ) {
    kr_env <- private$update_cache(keyring)
  }

  assert_that(is.raw(kr_env$nonce), length(kr_env$nonce) > 0L)
  assert_that(is.character(kr_env$check), length(kr_env$check) > 0L)

  list(
    nonce = kr_env$nonce,
    items = lapply(kr_env$items, b__file_validate_item),
    check = kr_env$check
  )
}


## --------------------------------------------------------------------
## helper functions

b_file_secret_encrypt <- function(secret, nonce, key) {
  res <- sodium_data_encrypt(
    charToRaw(secret),
    key,
    nonce
  )

  b_file_split_string(sodium_bin2hex(res))
}

b_file_secret_decrypt <- function(secret, nonce, key) {
  rawToChar(
    sodium_data_decrypt(
      sodium_hex2bin(b_file_merge_string(secret)),
      key,
      nonce
    )
  )
}

b_file_keyring_env <- function(file_name) {
  env_name <- normalizePath(file_name, mustWork = TRUE)

  kr_env <- b_file_keyrings[[env_name]]

  if (is.null(kr_env)) {
    kr_env <- b_file_keyrings[[env_name]] <- new.env(parent = emptyenv())
  }

  kr_env
}

b_file_error <- function(problem, reason = NULL) {
  if (is.null(reason)) {
    info <- problem
  } else {
    info <- paste0(problem, ": ", reason)
  }

  stop("keyring error (file-based keyring), ", info, call. = FALSE)
}

b__file_validate_item <- function(item) {
  assert_that(
    is_list_with_names(item, names = c("service_name", "user_name", "secret")),
    is_string(item[["service_name"]]),
    is_string_or_null(item[["user_name"]]),
    is_string_or_raw(item[["secret"]])
  )

  invisible(item)
}

b_file_split_string <- function(string, width = 78L) {
  assert_that(is_string(string))

  paste(
    lapply(
      seq.int(ceiling(nchar(string) / width)) - 1L,
      function(x) substr(string, x * width + 1L, x * width + width)
    ),
    collapse = "\n"
  )
}

b_file_merge_string <- function(string) {
  assert_that(is_string(string))
  paste(strsplit(string, "\n")[[1L]], collapse = "")
}

b__file_keyring_autocreate <- function(self, private, keyring) {
  keyring <- keyring %||% private$keyring
  file <- private$keyring_file(keyring)
  if (!file.exists(file)) {
    if (is_interactive()) {
      private$keyring_create_direct(
        keyring,
        password = NULL,
        prompt = paste0(
          "The '",
          keyring,
          "' keyring does not exist, enter a keyring password to create it: "
        )
      )
    } else {
      stop("The '", keyring, "' keyring does not exists, create it first!")
    }
  }
}

with_lock <- function(file, expr) {
  timeout <- getOption("keyring_file_lock_timeout", 1000)
  lockfile <- paste0(file, ".lck")
  l <- filelock::lock(lockfile, timeout = timeout)
  if (is.null(l)) stop("Cannot lock keyring file")
  on.exit(filelock::unlock(l), add = TRUE)
  expr
}
