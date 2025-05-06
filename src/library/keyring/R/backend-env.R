#' Environment variable keyring backend
#'
#' This is a simple keyring backend, that stores/uses secrets in
#' environment variables of the R session.
#'
#' It does not support multiple keyrings. It also does not support listing
#' all keys, since there is no way to distinguish keys from regular
#' environment variables.
#'
#' It does support service names and usernames: they will be separated
#' with a `:` character in the name of the environment variable. (Note that
#' such an environment variable typically cannot be set or queried from a
#' shell, but it can be set and queried from R or other programming
#' languages.)
#'
#' See [backend] for the documentation of the class's methods.
#'
#' @family keyring backends
#' @export
#' @include backend-class.R
#' @examples
#' \dontrun{
#' env <- backend_env$new()
#' env$set("r-keyring-test", username = "donaldduck")
#' env$get("r-keyring-test", username = "donaldduck")
#' Sys.getenv("r-keyring-test:donaldduck")
#'
#' # This is an error
#' env$list()
#'
#' # Clean up
#' env$delete("r-keyring-test", username = "donaldduck")
#' }

backend_env <- R6Class(
  "backend_env",
  inherit = backend,
  public = list(
    name = "env",
    get = function(service, username = NULL, keyring = NULL)
      b_env_get(self, private, service, username, keyring),
    set = function(
      service,
      username = NULL,
      keyring = NULL,
      prompt = "Password: "
    ) b_env_set(self, private, service, username, keyring, prompt),
    set_with_value = function(
      service,
      username = NULL,
      password = NULL,
      keyring = NULL
    ) b_env_set_with_value(self, private, service, username, password, keyring),
    delete = function(service, username = NULL, keyring = NULL)
      b_env_delete(self, private, service, username, keyring),
    list = function(service = NULL, keyring = NULL)
      b_env_list(self, private, service, keyring),

    docs = function() {
      modifyList(
        super$docs(),
        list(
          . = "Store secrets in environment variables."
        )
      )
    }
  ),
  private = list(
    env_to_var = function(service, username) {
      b_env_to_var(self, private, service, username)
    }
  )
)

warn_for_keyring <- function(keyring) {
  if (!is.null(keyring)) {
    warning(
      "The 'env' backend does not support multiple keyrings, ",
      "the 'keyring' argument is ignored"
    )
  }
}

b_env_get <- function(self, private, service, username, keyring) {
  warn_for_keyring(keyring)
  username <- username %||% getOption("keyring_username")
  var <- private$env_to_var(service, username)
  res <- Sys.getenv(var, NA_character_)
  if (is.na(res)) stop("Cannot find password")
  res
}

b_env_set <- function(self, private, service, username, keyring, prompt) {
  warn_for_keyring(keyring)
  password <- get_pass(prompt)
  if (is.null(password)) stop("Aborted setting keyring key")
  username <- username %||% getOption("keyring_username")
  b_env_set_with_value(
    self,
    private,
    service,
    username,
    password,
    keyring = NULL
  )
  invisible(self)
}

b_env_set_with_value <- function(
  self,
  private,
  service,
  username,
  password,
  keyring
) {
  warn_for_keyring(keyring)
  username <- username %||% getOption("keyring_username")
  var <- private$env_to_var(service, username)
  do.call(Sys.setenv, structure(list(password), names = var))
  invisible(self)
}

b_env_delete <- function(self, private, service, username, keyring) {
  warn_for_keyring(keyring)
  username <- username %||% getOption("keyring_username")
  var <- private$env_to_var(service, username)
  Sys.unsetenv(var)
  invisible(self)
}

b_env_to_var <- function(self, private, service, username, keyring) {
  if (is.null(username)) {
    service
  } else {
    paste0(service, ":", username)
  }
}

b_env_list <- function(self, private, service, keyring) {
  if (is.null(service)) stop("'service' is required for 'env' backend.")

  keys <- gsub(
    paste(service, ":", sep = ""),
    "",
    Filter(function(e) str_starts_with(e, service), names(Sys.getenv()))
  )

  data.frame(
    service = rep(service, length(keys)),
    username = keys,
    stringsAsFactors = FALSE
  )
}
