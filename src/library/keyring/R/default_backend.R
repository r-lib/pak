#' Select the default backend and default keyring
#'
#' The default backend is selected
#' 1. based on the `keyring_backend` option. See [base::options()].
#'    This can be set to a character string, and then the
#'    *backend_*`string` class is used to create the default backend.
#' 1. If this is not set, then the `R_KEYRING_BACKEND` environment variable
#'    is checked.
#' 1. If this is not set, either, then the backend is selected
#'    automatically, based on the OS:
#'    1. On Windows, the Windows Credential Store (`"wincred"`) is used.
#'    1. On macOS, Keychain services are selected (`"macos"`).
#'    1. Linux uses the Secret Service API (`"secret_service"`),
#'       and it also checks that the service is available. It is typically
#'       only available on systems with a GUI.
#'    1. If the file backend (`"file"`) is available, it is selected.
#'    1. On other operating systems, secrets are stored in environment
#'       variables (`"env"`).
#'
#' Most backends support multiple keyrings. For these the keyring is
#' selected from:
#' 1. the supplied `keyring` argument (if not `NULL`), or
#' 1. the `keyring_keyring` option.
#'     - You can change this by using `options(keyring_keyring = "NEWVALUE")`
#' 1. If this is not set, the `R_KEYRING_KEYRING` environment variable.
#'     - Change this value with `Sys.setenv(R_KEYRING_KEYRING = "NEWVALUE")`,
#'     either in your script or in your `.Renviron` file.
#'     See [base::Startup] for information about using `.Renviron`
#' 1. Finally, if neither of these are set, the OS default keyring is used.
#'     - Usually the keyring is automatically unlocked when the user logs in.
#'
#' @param keyring Character string, the name of the keyring to use,
#'   or `NULL` for the default keyring.
#' @return The backend object itself.
#'
#'
#' @seealso [backend_env], [backend_file], [backend_macos],
#'          [backend_secret_service], [backend_wincred]
#'
#' @export
#' @name backends

default_backend <- function(keyring = NULL) {
  assert_that(is_string_or_null(keyring))

  backend <- getOption("keyring_backend", "")
  if (identical(backend, "")) backend <- default_backend_env_or_auto()

  ## Is it just a backend name?
  if (is_string(backend)) backend <- backend_factory(backend)

  ## At this point 'backend' is a backend R6 class
  ## Check if a specific keyring is requested
  if (is.null(keyring)) {
    keyring <- getOption(
      "keyring_keyring",
      Sys.getenv("R_KEYRING_KEYRING", "")
    )
  }

  if (!is.null(keyring) && nzchar(keyring)) {
    backend$new(keyring = keyring)
  } else {
    backend$new()
  }
}

default_backend_env_or_auto <- function() {
  backend <- Sys.getenv("R_KEYRING_BACKEND", "")
  if (identical(backend, "")) backend <- default_backend_auto()
  backend
}

default_backend_auto <- function() {
  sysname <- tolower(Sys.info()[["sysname"]])

  if (sysname == "windows" && "wincred" %in% names(known_backends)) {
    backend_wincred
  } else if (sysname == "darwin" && "macos" %in% names(known_backends)) {
    backend_macos
  } else if (
    sysname == "linux" &&
      "secret_service" %in% names(known_backends) &&
      backend_secret_service$new()$is_available()
  ) {
    backend_secret_service
  } else if ("file" %in% names(known_backends) && file_backend_works()) {
    backend_file
  } else {
    if (getOption("keyring_warn_for_env_fallback", TRUE)) {
      warning(
        "Selecting ",
        sQuote("env"),
        " backend. ",
        "Secrets are stored in environment variables"
      )
    }
    backend_env
  }
}

file_backend_works <- function() {
  opt <- options(rlib_interactive = FALSE)
  on.exit(options(opt), add = TRUE)
  tryCatch(
    {
      kb <- backend_file$new()
      krs <- kb$keyring_list()
      def <- kb$keyring_default()
      if (!def %in% krs$keyring) {
        return(FALSE)
      }
      kb$list()
      TRUE
    },
    error = function(err) FALSE
  )
}

backend_factory <- function(name) {
  assert_that(is_string(name))
  if (!name %in% names(known_backends)) {
    stop("Unknown backend: ", sQuote(name))
  }
  class_name <- paste0("backend_", name)
  get(class_name, envir = parent.frame())
}

known_backends <- list(
  "wincred" = backend_wincred,
  "macos" = backend_macos,
  "secret_service" = backend_secret_service,
  "env" = backend_env,
  "file" = backend_file
)
