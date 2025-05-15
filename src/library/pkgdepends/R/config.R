#' Package configuration (internals)
#'
#' @description
#' Internal machinery for pkgdepends configuration. See [current_config()]
#' for how this machinery can be used to define the configuration of a
#' package.
#'
#' This is a standalone file that can be copied into another package without
#' changes.
#'
#' @format
#' `config` is a list of functions with a closure. You can use it two ways.
#'
#' For _one_ set of configuration values in a package, include this in
#' `.onLoad()`:
#'
#' ```r
#' conf <- config$new("prefix")
#' conf$add("myenrry", "string", "default-value")
#' ...
#' conf$lock()
#' ```
#'
#' For _multiple _ sets of configuration values (e.g. one per object),
#' include a function like this in the package:
#'
#' ```r
#' current_config <- function() {
#'   conf <- config$new("prefix")
#'   conf$add("myentry", "string", "default-value")
#'   ...
#'   conf$lock()
#'   conf
#' }
#' ```
#'
#' and then call `current_config()` every time you need to create a
#' new set of configuration values.
#'
#' @keywords internal
#'
#' @details
#' # Terminology
#'
#' ## Config entry (or entry)
#'
#' A single configurable entity. A config entry has a name. This name is
#' always standardized by replacing dashes with underscores.
#'
#' A config entry has a type: a string, a flag, or some
#' user defined type. It can also have a default value. Examples:
#'
#' * The URL of an API, which can be a string.
#' * Path to a directory to use for caching, either a string, or NULL
#'   (`string_or_null` type).
#' * Number of processor cores to use, a positive integer (`count` type).
#'
#' ## Configuration (or config)
#'
#' A set of configuration entries. `config$new()` returns a configuration.
#' A configuration can be extended by adding new entries to it, until it
#' is locked.
#'
#' ## R option (or option)
#'
#' An option set with [base::options()] and queried with [base::getOption()].
#'
#' ## Configuration prefix
#'
#' A prefix that is typically package specific, and it is used for all
#' config entries of a package, to avoid environment variable and option
#' name clashes between packages.
#'
#' ## Config entry type
#'
#' It defines the allowed values of configuration entries, and also how
#' a string from an environment variable is translated to the value of
#' the entry. See the list of builtin types below. You can also define new
#' types.
#'
#' # Built-in types
#'
#' ## `string`
#'
#' String scalar, `NA` is not allowed.
#'
#' ## `count`
#'
#' Non-negative integer, `NA` is not allowed.
#'
#' ## `flag`
#'
#' Logical scalar, `NA` is not allowed. For environment variables the
#' `TRUE` values are: `yes`, `true`, `1` and `on`, and the `FALSE` values
#' are: `no`, `false`, `0`, `off`. (All are case insensitive.)
#'
#' ## `string_or_null`
#'
#' String or `NULL`. `NA` is not allowed. In environment variables the
#' string `NULL` means an R `NULL` value.
#'
#' ## `character`
#'
#' Character vector without `NA` values. In environment variables the
#' entries are separated by a semicolon.
#'
#' ## `custom`
#'
#' Custom type. An `env_decode` function must be specified for each config
#' entry of this type, otherwise an error is throw if the corresponding
#' environment variable is set.

config <- local({
  `%||%` <- function(l, r) if (is.null(l)) r else l

  # It is used as the check function if no check function is needed
  true <- function(...) TRUE

  is_config_name <- function(name) {
    if (is_string(name)) return(TRUE)
    structure(
      FALSE,
      msg = c(
        "A config entry name ({.arg name} argument) must be a string
         (character scalar).",
        i = "It is {.type {name}} instead."
      )
    )
  }

  is_config_check <- function(check) {
    if (is_string(check) || is.function(check) || is.null(check)) return(TRUE)
    structure(
      FALSE,
      msg = c(
        "A config check function ({.arg check} argument) must be of",
        "*" = "a predicate function that returns a logical scalar, or",
        "*" = "a config type name, a character scalar, or",
        "*" = "{.code NULL} for no value checks.",
        "i" = "{.arg check} is {.type {check}}."
      )
    )
  }

  is_config_env_decoder <- function(env_decode) {
    is_string(env_decode) || is.function(env_decode) || is.null(env_decode)
  }

  standard_name <- function(x) {
    gsub("-", "_", x)
  }

  # Built-in types
  builtin_types <- c(
    "character",
    "count",
    "custom",
    "flag",
    "string",
    "string_or_null"
  )

  # Checks for builtin types
  builtin_checks <- list(
    character = function(x) is.character(x) && !anyNA(x),
    custom = function(x) true(x),
    flag = function(x) is_flag(x),
    string = function(x) is_string(x),
    string_or_null = function(x) is_optional_string(x),
    count = function(x) is_count(x)
  )

  # Builtin environment variable decoders
  builtin_env_decode <- list(
    character = function(x, ...) {
      strsplit(x, ";", fixed = TRUE)[[1]]
    },
    custom = function(x, name, env, ...) {
      throw(pkg_error(
        "Cannot decode config value from environment variable {.envvar {name}}.",
        i = "This is an probably an internal error in the {.pkg {env$package}} package."
      ))
    },
    flag = function(x, name, ...) {
      x <- tolower(x)
      if (tolower(x) %in% c("yes", "true", "1", "on")) return(TRUE)
      if (tolower(x) %in% c("no", "false", "0", "off")) return(FALSE)
      throw(pkg_error(
        "Invalid value for the {.envvar {name}} environment variable.",
        i = "It must be either {.code true} or {.code false}."
      ))
    },
    string = function(x, ...) x,
    string_or_null = function(x, ...) if (identical(x, "NULL")) NULL else x,
    count = function(x, name, ...) {
      num <- suppressWarnings(as.numeric(num))
      if (is.na(num) || !is_count(num)) {
        throw(pkg_error(
          "Cannot interpret environment variable {.envvar {name}} as a
           count: {.code {x}}."
        ))
      }
      as.integer(num)
    }
  )

  get_internal <- function(env, name) {
    assert_that(is_config_name(name))
    name <- standard_name(name)
    if (!name %in% names(env$data)) {
      throw(pkg_error(
        "Unknown configuration entry: {.code {name}}.",
        i = "This is an probably an internal error in the
             {.pkg {env$package}} package."
      ))
    }

    rec <- env$data[[name]]

    # was explicitly set?
    if (!is.null(rec$value)) return(list("set", rec$value))

    # set via options()
    optname <- paste0(env$prefix, name)
    opt <- getOption(optname)
    if (!is.null(opt)) {
      if (!is.null(chk <- env$data[[name]]$check)) chk(opt)
      return(list("option", opt))
    }

    # set via env var
    envvname <- toupper(chartr(".", "_", paste0(env$prefix, name)))
    envv <- Sys.getenv(envvname, NA_character_)
    if (!is.na(envv)) {
      return(list("envvar", rec$env_decode(envv, envvname, env)))
    }

    # otherwise the default, but if it is a function, then call it
    def <- rec$default
    if (is.function(def)) {
      if (length(formals(def)) > 0) {
        def <- def(env)
      } else {
        def <- def()
      }
      if (!is.null(chk <- env$data[[name]]$check)) chk(def)
    }
    list("default", def)
  }

  #' # `config$onload_hook()`: pretty-printing configuration
  #'
  #' Call `config$onload_hook()` from the package's `.onLoad()` function
  #' if you want to define S3 methods to print the package configuration
  #' nicer.
  #'
  #' ## Usage
  #' ```
  #' config$onload_hook()
  #' ```

  onload_hook <- function() {
    register_if_needed("print", "config_v1", print_config)
    register_if_needed("print", "config", print_config)
  }

  #' # `config$new()`: Create a new configuration
  #'
  #' Typically `new()` is called outside of the functions of the
  #' package, so the `config` object is created at install time.
  #'
  #' ## Usage
  #' ```
  #' conf <- config$new(prefix = utils::packageName(parent.frame()))
  #' ```
  #'
  #' ## Arguments
  #' * `prefix`: prefix of the config entry names. For environment
  #'   variables it is converted to uppercase, and dots are converted
  #'   to underscores. The default prefix is the name of the calling package.
  #'   An underscore separator is used between the prefix and the entry
  #'   name for environment variable names. A dot separator is used for
  #'   R option names.
  #'
  #' ## Value
  #' `new()` returns a `config` object, which is a list containing
  #' the configuration data and functions (methods) to query and change it.
  #'
  #' ```r
  #' conf <- config$new()
  #' ```
  #'
  #' # Configuration methods

  new <- function(
    prefix = utils::packageName(parent.frame()),
    package = utils::packageName(parent.frame())
  ) {
    env <- new.env(parent = emptyenv())
    env$prefix <- if (!is.null(prefix)) paste0(prefix, ".")
    env$data <- new.env(parent = emptyenv())
    env$package <- package

    # These can be modified by the user of the config class, as needed,
    # to add user-defined types
    env$types <- builtin_types
    env$checks <- builtin_checks
    env$env_decode = builtin_env_decode

    # --------------------------------------------------------------------
    #' ## `conf$add()`: add a new configuration entry
    #'
    #' ### Usage
    #'
    #' ```r
    #' conf$add(
    #'   name,
    #'   type = conf$types,
    #'   default = NULL,
    #'   check = type[1],
    #'   env_decode = type[1]
    #' )
    #' ```
    #'
    #' ### Arguments
    #'
    #' * `name`: name of the entry.
    #' * `type`: type of the entry, string.
    #' * `default`: default value of config entry. If this is a function,
    #'    then it is called to generate the default, at the time the
    #'    config entry's value is queried.
    #' * `check`: if it is a function, it is called to check the value of
    #'    the entry. The function should return `TRUE` for successful and
    #'    `FALSE` for unsuccessful checks. If `NULL`, then no check is
    #'    performed. It can also be the name of a type, then the default
    #'    check for that type is performed, if any.
    #' * `env_decode`: if it is a function, then it is used to decode the
    #'    value of the entry from an environment variable, i.e. a string.
    #'    It can also be the name of a type, then the `env_decode()`
    #'    function of that type is used for decoding.
    #'
    #' ### Value
    #'
    #' The configuration, invisibly.

    env$add <- function(
      name,
      type = env$types,
      default = NULL,
      check = type[1],
      env_decode = type[1]
    ) {
      # Need to explicitly add `env$types` on R 3.4.x
      type <- match.arg(type, env$types)
      assert_that(
        is_config_name(name),
        is_config_check(check),
        is_config_env_decoder(env_decode)
      )
      name <- standard_name(name)

      if (name %in% names(env$data)) {
        throw(pkg_error(
          "There is already a config entry called {.code {name}}."
        ))
      }

      if (is_string(check)) check <- env$checks[[check]]
      check <- check %||% true
      if (!is.null(default) && !is.function(default)) check(default)

      if (is_string(env_decode)) env_decode <- env$env_decode[[env_decode]]
      env_decode <- env_decode %||% identity

      env$data[[name]] <- list(
        type = type,
        check = check,
        default = default,
        env_decode = env_decode,
        value = NULL
      )
      invisible(env)
    }

    # --------------------------------------------------------------------
    #' ## `conf$get()`: query the value of a configuration entry
    #'
    #' ### Usage
    #'
    #' ```r
    #' conf$get(name)
    #' ```
    #'
    #' ### Arguments
    #'
    #' * `name`: name of the entry.
    #'
    #' ### Value
    #'
    #' Value of the entry.
    #'
    #' * If the entry was set via `conf$set()` or `conf$update()`, then
    #'   that value is returned.
    #' * Otherwise, if the entry is set via an R option (see
    #'   [base::getOption()]), then that value is returned. The config
    #'   prefix is used to get the option name, with a dot separator.
    #'   I.e. for an entry called 'foo', and prefix 'pkg', the `pkg.foo`
    #'   option is used. If the entry has a check function, that is called
    #'   before returning.
    #' * Otherwise, if the entry is set via an environment variable, then
    #'   that value is returned. The config prefix is used to get the name
    #'   of the environment variable. In addition, dots are replaced with
    #'   underscores and the name is converted to uppercase. I.e. for an
    #'   entry called 'foo.bar', and prefix 'pkg', the 'PKG_FOO_BAR`
    #'   environment variable is used.
    #' * Otherwise, if the entry has a default value, that is returned.
    #'   The default value might be a function, in which case that function
    #'   is called to produce a default value. If the entry has a check
    #'   function, that is called before returning.

    env$get <- function(name) {
      get_internal(env, name)[[2]]
    }

    # --------------------------------------------------------------------
    #' ## `conf$set()`: set the value of a configuration entry
    #'
    #' If you `$set()` a config entry, then the `value` used in `$set()`
    #' will be returned by `$get()`, without consulting R options or
    #' environment variables.
    #'
    #' ### Usage
    #'
    #' ```r
    #' conf$set(name, value)
    #' ```
    #'
    #' ### Arguments
    #'
    #' * `name`: name of the entry.
    #' * `value`: value of the entry. If the entry has a check function,
    #'   it is called here.
    #'
    #' ### Value
    #'
    #' The configuration, invisibly.

    env$set <- function(name, value) {
      assert_that(is_config_name(name))
      name <- standard_name(name)
      if (!name %in% names(env$data)) {
        throw(pkg_error(
          "Cannot set unknown config entry: {.code {name}}.",
          i = "See `$list()` for the list of all config entries."
        ))
      }
      if (!is.null(chk <- env$data[[name]]$check)) chk(value)
      env$data[[name]]$value <- value
      invisible(env)
    }

    # --------------------------------------------------------------------
    #' ## `$conf$unset()`: unset a configuration entry
    #'
    #' Note that this function does _not_ unset R options and environment
    #' variables. It merely removes a value that was assigned by `$set()`
    #' or `$update()`.
    #'
    #' ### Usage
    #'
    #' ```r
    #' conf$unset(name)
    #' ```
    #'
    #' ### Arguments
    #'
    #' * `name`: name of the entry.
    #'
    #' ### Value
    #'
    #' The configuration, invisibly.

    env$unset <- function(name) {
      assert_that(is_config_name(name))
      name <- standard_name(name)
      if (!name %in% names(env$data)) {
        throw(pkg_error(
          "Cannot unset unknown config entry: {.code {name}}.",
          i = "See `$list()` for the list of all config entries."
        ))
      }
      env$data[[name]]$value <- NULL
      invisible(env)
    }

    # --------------------------------------------------------------------
    #' ## `cond$update()`: update the values of configuration entries
    #'
    #' You can use this method to set()` and/or `$unset()` multiple config
    #' entries. `NULL` values in `new` will unset the corresponding entry.
    #'
    #' ### Usage
    #'
    #' ```r
    #' conf$update(new)
    #' ```
    #'
    #' ### Arguments
    #'
    #' * `new`: named list, where names are config entry names and values
    #'   are the corresponding config entry values. `NULL` values will
    #'   unset the config entry. The list is processed sequentially, so
    #'   for duplicates the latest values will be in effect. (But every
    #'   value is checked with its check function, if any.)
    #'
    #' ### Value
    #'
    #' The configuration, invisibly.

    env$update <- function(new) {
      for (i in seq_along(new)) {
        if (is.null(new[[i]])) {
          env$unset(names(new)[i])
        } else {
          env$set(names(new)[i], new[[i]])
        }
      }
      invisible(env)
    }

    # --------------------------------------------------------------------
    #' ## `conf$list()`: query the names of all config entries
    #'
    #' Note that their order is non-deterministic currently.
    #'
    #' ### Usage
    #'
    #' ```r
    #' conf$list()
    #' ```
    #'
    #' ### Value
    #'
    #' Character vector.

    env$list <- function() {
      names(env$data)
    }

    # --------------------------------------------------------------------
    #' ## `conf$exists()`: check if a config entry exists
    #'
    #' ### Usage
    #'
    #' ```r
    #' conf$exists(name)
    #' ```
    #'
    #' ### Arguments
    #'
    #' * `name`: name of the entry.
    #'
    #' ### Value
    #'
    #' Logical scalar, `TRUE` if the entry exists in the configuration,
    #' `FALSE` otherwise. Note that `TRUE` does not mean that the value
    #' of the entry was set.

    env$exists <- function(name) {
      assert_that(is_config_name(name))
      name <- standard_name(name)
      name %in% env$list()
    }

    # --------------------------------------------------------------------
    #' ## `conf$lock()`: lock the configuration
    #'
    #' If a configuration is locked, then no more entries can be added to
    #' it.
    #'
    #' ### Usage
    #'
    #' ```r
    #' conf$lock()
    #' ```
    #'
    #' ### Value
    #'
    #' The configuration, invisibly.

    env$lock <- function() {
      lockEnvironment(env$data)
      invisible(env)
    }

    # --------------------------------------------------------------------
    #' ## `conf$fix()`: fix the value of a single configuration entry
    #'
    #' The currently `$set()` value is fixed and cannot be changed any
    #' more. You can fix an entry without setting its value, in which case
    #' R options and environment variables can still be used to update it.
    #'
    #' ### Usage
    #'
    #' ```r
    #' conf$unset(name)
    #' ```
    #'
    #' ### Arguments
    #'
    #' * `name`: name of the entry.
    #'
    #' ### Value
    #'
    #' The configuration, invisibly.

    env$fix <- function(name) {
      assert_that(is_config_name(name))
      name <- standard_name(name)
      if (!name %in% names(env$data)) {
        throw(pkg_error(
          "Cannot fix unknown config entry: {.code {name}}.",
          i = "See `$list()` for the list of all config entries."
        ))
      }
      lockBinding(name, env$data)
      invisible(env)
    }

    # --------------------------------------------------------------------
    #' ## `conf$add_type()`: add a new config entry type to a configuration
    #'
    #' After this the new type can be used in `conf$add()` calls.
    #'
    #' ### Usage
    #'
    #' ```r
    #' conf$add_type(type_name, check, env_decode)
    #' ```
    #'
    #' ### Arguments
    #'
    #' * `type_name`: type name.
    #' * `check`: default check function of the type. Use `NULL` for no
    #'   checks.
    #' * `env_decode`: function to use to decode a value of the type from
    #'   an environment variable (i.e. string).
    #'
    #' ### Value
    #'
    #' The configuration, invisibly.

    env$add_type <- function(type_name, check, env_decode) {
      assert_that(
        is_string(type_name),
        is.function(check),
        is.function(env_decode)
      )
      if (type_name %in% env$types) {
        throw(pkg_error(
          "There is already a config entry type called {.code {type_name}}."
        ))
      }
      env$types <- c(env$types, type_name)
      env$checks[[type_name]] <- check
      env$env_decode[[type_name]] <- env_decode
      invisible(env)
    }

    structure(env, class = c("config_v1", "config"))
  }

  print_config <- function(x, ...) {
    nms <- names(x$data)
    all <- structure(
      lapply(nms, function(n) get_internal(x, n)),
      names = nms
    )
    cat0("# ", substr(x$prefix, 1, nchar(x$prefix) - 1), " config\n")
    for (i in seq_along(all)) {
      cat0("## ", names(all)[i], "\n")
      cat0("<", all[[i]][[1]], ">\n")
      print(all[[i]][[2]])
      cat0("\n")
    }
    invisible(x)
  }

  register_if_needed <- function(generic, class, fun) {
    if (is.null(utils::getS3method(generic, class, TRUE))) {
      registerS3method(generic, class, fun, baseenv())
    }
  }

  structure(
    list(
      .internal = environment(),
      new = new,
      onload_hook = onload_hook
    ),
    class = c("standalone_config", "standalone")
  )
})
