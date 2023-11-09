
common_hook <- function() {
  substitute({
    # This should not happen in a new R session, but just to be safe
    while ("tools:callr" %in% search()) detach("tools:callr")
    env <- readRDS(`__envfile__`)
    do.call("attach", list(env, pos = length(search()), name = "tools:callr"))
    data <- env$`__callr_data__`
    data$pxlib <- data$load_client_lib(
      data$sofile[[paste0("arch-", .Platform$r_arch)]],
      data$pxdir
    )
    options(error = function() invokeRestart("abort"))
    rm(list = c("data", "env"))

    lapply(
      c("R_ENVIRON", "R_ENVIRON_USER", "R_PROFILE", "R_PROFILE_USER",
        "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE"),
      function(var) {
        bakvar <- paste0("CALLR_", var, "_BAK")
        val <- Sys.getenv(bakvar, NA_character_)
        if (!is.na(val)) {
          do.call("Sys.setenv", structure(list(val), names = var))
        } else {
          Sys.unsetenv(var)
        }
        Sys.unsetenv(bakvar)
      }
    )

    Sys.unsetenv("CALLR_CHILD_R_LIBS")
    Sys.unsetenv("CALLR_CHILD_R_LIBS_SITE")
    Sys.unsetenv("CALLR_CHILD_R_LIBS_USER")

  }, list("__envfile__" = env_file))
}

default_load_hook <- function(user_hook = NULL) {
  prepare_client_files()
  hook <- common_hook()

  if (!is.null(user_hook)) {
    hook <- substitute({ d; u }, list(d = hook, u = user_hook))
  }
  paste0(deparse(hook), "\n")
}

session_load_hook <- function(user_hook = NULL) {
  chook <- common_hook()
  ehook <- substitute({
    data <- as.environment("tools:callr")$`__callr_data__`
    data$pxlib$disable_fd_inheritance()
    rm(data)
  })

  hook <- substitute({ c; e }, list(c = chook, e = ehook))

  if (!is.null(user_hook)) {
    hook <- substitute({ d; u }, list(d = hook, u = user_hook))
  }

  hook <- substitute({
    err_ <- TRUE
    callr_startup_hook <- function() {
      on.exit(if (err_) quit("no", 1, TRUE))
      { `_hook_` }
      err_ <<- FALSE
    }
    callr_startup_hook()
    rm(err_, callr_startup_hook)
  }, list("_hook_" = hook))

  paste0(deparse(hook), "\n")
}

user_hooks_env <- new.env(parent = emptyenv())

#' Add a user hook to be executed before launching an R subprocess
#'
#' This function allows users of `callr` to specify functions that get invoked
#' whenever an R session is launched. The function can modify the environment
#' variables and command line arguments.
#'
#' The prototype of the hook function is `function (options)`, and it is
#' expected to return the modified `options`.
#' @param ... Named argument specifying a hook function to add, or `NULL` to
#'   delete the named hook.
#' @return `add_hook` is called for its side-effects.
#' @export

add_hook <- function(...) {
  args <- list(...)
  if (length(args) != 1L) {
    stop("More than one argument passed to `add_hook`")
  }

  name <- names(args)
  if (is.null(name)) {
    stop("Argument passed to `add_hook` must be named")
  }

  hook <- args[[1]]
  if (is.null(hook)) {
    rm(list = name, envir = user_hooks_env)
  } else {
    assign(name, match.fun(hook), envir = user_hooks_env)
  }

  invisible()
}

call_user_hooks <- function(options) {
  for (name in names(user_hooks_env)) {
    hook <- user_hooks_env[[name]]
    options <- tryCatch(hook(options), error = function (e) options)
  }
  options
}
