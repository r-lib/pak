
## ----------------------------------------------------------------------
## Helper functions
## ----------------------------------------------------------------------

remote_is_alive <- function() {
  inherits(rs <- pkgman_data$remote, "process") && rs$is_alive()
}

should_remote <- function() {
  !isFALSE(getOption("pkgman.subprocess"))
}

remote <- function(func, args = list()) {
  restart_remote_if_needed()
  on.exit(restart_remote_if_needed(), add = TRUE)

  rs <- pkgman_data$remote
  state <- rs$get_state()
  if (state %in% c("busy", "starting")) {
    pr <- pkgman_data$ns$processx$poll(list(rs$get_poll_connection()), 5000)[[1]]
    state <- rs$get_state()
    if (state == "starting") {
      rs$read()
      state <- rs$get_state()
    }
  }
  if (state != "idle") stop("Subprocess is busy or cannot start")

  func2 <- func
  body(func2) <- substitute({
    withCallingHandlers(
      cliapp_message = function(msg) {
        withCallingHandlers(
          cliapp:::cli_server_default(msg),
          message = function(mmsg) {
            class(mmsg) <- c("callr_message", "message", "condition")
            signalCondition(mmsg)
            invokeRestart("muffleMessage")
          }
        )
        invokeRestart("muffleMessage")
      },
      `__body__`
    )},
    list("__body__" = body(func))
  )

  res <- withCallingHandlers(
    callr_message = function(msg) {
      message(msg)
      if (!is.null(findRestart("muffleMessage"))) {
        invokeRestart("muffleMessage")
      }
    },
    rs$run_with_output(func2, args)
  )
  if (!is.null(res$error)) stop(res$error)

  res$result
}

new_remote_session <- function() {
  callr <- pkgman_data$ns$callr
  crayon <- pkgman_data$ns$crayon
  opts <- callr$r_session_options(stderr = NULL,  stdout = NULL)
  opts$env <- c(
    opts$env, R_PKG_SHOW_PROGRESS = is_verbose(),
    R_PKG_PKGMAN_WORKER = "true",
    R_PKG_PKGMAN_COLORS = as.character(crayon$has_color()),
    R_PKG_PKGMAN_NUM_COLORS = as.character(crayon$num_colors()))
  opts$load_hook <- quote({
    cliapp::start_app(theme = cliapp::simple_theme())
  })
  pkgman_data$remote <- callr$r_session$new(opts, wait = FALSE)
}

restart_remote_if_needed <- function() {
  "!DEBUG Restarting background process"
  rs <- pkgman_data$remote
  if (inherits(rs, "r_session") &&
      rs$is_alive() &&
      rs$get_state() != "busy") return()

  ## Try to interrupt nicely (SIGINT/CTRL+C), if that fails within 100ms,
  ## kill it.
  rs$interrupt()
  rs$wait(100)
  rs$kill()
  new_remote_session()
}

load_private_package <- function(package, reg_prefix = "")  {
  ## First we try ps, that has no dependencies

  if (!is.null(ns <- pkgman_data$ns[[package]])) return(ns)

  priv <- get_private_lib()

  ## Load the R code
  pkg_env <- new.env(parent = asNamespace(.packageName))
  pkg_dir <- normalizePath(file.path(priv, package))
  lazyLoad(file.path(pkg_dir, "R", package), envir = pkg_env)

  ## Reset environments
  set_function_envs(pkg_env, pkg_env)

  ## Load shared library
  dll_file <- file.path(pkg_dir, "libs", .Platform$r_arch,
                        paste0(package, .Platform$dynlib.ext))
  if (file.exists(dll_file)) {
    dll <- dyn.load(dll_file)
    natfuns <- getDLLRegisteredRoutines(dll)$.Call
    for (natfun in natfuns) {
      pkg_env[[paste0(reg_prefix, natfun$name)]] <- natfun
    }
  }

  pkg_env[["::"]] <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    if (pkg %in% names(pkgman_data$ns)) {
      pkgman_data$ns[[pkg]][[name]]
    } else {
      getExportedValue(pkg, name)
    }
  }
  environment(pkg_env[["::"]]) <- pkg_env

  pkg_env[["asNamespace"]] <- function(ns, ...) {
    if (ns %in% names(pkgman_data$ns)) {
      pkgman_data$ns[[ns]]
    } else {
      base::asNamespace(ns, ...)
    }
  }
  environment(pkg_env[["asNamespace"]]) <- pkg_env

  pkg_env[["UseMethod"]] <- function(generic, object) {
    base::UseMethod(generic, object)
  }
  environment(pkg_env[["UseMethod"]]) <- pkg_env

  ## We add the env before calling .onLoad, because .onLoad might refer
  ## to the package env via asNamespace(), e.g. the ps package does that.
  ## In theory we should handle errors in .onLoad...
  pkgman_data$ns[[package]] <- pkg_env
  if (".onLoad" %in% names(pkg_env)) pkg_env$.onLoad(priv, package)

  pkg_env
}

set_function_envs <- function(within, new) {
  old <- .libPaths()
  .libPaths(character())
  on.exit(.libPaths(old), add = TRUE)
  nms <- names(within)
  suppressWarnings({
    for (nm in nms) {
      if (is.function(within[[nm]])) {
        if (identical(environment(within[[nm]]), base::.GlobalEnv)) {
          environment(within[[nm]]) <- new
        } else if (identical(parent.env(environment(within[[nm]])),
                             base::.GlobalEnv)) {
          parent.env(environment(within[[nm]])) <- new
        }
      } else if ("R6ClassGenerator" %in% class(within[[nm]])) {
        within[[nm]]$parent_env <- new
        for (mth in names(within[[nm]]$public_methods)) {
          environment(within[[nm]]$public_methods[[mth]]) <- new
        }
        for (mth in names(within[[nm]]$private_methods)) {
          environment(within[[nm]]$private_methods[[mth]]) <- new
        }
      }
    }
  })

  invisible()
}
