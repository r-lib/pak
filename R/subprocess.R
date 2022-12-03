
## ----------------------------------------------------------------------
## Helper functions
## ----------------------------------------------------------------------

remote_is_alive <- function() {
  inherits(rs <- pkg_data$remote, "process") && rs$is_alive()
}

remote <- function(func, args = list()) {
  restart_remote_if_needed()
  load_private_packages()
  on.exit(restart_remote_if_needed(), add = TRUE)

  rs <- pkg_data$remote
  state <- rs$get_state()
  if (state %in% c("busy", "starting")) {
    pr <- pkg_data$ns$processx$poll(list(rs$get_poll_connection()), 5000)[[1]]
    state <- rs$get_state()
    if (state == "starting") {
      rs$read()
      state <- rs$get_state()
    }
  }
  if (state != "idle") stop("Subprocess is busy or cannot start")

  func2 <- func
  subst_args <- list(
    "__body__" = body(func),
    "__verbosity__" = is_verbose(),
    "__repos__" = getOption("repos")
  )
  body(func2) <- substitute({
      withCallingHandlers(
        cli_message = function(msg) {
          withCallingHandlers(
            asNamespace("cli")$cli_server_default(msg),
            message = function(mmsg) {
              class(mmsg) <- c("callr_message", "message", "condition")
              signalCondition(mmsg)
              invokeRestart("cli_message_handled")
            }
          )
          invokeRestart("cli_message_handled")
        },
        {
          options(pkg.show_progress = `__verbosity__`, repos = `__repos__`)
          `__body__`
        }
      )
  }, subst_args)

  opts <- options()
  pkg_options <- opts[grepl("^pkg[.]", names(opts))]
  envs <- Sys.getenv()
  pkg_envs <- envs[grepl("^PKG_", names(envs))]
  rs$run(function(new_opts, new_envs) {
    opts <- options()
    old_opts <- opts[grepl("^pkg[.]", names(opts))]
    # remove all pkg.* options
    options(structure(
      vector(length(old_opts), mode = "list"),
      names = names(old_opts)
    ))
    # set new ones
    options(new_opts)

    envs <- Sys.getenv()
    old_envs <- envs[grepl("^PKG_", names(envs))]
    Sys.unsetenv(old_envs)
    if (length(new_envs)) do.call("Sys.setenv", as.list(new_envs))
  }, list(new_opts = pkg_options, new_envs = pkg_envs))

  res <- withCallingHandlers(
    callr_message = function(msg) {
      withRestarts({
        signalCondition(msg)
        out <- if (is_interactive() || sink.number() > 0) stdout() else stderr()
        cat(conditionMessage(msg), file = out, sep = "")
      }, muffleMessage = function() NULL)
      if (!is.null(findRestart("cli_message_handled"))) {
        invokeRestart("cli_message_handled")
      }
    },
    rs$run_with_output(func2, args)
  )

  if (!is.null(res$error)) {
    if (inherits(res$error, "callr_status_error")) {
      res$error$message <- "error in pak subprocess"
    }
    err$throw(res$error)
  }

  res$result
}

#' @export
print.pak_error <- function(x, ...) {
  cat(x$formatted_message, sep = "\n")
}

new_remote_session <- function() {
  load_private_packages()
  callr <- pkg_data$ns$callr
  cli <- pkg_data$ns$cli
  opts <- callr$r_session_options(stderr = NULL,  stdout = NULL)
  opts$env <- c(
    opts$env,
    R_PKG_SHOW_PROGRESS = is_verbose(),
    R_PKG_PKG_WORKER = "true",
    R_PKG_NUM_COLORS = as.character(cli$num_ansi_colors()),
    R_PKG_DYNAMIC_TTY = cli$is_dynamic_tty(),
    PKGCACHE_NO_PILLAR = "true"
  )
  pkg_data$remote <- callr$r_session$new(opts, wait = FALSE)
}

try_new_remote_session <- function() {
  tryCatch({
    load_private_packages()
    new_remote_session()
  }, error = function(e) e)
}

restart_remote_if_needed <- function() {
  "!DEBUG Restarting background process"
  rs <- pkg_data$remote
  if (inherits(rs, "r_session") &&
      rs$is_alive() &&
      rs$get_state() != "busy") return()

  ## Try to interrupt nicely (SIGINT/CTRL+C), if that fails within 100ms,
  ## kill it.
  if (inherits(rs, "r_session")) {
    rs$interrupt()
    rs$wait(100)
    rs$kill()
  }

  new_remote_session()
}

load_private_cli <- function() {
  if (!is.null(pkg_data$ns$cli)) return(pkg_data$ns$cli)
  load_private_package("glue")
  old <- Sys.getenv("CLI_NO_THREAD", NA_character_)
  Sys.setenv(CLI_NO_THREAD = "1")
  on.exit(
    if (is.na(old)) {
      Sys.unsetenv("CLI_NO_THREAD")
    } else {
      Sys.setenv(CLI_NO_THREAD = old)
    },
    add = TRUE
  )
  load_private_package("cli")
  pkg_data$ns$cli
}

load_private_packages <- function() {
  load_private_package("glue")
  load_private_cli()
  load_private_package("ps")
  load_private_package("processx", "c_")
  load_private_package("callr")
}

load_private_package <- function(package, reg_prefix = "",
                                 lib = private_lib_dir()) {
  if (!is.null(pkg_data$ns[[package]])) return()

  ## Load the R code
  pkg_env <- new.env(parent = asNamespace(.packageName))
  if (!file.exists(file.path(lib, package))) {
    stop("Cannot load ", package, " from the private library")
  }
  pkg_dir0 <- normalizePath(file.path(lib, package))
  mkdirp(pkg_dir <- file.path(tempfile(), package))
  pkg_dir <- normalizePath(pkg_dir)
  file.copy(pkg_dir0, dirname(pkg_dir), recursive = TRUE)
  pkg_env[[".packageName"]] <- package
  pkg_env[["__pkg-dir__"]] <- pkg_dir

  reg.finalizer(pkg_env, onexit = TRUE, function(x) {
    tryCatch({
      pkg_dir <- pkg_env[["__pkg-dir__"]]
      if (!is.null(pkg_dir)) pkg_dir <- suppressWarnings(normalizePath(pkg_dir))
      if (!is.null(pkg_env[[".onUnload"]])) {
        tryCatch(pkg_env[[".onUnload"]](pkg_dir), error = function(e) e)
      }
      libs <- .dynLibs()
      paths <- suppressWarnings(normalizePath(vcapply(libs, "[[", "path")))
      matchidx <- grepl(pkg_dir, paths, fixed = TRUE)
      if (any(matchidx)) {
        pkglibs <- libs[matchidx]
        for (lib in pkglibs) dyn.unload(lib[["path"]])
        .dynLibs(libs[!matchidx])
      }
      unlink(dirname(pkg_dir), recursive = TRUE, force = TRUE)
    }, error = function(e) e)
  })

  tryCatch(
    suppressWarnings(lazyLoad(file.path(pkg_dir, "R", package), envir = pkg_env)),
    error = function(err) {
      err$message <- paste0(
        "Cannot load ", package, " from the private library: ",
        err$message
      )
      stop(err)
    }
  )

  sysdata <- file.path(pkg_dir, "R", "sysdata.rdb")
  if (file.exists(sysdata)) {
    lazyLoad(file.path(pkg_dir, "R", "sysdata"), envir = pkg_env)
  }

  ## Reset environments
  set_function_envs(pkg_env, pkg_env)
  ## Sometimes a package refers to its env, this is one known instance.
  ## We could also walk the whole tree, but probably not worth it.
  if (!is.null(pkg_env$err$.internal$package_env)) {
    pkg_env$err$.internal$package_env <- pkg_env
  }

  # patch processx and callr
  if (package %in% c("callr", "processx")) {
    assign("has_cli", function() TRUE, envir = pkg_env$err$.internal)
  }
  if (package == "callr") {
    registerS3method(
      "format",
      "callr_status_error",
      pkg_env$format.callr_status_error,
      baseenv()
    )
    registerS3method(
      "print",
      "callr_status_error",
      pkg_env$print.callr_status_error,
      baseenv()
    )
  }

  # Hack to avoid S3 dispatch
  if (package == "glue") {
    pkg_env$as_glue <- pkg_env$as_glue.character
  }

  ## Load shared library
  dll_file <- file.path(pkg_dir, "libs", .Platform$r_arch,
                        paste0(package, .Platform$dynlib.ext))
  if (file.exists(dll_file)) {
    dll <- dyn.load(dll_file)
    dll[["name"]] <- paste0("pkg-", dll[["name"]])
    .dynLibs(c(.dynLibs(), list(dll)))
    natfuns <- getDLLRegisteredRoutines(dll)$.Call
    for (natfun in natfuns) {
      pkg_env[[paste0(reg_prefix, natfun$name)]] <- natfun
    }
  }

  pkg_env[["::"]] <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    if (pkg %in% names(pkg_data$ns)) {
      pkg_data$ns[[pkg]][[name]]
    } else {
      getExportedValue(pkg, name)
    }
  }
  environment(pkg_env[["::"]]) <- pkg_env

  pkg_env[["asNamespace"]] <- function(ns, ...) {
    if (ns %in% names(pkg_data$ns)) {
      pkg_data$ns[[ns]]
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
  pkg_data$ns[[package]] <- pkg_env
  if (".onLoad" %in% names(pkg_env)) {
    if (package == "callr") {
      px <- pkg_data$ns$processx[["__pkg-dir__"]]
      Sys.setenv(CALLR_PROCESSX_CLIENT_LIB = px)
    }
    withCallingHandlers(
      pkg_env$.onLoad(dirname(pkg_dir), package),
      error = function(e) pkg_data$ns[[package]] <<- NULL
    )
  }

  invisible()
}

set_function_envs <- function(within, new) {
  old <- .libPaths()
  .libPaths(character())
  on.exit(.libPaths(old), add = TRUE)
  nms <- names(within)

  is_target_env <- function(x) {
    identical(x, base::.GlobalEnv) || environmentName(x) != ""
  }

  suppressWarnings({
    for (nm in nms) {
      if (is.function(within[[nm]])) {
        if (is_target_env(environment(within[[nm]])))  {
          environment(within[[nm]]) <- new
        } else if (is_target_env(parent.env(environment(within[[nm]])))) {
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

## This is a workaround for R CMD check

r_cmd_check_fix <- function() {
  glue::glue
  callr::r
  cli::rule
  filelock::lock
  invisible()
}
