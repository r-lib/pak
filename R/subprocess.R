
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
  body(func2) <- substitute({
    withCallingHandlers(
      cliapp_message = function(msg) {
        withCallingHandlers(
          asNamespace("cliapp")$cli_server_default(msg),
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

new_remote_session <- function(create = TRUE) {
  get_private_lib(create = create)
  load_private_packages(create = create)
  callr <- pkg_data$ns$callr
  crayon <- pkg_data$ns$crayon
  opts <- callr$r_session_options(stderr = NULL,  stdout = NULL)
  opts$env <- c(
    opts$env, R_PKG_SHOW_PROGRESS = is_verbose(),
    R_PKG_PKG_WORKER = "true",
    R_PKG_PKG_COLORS = as.character(crayon$has_color()),
    R_PKG_PKG_NUM_COLORS = as.character(crayon$num_colors()))
  opts$load_hook <- quote({
    cliapp::start_app(theme = cliapp::simple_theme())
  })
  pkg_data$remote <- callr$r_session$new(opts, wait = FALSE)
}

try_new_remote_session <- function() {
  tryCatch({
    check_for_private_lib()
    load_private_packages(create = FALSE)
    new_remote_session(create = FALSE)
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

load_private_packages <- function(create = TRUE) {
  load_private_package("crayon", create = create)
  load_private_package("ps", create = create)
  load_private_package("processx", "c_", create = create)
  load_private_package("callr", create = create)
}

load_private_package <- function(package, reg_prefix = "", create = TRUE,
                                 lib = get_private_lib(create = create))  {
  if (!is.null(pkg_data$ns[[package]])) return()

  ## Load the R code
  pkg_env <- new.env(parent = asNamespace(.packageName))
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

  lazyLoad(file.path(pkg_dir, "R", package), envir = pkg_env)

  ## Reset environments
  set_function_envs(pkg_env, pkg_env)

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
    withCallingHandlers(
      pkg_env$.onLoad(pkg_dir, package),
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
  callr::r
  cli::rule
  crayon::red
  filelock::lock
  invisible()
}
