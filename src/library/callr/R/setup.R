
setup_script_files <- function(options) {
  within(options, {
    func_file   <- save_function_to_temp(options)
    result_file <- tempfile("callr-res-")
    script_file <- make_vanilla_script_file(
      func_file, result_file, options$error, !is.null(options$stderr)
    )
    tmp_files <- c(tmp_files, func_file, script_file, result_file)
  })
}

save_function_to_temp <- function(options) {
  tmp <- tempfile("callr-fun-")
  options$func <- transport_fun(options$func, options$package)
  # Once we start saving the function environments, we might get
  # "'package:x' may not be available when loading" warnings
  suppressWarnings(saveRDS(
    list(options$func, options$args), file = tmp,
    compress = getOption("callr.compress_transport", FALSE)))
  tmp
}

transport_fun <- function(fun, package,
                          source_refs = getOption("callr.keep.source")) {
  if (!isTRUE(source_refs)) fun <- remove_source(fun)

  if (isTRUE(package)) {
    # Do nothing
  } else if (identical(package, FALSE)) {
    environment(fun) <- .GlobalEnv
  } else if (is_string(package)) {
    environment(fun) <- asNamespace(package)
  } else {
    stop("Invalid `package` value for callr function")
  }

  fun
}

setup_context <- function(options) {

  ## Avoid R CMD check warning...
  repos <- libpath <- system_profile <- user_profile <- load_hook <- NULL

  make_path <- function(x) paste(x, collapse = .Platform$path.sep)

  options <- within(options, {
    ## profiles
    profiles <- make_profiles(system_profile, user_profile, repos, libpath,
                             load_hook, env)
    tmp_files <- c(tmp_files, profiles)

    ## environment files
    envs <- make_environ(profiles, libpath, env)
    tmp_files <- c(tmp_files, envs)

    ## environment variables

    ## First, save these, so we can restore them exactly in the subprocess,
    ## and sub-subprocesses are not affected by our workarounds
    save_env <- c("R_ENVIRON", "R_ENVIRON_USER", "R_PROFILE",
                  "R_PROFILE_USER", "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE")
    keep_set <- save_env %in% names(env)
    save_set <- !keep_set & save_env %in% names(Sys.getenv())
    save_nms <- paste0("CALLR_", save_env, "_BAK")
    env[save_nms[keep_set]] <- env[save_env[keep_set]]
    env[save_nms[save_set]] <- Sys.getenv(save_env[save_set])
    env <- env[setdiff(names(env), save_nms[!keep_set & !save_set])]

    if (is.na(env["R_ENVIRON"])) env["R_ENVIRON"] <- envs[[1]]
    if (is.na(env["R_ENVIRON_USER"])) env["R_ENVIRON_USER"] <- envs[[2]]
    if (is.na(env["R_PROFILE"])) env["R_PROFILE"] <- profiles[[1]]
    if (is.na(env["R_PROFILE_USER"])) env["R_PROFILE_USER"] <- profiles[[2]]

    if (is.na(env["R_LIBS"])) env["R_LIBS"] <- make_path(libpath)
    if (is.na(env["R_LIBS_USER"])) env["R_LIBS_USER"] <- make_path(libpath)
    if (is.na(env["R_LIBS_SITE"])) env["R_LIBS_SITE"] <- make_path(.Library.site)

    env["CALLR_IS_RUNNING"] <- "true"
  })

  call_user_hooks(options)
}

make_profiles <- function(system, user, repos, libpath, load_hook, env) {

  profile_system <- tempfile("callr-spr-")
  profile_user <- tempfile("callr-upr-")

  ## Create file2
  cat("", file = profile_system)
  cat("", file = profile_user)

  ## Add profiles
  if (system) {
    sys <- env["R_PROFILE"]
    if (is.na(sys)) {
      sys <- Sys.getenv(
        "R_PROFILE",
        file.path(R.home("etc"), "Rprofile.site")
      )
    }
    sys <- path.expand(sys)
    if (file.exists(sys)) {
      file.append(profile_system, sys)
      cat("\n", file = profile_system, append = TRUE)
    }
  }

  if (identical(user, "project")) {
    local <- ".Rprofile"
    if (file.exists(local)) user <- local else user <- NA_character_
  } else if (user) {
    user <- env["R_PROFILE_USER"]
    if (is.na(user)) user <- Sys.getenv("R_PROFILE_USER", NA_character_)
    local <- ".Rprofile"
    home  <- path.expand("~/.Rprofile")
    if (is.na(user) && file.exists(local)) user <- local
    if (is.na(user) && file.exists(home)) user <- home
  } else {
    user <- NA_character_
  }

  # Prevent circular inclusion of .Rprofile.
  cat(
    "if (is.null(getOption(\"callr.rprofile_loaded\"))) {",
    "  options(callr.rprofile_loaded = TRUE)",
    sep = "\n",
    file = profile_user,
    append = TRUE
  )

  if (!is.na(user) && file.exists(user)) {
    xpr <- substitute(
      if (file.exists(user)) source(user, local = TRUE),
      list(user = user)
    )
    cat(deparse(xpr), file = profile_user, append = TRUE, sep = "\n")
  }

  ## Override repos, as requested
  for (p in c(profile_system, profile_user)) {
    cat("options(repos=", deparse(repos), ")\n", sep = "", file = p,
        append = TRUE)
  }

  ## Set .Library.site
  cat(".Library.site <- ", deparse(.Library.site),
      "\n.libPaths(.libPaths())\n", file = profile_system, append = TRUE)

  ## Set .libPaths()
  for (p in c(profile_system, profile_user))  {
    cat(".libPaths(", deparse(libpath), ")\n", sep = "", file = p,
        append = TRUE)
  }

  if (!is.null(load_hook)) {
    cat(load_hook, sep = "",  file = profile_user, append = TRUE)
  }

  # End of include guard.
  cat("\n}\n", file = profile_user, append = TRUE)

  c(profile_system, profile_user)
}

make_environ <- function(profiles, libpath, env) {

  env_sys <- tempfile("callr-sev-")
  env_user <- tempfile("callr-uev-")

  for (ef in c(env_sys, env_user)) {
    cat("CALLR_CHILD_R_LIBS=\"${R_LIBS}\"\n",
        "CALLR_CHILD_R_LIBS_USER=\"${R_LIBS_USER}\"\n",
        "CALLR_CHILD_R_LIBS_SITE=\"${R_LIBS_SITE}\"\n",
        file = ef, append = TRUE)
  }

  sys <- env["R_ENVIRON"]
  if (is.na(sys)) sys <- Sys.getenv("R_ENVIRON", NA_character_)
  if (is.na(sys)) sys <- file.path(R.home("etc"), "Renviron.site")
  if (!is.na(sys) && file.exists(sys)) {
    file.append(env_sys, sys)
    cat("\n", file = env_sys, append = TRUE)
  }

  user <- env["R_ENVIRON_USER"]
  if (is.na(user)) user <- Sys.getenv("R_ENVIRON_USER", NA_character_)
  local <- ".Renviron"
  home <- "~/.Renviron"
  if (is.na(user) && file.exists(local)) user <- local
  if (is.na(user) && file.exists(home)) user <- home
  if (!is.na(user) && file.exists(user)) {
    file.append(env_user, user)
    cat("\n", file = env_user, append = TRUE)
  }

  for (ef in c(env_sys, env_user)) {
    cat("R_PROFILE=\"", profiles[[1]], "\"\n", file = ef,
        append = TRUE, sep = "")
    cat("R_PROFILE_USER=\"", profiles[[2]], "\"\n", file = ef,
        append = TRUE, sep = "")
    cat("R_LIBS_SITE=\"${CALLR_CHILD_R_LIBS_SITE:-",
        paste(.Library.site, collapse = .Platform$path.sep), "}\"\n",
        file = ef, append = TRUE, sep = "")
    cat("R_LIBS=\"${CALLR_CHILD_R_LIBS:-",
        paste(libpath, collapse = .Platform$path.sep), "}\"\n",
        file = ef, append = TRUE, sep = "")
    cat("R_LIBS_USER=\"${CALLR_CHILD_R_LIBS_USER:-",
        paste(libpath, collapse = .Platform$path.sep), "}\"\n",
        file = ef, append = TRUE, sep = "")
  }

  c(env_sys, env_user)
}

setup_callbacks <- function(options) {

  ## We cannot easily use `within` here, because the
  ## functions we create will have the wrong environment

  cb <- options$callback
  block_cb <- options$block_callback

  ## This is cumbersome, because we cannot easily set a named list
  ## element to NULL
  options <- append(
    options,
    list("real_block_callback" =
           if (!is.null(block_cb)) function(x, proc) block_cb(x))
  )

  callback_factory <- function(stream) {
    ## Need to evaluate it when the callback is created
    force(stream)

    ## In case there is no output, we create an empty file here
    if (!is.null(stream) && stream != "2>&1") cat("", file = stream)

    if (!is.null(cb)) {
      function(x, proc) {
        if (!is.null(stream)) cat(x, file = stream, sep = "\n", append = TRUE)
        cb(x)
      }

    } else {
      function(x, proc) {
        if (!is.null(stream)) cat(x, file = stream, sep = "\n", append = TRUE)
      }
    }
  }

  options <- append(options, list("real_callback" = callback_factory))
  options
}

setup_r_binary_and_args <- function(options, script_file = TRUE) {
  options$arch <- options$arch %||% "same"
  if (grepl("[/\\\\]", options$arch)) {
    path <- options$arch

  } else if (options$arch != "same") {
    path <- file.path(
      R.home(),
      "bin",
      options$arch,
      if (os_platform() == "windows") "Rterm" else "R"
    )

  } else {
    exec <- if (os_platform() == "windows") "Rterm" else "R"
    path <- file.path(R.home("bin"), exec)
  }

  if (!file.exists(path) &&
      !file.exists(paste0(path, ".exe"))) {
    stop("Cannot find R executable at `", path, "`")
  }

  options$bin <- path
  options$real_cmdargs <-
    c(options$cmdargs, if (script_file) c("-f", options$script_file))
  options
}

setup_rcmd_binary_and_args <- function(options) {

  if (os_platform() == "windows") {
    options$bin <- file.path(R.home("bin"), "Rcmd.exe")
    options$real_cmdargs <- c(options$cmd, options$cmdargs)

  } else {
    options$bin <- file.path(R.home("bin"), "R")
    options$real_cmdargs <- c("CMD", options$cmd, options$cmdargs)
  }

  options
}

setup_rscript_binary_and_args <- function(options) {

  if(os_platform() == "windows") {
    options$bin <- file.path(R.home("bin"), "Rscript.exe")

  } else {
    options$bin <- file.path(R.home("bin"), "Rscript")
  }

  options$real_cmdargs <- c(options$script, options$cmdargs)
  options
}
