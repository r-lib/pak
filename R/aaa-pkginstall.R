
#' Perform a package installation plan, as creted by pkgdepends
#'
#' @param plan Package plan object, returned by pkgdepends
#' @param lib Library directory to install to.
#' @param num_workers Number of worker processes to use.
#' @return Information about the installation process.
#'
#' @noRd

install_package_plan <- function(plan, lib = .libPaths()[[1]],
                                 num_workers = 1) {

  start <- Sys.time()

  required_columns <- c(
    "type", "binary", "dependencies", "file", "vignettes",
    "needscompilation", "metadata", "package")
  stopifnot(
    inherits(plan, "data.frame"),
    all(required_columns %in% colnames(plan)),
    is_string(lib),
    is_count(num_workers, min = 1L)
  )

  config <- list(lib = lib, num_workers = num_workers)
  state <- make_start_state(plan, config)
  state$progress <- create_progress_bar(state)
  on.exit(done_progress_bar(state), add =  TRUE)

  withCallingHandlers({

    ## Initialise one task for each worker
    for (i in seq_len(state$config$num_workers)) {
      task <- select_next_task(state)
      state <- start_task(state, task)
    }

    repeat {
      if (are_we_done(state)) break;
      update_progress_bar(state)

      events <- poll_workers(state)
      state <- handle_events(state, events)
      task  <- select_next_task(state)
      state <- start_task(state, task)
    }
  }, error = function(e) kill_all_processes(state))

  create_install_result(state)
}

make_start_state <- function(plan, config) {

  ## We store the data about build and installation here
  install_cols <- data.frame(
    stringsAsFactors = FALSE,
    build_done = (plan$type %in% c("deps", "installed")) | plan$binary,
    build_time = I(rep_list(nrow(plan), as.POSIXct(NA))),
    build_error = I(rep_list(nrow(plan), list())),
    build_stdout = I(rep_list(nrow(plan), character())),
    build_stderr = I(rep_list(nrow(plan), character())),
    install_done = plan$type %in% c("deps", "installed"),
    install_time = I(rep_list(nrow(plan), as.POSIXct(NA))),
    install_error = I(rep_list(nrow(plan), list())),
    install_stdout = I(rep_list(nrow(plan), character())),
    install_stderr = I(rep_list(nrow(plan), character())),
    worker_id = NA_character_
  )
  plan <- cbind(plan, install_cols)

  installed <- plan$package[plan$install_done]
  plan$deps_left <- lapply(plan$dependencies, setdiff, installed)

  list(
    plan = plan,
    workers = list(),
    config = config)
}

are_we_done <- function(state) {
  all(state$plan$install_done)
}

poll_workers <- function(state) {
  if (length(state$workers)) {
    timeout <- get_timeout(state)
    procs <- lapply(state$workers, "[[", "process")
    res <- processx::poll(procs, ms = timeout)
    map_lgl(res, function(x) "ready" %in% x)

  } else {
    logical()
  }
}

get_timeout <- function(state) 100

handle_events <- function(state, events) {
  for (i in which(events)) state <- handle_event(state, i)
  state$workers <- drop_nulls(state$workers)
  state
}

handle_event <- function(state, evidx) {
  proc <- state$workers[[evidx]]$process

  ## Read out stdout and stderr. If process is done, then read out all
  if (proc$is_alive()) {
    state$workers[[evidx]]$stdout <-
      c(state$workers[[evidx]]$stdout, out <- proc$read_output(n = 10000))
    state$workers[[evidx]]$stderr <-
      c(state$workers[[evidx]]$stderr, err <- proc$read_error(n = 10000))
  } else {
    state$workers[[evidx]]$stdout <-
      c(state$workers[[evidx]]$stdout, out <- proc$read_all_output())
    state$workers[[evidx]]$stderr <-
      c(state$workers[[evidx]]$stderr, err <- proc$read_all_error())
  }

  ## If there is still output, then wait a bit more
  if (proc$is_alive() ||
      proc$is_incomplete_output() || proc$is_incomplete_error()) {
    return(state)
  }

  ## Otherwise we are done. Remove worker
  worker <- state$workers[[evidx]]
  state$workers[evidx] <- list(NULL)

  ## Post-process, this will throw on error
  if (is.function(proc$get_result)) proc$get_result()

  ## Cut stdout and stderr to lines
  worker$stdout <- cut_into_lines(worker$stdout)
  worker$stderr <- cut_into_lines(worker$stderr)

  ## Record what was done
  stop_task(state, worker)
}

select_next_task <- function(state) {

  ## Cannot run more workers?
  if (length(state$workers) >= state$config$num_workers) {
    return(task("idle"))
  }

  ## Can we select a source package build? Do that.
  can_build <- which(
    ! state$plan$build_done &
    map_int(state$plan$deps_left, length) == 0 &
    is.na(state$plan$worker_id))

  if (any(can_build)) {
    pkgidx <- can_build[1]
    return(task("build", pkgidx = pkgidx))
  }

  ## TODO: can we select a binary that is depended on by a source package?

  ## Otherwise select a binary if there is one
  can_install <- which(
    state$plan$build_done &
    ! state$plan$install_done &
    is.na(state$plan$worker_id))

  if (any(can_install)) {
    pkgidx <- can_install[1]
    return(task("install", pkgidx = pkgidx))
  }

  ## Detect internal error
  if (!all(state$plan$install_done) && all(is.na(state$plan$worker_id))) {
    stop("Internal error, no task running and cannot select new task")
  }

  ## Looks like nothing else to do
  task("idle")
}

task <- function(name, ...) {
  list(name = name, args = list(...))
}

start_task <- function(state, task) {
  if (task$name == "idle") {
    state

  } else if (task$name == "build") {
    start_task_build(state, task)

  } else if (task$name == "install") {
    start_task_install(state, task)

  } else {
    stop("Unknown task, internal error")
  }
}

get_worker_id <- (function() {
  id <- 0
  function() {
    id <<- id + 1
    as.character(id)
  }
})()

make_build_process <- function(path, tmp_dir, lib, vignettes,
                               needscompilation) {

  ## with_libpath() is needed for newer callr, which forces the current
  ## lib path in the child process.
  withr::with_libpaths(lib, action = "prefix",
      pkgbuild::pkgbuild_process$new(
      path, tmp_dir, binary = TRUE, vignettes = vignettes,
      needs_compilation = needscompilation, compile_attributes = FALSE,
      args = glue::glue("--library={lib}"))
    )
}

start_task_build <- function(state, task) {
  pkgidx <- task$args$pkgidx
  path <- if (state$plan$type[pkgidx] == "local") {
      sub("^file://", "", state$plan$sources[[pkgidx]])
    } else {
      state$plan$file[pkgidx]
    }
  vignettes <- state$plan$vignettes[pkgidx]
  needscompilation <- !identical(state$plan$needscompilation[pkgidx], "no")
  tmp_dir <- create_temp_dir()
  lib <- state$config$lib

  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]
  alert("info", "Building {pkg {pkg}} {version {version}}")

  px <- make_build_process(path, tmp_dir, lib, vignettes, needscompilation)
  worker <- list(id = get_worker_id(), task = task, process = px,
                 stdout = character(), stderr = character())
  state$workers <- c(
    state$workers, structure(list(worker), names = worker$id))
  state$plan$worker_id[pkgidx] <- worker$id
  state$plan$build_time[[pkgidx]] <- Sys.time()
  state
}

start_task_install <- function(state, task) {
  pkgidx <- task$args$pkgidx
  filename <- state$plan$file[pkgidx]
  lib <- state$config$lib
  metadata <- state$plan$metadata[[pkgidx]]

  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]
  update_progress_bar(state)

  px <- make_install_process(filename, lib = lib, metadata = metadata)
  worker <- list(
    id = get_worker_id(), task = task, process = px,
    stdout = character(), stderr = character())

  state$workers <- c(
    state$workers, structure(list(worker), names = worker$id))
  state$plan$worker_id[pkgidx] <- worker$id
  state$plan$install_time[[pkgidx]] <- Sys.time()
  state
}

stop_task <- function(state, worker) {
  if (worker$task$name == "build") {
    stop_task_build(state, worker)

  } else if (worker$task$name == "install") {
    stop_task_install(state, worker)

  } else {
    stop("Unknown task, internal error")
  }
}

stop_task_build <- function(state, worker) {

  ## TODO: make sure exit status is non-zero on build error!
  success <- worker$process$get_exit_status() == 0

  pkgidx <- worker$task$args$pkgidx
  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]
  time <- Sys.time() - state$plan$build_time[[pkgidx]]
  ptime <- prettyunits::pretty_sec(as.numeric(time, units = "secs"))

  if (success) {
    alert("success", "Built {pkg {pkg}} {version {version}} \\
           {timestamp {ptime}}")
    ## Need to save the name of the built package
    state$plan$file[pkgidx] <- worker$process$get_built_file()
  } else {
    alert("danger", "Failed to build {pkg {pkg}} \\
           {version {version}} {timestamp {ptime}}")
  }
  update_progress_bar(state, 1L)

  state$plan$build_done[[pkgidx]] <- TRUE
  state$plan$build_time[[pkgidx]] <- time
  state$plan$build_error[[pkgidx]] <- ! success
  state$plan$build_stdout[[pkgidx]] <- worker$stdout
  state$plan$build_stderr[[pkgidx]] <- worker$stderr
  state$plan$worker_id[[pkgidx]] <- NA_character_

  if (!success) {
    abort("Failed to build source package {pkg}.")
  }

  state
}

installed_note <- function(pkg) {

  standard_note <- function() {
    if (pkg$type %in% c("cran", "standard")) {
      ""
    } else {
      paste0("(", pkg$type, ")")
    }
  }

  github_note <- function() {
    meta <- pkg$metadata[[1]]
    paste0("(github::", meta[["RemoteUsername"]], "/", meta[["RemoteRepo"]],
           "@", substr(meta[["RemoteSha"]], 1, 7), ")")
  }

  switch(
    pkg$type,
    cran = "",
    bioc = "(BioC)",
    standard = standard_note(),
    local = "(local)",
    github = github_note()
  )
}

stop_task_install <- function(state, worker) {

  ## TODO: make sure the install status is non-zero on exit
  success <- worker$process$get_exit_status() == 0

  pkgidx <- worker$task$args$pkgidx
  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]
  time <- Sys.time() - state$plan$install_time[[pkgidx]]
  ptime <- prettyunits::pretty_sec(as.numeric(time, units = "secs"))
  note <- installed_note(state$plan[pkgidx,])

  if (success) {
    alert("success", "Installed {pkg {pkg}} \\
             {version {version}} {note} {timestamp {ptime}}")
  } else {
    alert("danger", "Failed to install  {pkg pkg}} {version {version}}")
  }
  update_progress_bar(state, 1L)

  state$plan$install_done[[pkgidx]] <- TRUE
  state$plan$install_time[[pkgidx]] <- time
  state$plan$install_error[[pkgidx]] <- ! success
  state$plan$install_stdout[[pkgidx]] <- worker$stdout
  state$plan$install_stderr[[pkgidx]] <- worker$stderr
  state$plan$worker_id[[pkgidx]] <- NA_character_

  if (!success) {
    abort("Failed to install binary package {pkg}.")
  }

  ## Need to remove from the dependency list
  state$plan$deps_left <- lapply(state$plan$deps_left, setdiff, pkg)

  state
}

create_install_result <-  function(state) {
  result <- state$plan
  class(result) <- c("pkginstall_result", class(result))
  result
}

print.pkginstall_result <- function(x, ...) {
  newly <- sum(x$lib_status == "new")
  upd   <- sum(x$lib_status == "update")
  noupd <- sum(x$lib_status == "no-update")
  curr  <- sum(x$lib_status == "current")
  if (newly) cat("Installed: ",  newly, "\n", sep = "")
  if (upd)   cat("Updated: ",    upd,   "\n", sep = "")
  if (noupd) cat("Not updated:", noupd, "\n", sep = "")
  if (curr)  cat("Current: ",    curr,  "\n", sep = "")

  ## TODO
  build_time <- sum(unlist(x$build_time), na.rm = TRUE)
  inst_time <- sum(unlist(x$install_time), na.rm = TRUE)

  cat("Build time:  ", prettyunits::pretty_sec(build_time), "\n", sep = "")
  cat("Intall time: ", prettyunits::pretty_sec(inst_time), "\n", sep = "")

  invisible(x)
}

kill_all_processes <- function(state) {
  alive <- FALSE
  for (i in seq_along(state$workers)) {
    proc <- state$workers[[i]]$process
    proc$signal(tools::SIGINT)
    alive <- alive || proc$is_alive()
  }

  if (alive) {
    for (i in seq_along(state$workers)) {
      proc <- state$workers[[i]]$process
      proc$wait(200)
      proc$kill_tree()
    }
  }
}

#' Install a R binary package
#'
#' @param filename filename of built binary package to install
#' @param lib library to install packages into
#' @param metadata Named character vector of metadata entries to be added
#'   to the \code{DESCRIPTION} after installation.
#' @param quiet Whether to suppress console output.
#' @noRd

install_binary <- function(filename, lib = .libPaths()[[1L]],
                           metadata = NULL, quiet = NULL) {

  stopifnot(
    is_string(filename), file.exists(filename),
    is_string(lib),
    all_named(metadata),
    is.null(quiet) || is_flag(quiet))

  quiet <- quiet %||% ! is_verbose()

  px <- make_install_process(filename, lib = lib, metadata = metadata)
  stdout <- ""
  stderr <- ""

  bar <- cliapp::cli_progress_bar(
      format = paste0(":spin Installing ", filename))

  repeat {
    px$poll_io(100)
    if (!quiet) bar$tick(0)
    stdout <- paste0(stdout, px$read_output())
    stderr <- paste0(stderr, px$read_error())
    if (!px$is_alive() &&
        !px$is_incomplete_output() && !px$is_incomplete_error()) {
      break
    }
  }

  if (!quiet) bar$terminate()
  if (px$get_exit_status() != 0) {
    stop("Package installation failed\n", stderr)
  }

  cliapp::cli_alert_success(paste0("Installed ", filename))

  invisible(px$get_result())
}

install_extracted_binary <- function(filename, lib_cache, pkg_cache, lib,
                                     metadata, now) {

  pkg <- verify_extracted_package(filename, pkg_cache)
  add_metadata(pkg$path, metadata)
  pkg_name <- pkg$name

  lockfile <- lock_cache(lib_cache, pkg_name, getOption("install.lock"))
  on.exit(filelock::unlock(lockfile), add = TRUE)

  installed_path <- file.path(lib, pkg_name)
  if (file.exists(installed_path)) {
    # First move the existing library (which still works even if a process has
    # the DLL open), then try to delete it, which may fail if another process
    # has the file open.
    move_to <- file.path(create_temp_dir(), pkg_name)
    ret <- file.rename(installed_path, move_to)
    if (!ret) {
      abort(type = "filesystem",
        "Failed to move installed package at {installed_path}",
        package = pkg_name)
    }
    ret <- unlink(move_to, recursive = TRUE, force = TRUE)
    if (ret != 0) {
      warn(type = "filesystem",
        "Failed to remove installed package at {move_to}",
        package = pkg_name)
    }
  }
  ret <- file.rename(pkg$path, installed_path)
  if (!ret) {
    abort(type = "filesystem",
      "Unable to move package from {pkg$path} to {installed_path}",
      package = pkg_name)
  }

  signalCondition(
    cnd("pkginstall_installed",
      package = pkg_name, path = installed_path, time = Sys.time() - now, type = "binary"))

  installed_path
}

add_metadata <- function(pkg_path, metadata) {
  if (!length(metadata)) return()

  ## During installation, the DESCRIPTION file is read and an package.rds
  ## file created with most of the information from the DESCRIPTION file.
  ## Functions that read package metadata may use either the DESCRIPTION
  ## file or the package.rds file, therefore we attempt to modify both of
  ## them, and return an error if neither one exists.

  source_desc <- file.path(pkg_path, "DESCRIPTION")
  binary_desc <- file.path(pkg_path, "Meta", "package.rds")
  if (file.exists(source_desc)) {
    do.call(desc::desc_set, c(as.list(metadata), list(file = source_desc)))
  }

  if (file.exists(binary_desc)) {
    pkg_desc <- base::readRDS(binary_desc)
    desc <- as.list(pkg_desc$DESCRIPTION)
    desc <- utils::modifyList(desc, as.list(metadata))
    pkg_desc$DESCRIPTION <- stats::setNames(as.character(desc), names(desc))
    base::saveRDS(pkg_desc, binary_desc)
  }

  if (!file.exists(source_desc) && !file.exists(binary_desc)) {
    stop("No DESCRIPTION found!", call. = FALSE)
  }
}

make_install_process <- function(filename, lib = .libPaths()[[1L]],
                                 metadata = NULL) {
  filename; lib; metadata

  now <- Sys.time()

  type <- detect_package_archive_type(filename)
  if (type == "unknown") {
    abort(type = "invalid_input",
          "Cannot extract {filename}, unknown archive type?")
  }

  lib_cache <- library_cache(lib)
  mkdirp(pkg_cache <- tempfile(tmpdir = lib_cache))

  ppfun <- function() {
    install_extracted_binary(filename, lib_cache, pkg_cache, lib, metadata,
                             now)
  }

  p <- if (type == "zip") {
    make_unzip_process(filename, exdir = pkg_cache, post_process = ppfun)
  } else {
    ## TODO: we already know the package type, no need to detect again
    make_untar_process(filename, exdir = pkg_cache, post_process = ppfun)
  }

  reg.finalizer(p, function(...) unlink(pkg_cache, recursive = TRUE),
                onexit = TRUE)

  p
}

#' Intall Packages from Local Files
#'
#' Provides a replacement for `utils::install.packages(repo = NULL)`.
#' I.e. it builds binary packages from source packages, and extracts the
#' compressed archives into the package library.
#'
#' @section Features:
#'
#' Compared to `utils::install.packages()` it
#'
#' - Has robust support for installing packages in parallel.
#' - Fails immediately when the first package fails when installing
#'   multiple packages, rather than returning a warning.
#' - Uses the same code paths on all platforms, rather than similar (but
#'   not identical) code paths.
#' - Succeeds or fails atomically. Either the complete package is installed
#'   or it fails with an informative error message.
#' - Has additional tests for package validity before installing
#' - Always uses per-package lock files, to protect against simultaneous
#'   installation.
#' - Has a robust set of tests, to ensure correctness and ease debugging
#'   installation issues.
#'
#' @section Locking:
#'
#' pkginstall uses the `install.lock` option. If this is set to `FALSE`,
#' then no locking is performed. For all other values (including if the
#' option is unset or is `NULL`), per-package lock files are used, via the
#' filelock package.
#' @noRd
#' 
"_PACKAGE"

pkg_data <- new.env()

progress_chars <- function() {
  if (is.null(pkg_data$chars)) {
    if (cli::is_utf8_output()) {
      pkg_data$chars <- list(
        build = "\U0001f4e6",
        inst = "\u2705",
        lpar = "\u2e28",
        rpar = "\u2e29",
        fill = "\u2588",
        half = "\u2592"

      )
    } else {
      pkg_data$chars <- list(
        build = crayon::bgGreen(" B "),
        inst = crayon::bgGreen(" I "),
        lpar = "(",
        rpar = ")",
        fill = "#",
        half = "-"
      )
    }
  }

  pkg_data$chars
}

alert <- function(type, msg, .envir = parent.frame()) {
  if (!is_verbose()) return()
  switch (
    type,
    success = cliapp::cli_alert_success(msg, .envir = .envir),
    info = cliapp::cli_alert_info(msg, .envir = .envir),
    warning = cliapp::cli_alert_warning(msg, .envir = .envir),
    danger = cliapp::cli_alert_danger(msg, .envir = .envir)
  )
}

create_progress_bar <- function(state) {
  if (!is_verbose()) return()
  pkg_data$spinner <- cli::get_spinner()
  pkg_data$spinner_state <- 1L

  cliapp::cli_progress_bar(
    format = ":xbar ETA :eta | :xbuilt | :xinst | :xmsg",
    total = sum(!state$plan$build_done) + sum(!state$plan$install_done),
    force = TRUE
  )
}

update_progress_bar <- function(state, tick = 0) {

  if (!is_verbose()) return()

  plan <- state$plan
  total <- nrow(plan)
  installed <- sum(plan$install_done)
  built <- sum(plan$build_done)

  building <- sum(buildingl <- !plan$build_done & !is.na(plan$worker_id))
  installing <- sum(!buildingl & !is.na(plan$worker_id))

  chars <- progress_chars()
  tokens <- list(
    xbar = make_bar_pkginstall(installed / total, built/total, width =  15),
    xbuilt = make_progress_block(chars$build, built, total, building),
    xinst = make_progress_block(chars$inst, installed, total, installing),
    xmsg = make_trailing_progress_msg(state)
  )

  state$progress$tick(tick, tokens = tokens)
}

## p1 <= p2 must hold

make_bar_pkginstall <- function(p1, p2, width) {
  width <- width - 2L

  w1 <- if (isTRUE(all.equal(p1, 1))) width else trunc(width * p1)
  w2 <- if (isTRUE(all.equal(p2, 1))) width - w1 else trunc(width * (p2 - p1))

  chars <- progress_chars()
  p1chars <- rep(chars$fill, w1)
  p2chars <- rep(chars$half, w2)
  xchars <- rep(" ", max(width - w1 - w2, 0))
  bar <- paste(
    c(chars$lpar, p1chars, p2chars, xchars, chars$rpar), collapse = "")

  ## This is a workaround for an RStudio bug:
  ## https://github.com/r-lib/pkginstall/issues/42
  if (Sys.getenv("RSTUDIO", "") == "" ||
      Sys.getenv("RSTUDIO_TERM", "") != "") {
    crayon::green(bar)
  } else {
    bar
  }
}

make_progress_block <- function(sym, done, total, prog) {
  spin <- pkg_data$spinner$frames[[pkg_data$spinner_state]]
  pkg_data$spinner_state <-
    pkg_data$spinner_state %% length(pkg_data$spinner$frames) + 1L
  paste0(
    sym, "  ",
    done, "/", total,
    if (prog) paste0(" ", spin, " ", prog) else "    "
  )
}

done_progress_bar <- function(state) {
  if (!is_verbose()) return()
  state$progress$terminate()
}

make_trailing_progress_msg <- function(state) {
  working <- !is.na(state$plan$worker_id)
  installing <- state$plan$build_done & working
  building <- !state$plan$build_done & working

  building_pkgs <- paste(state$plan$package[building], collapse = ", ")
  installing_pkgs <- paste(state$plan$package[installing], collapse = ", ")

  paste0(
    if (any(building)) paste0("building ", building_pkgs),
    if (any(building) && any(installing)) ", ",
    if (any(installing)) paste0("installing ", installing_pkgs)
  )
}

#' Create a tar background process
#'
#' Use an external tar program, if there is a working one, otherwise use
#' the internal implementation.
#'
#' When using the internal implementation, we need to start another R
#' process.
#'
#' @param tarfile Tar file.
#' @param files Files or regular expressions to set what to extract. if
#'   `NULL` then everything is extracted.
#' @param exdir Where to extract the archive. It must exist.
#' @param restore_times Whether to restore file modification times.
#' @param post_process Function to call after the extraction.
#' @return The [callr::process] object.
#' @keywords internal
#' @noRd

make_untar_process <- function(tarfile, files = NULL, exdir = ".",
                               restore_times = TRUE, post_process = NULL) {
  internal <- suppressWarnings(need_internal_tar())
  if (internal) {
    r_untar_process()$new(tarfile, files, exdir, restore_times,
                          post_process = post_process)
  } else {
    external_untar_process()$new(tarfile, files, exdir, restore_times,
                                 post_process = post_process)
  }
}

#' Check if we need to use R's internal tar implementation
#'
#' This is slow, because we need to start an R child process, and the
#' implementation is also very slow. So it is better to use an extranl tar
#' program, if we can. We test this by trying to uncompress a .tar.gz
#' archive using the external program. The name of the tar program is
#' taken from the `TAR` environment variable, if this is unset then `tar`
#' is used.
#'
#' @return Whether we need to use the internal tar implementation.
#' @keywords internal
#' @noRd

need_internal_tar <- local({
  internal <- NULL
  function() {
    if (!is.null(internal)) return(internal)

    mkdirp(tmp <- tempfile())
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    tarfile <- system.file(package = .packageName, "tools", "pkg_1.0.0.tgz")

    tryCatch(
      p <- external_untar_process()$new(tarfile, exdir = tmp),
      error = function(e) {
        internal <<- TRUE
      }
    )
    if (!is.null(internal)) return(internal)

    p$wait(timeout = 2000)
    p$kill()
    internal <<- p$get_exit_status() != 0 ||
      !file.exists(file.path(tmp, "pkg", "DESCRIPTION"))
    internal
  }
})

external_untar_process <- function() {
  R6::R6Class(
    "external_untar_process",
    inherit = processx::process,
  
    public = list(
      initialize = function(tarfile, files = NULL, exdir = ".",
                            restore_times = TRUE,
                            tar = Sys.getenv("TAR", "tar"),
                            post_process = NULL)
        eup_init(self, private, super, tarfile, files, exdir,
                 restore_times, tar, post_process)
    ),
    
    private = list(
      options = NULL
    )
  )
}

r_untar_process <- function() {
  R6::R6Class(
    "r_untar_process",
    inherit = callr::r_process,

    public = list(
      initialize = function(tarfile, files = NULL, exdir = ".",
                            restore_times = TRUE, post_process = NULL)
        runtar_init(self, private, super, tarfile, files, exdir,
                    restore_times, tar, post_process)
    ),
    
    private = list(
      options = NULL
    )
  )
}
  
eup_init <- function(self, private, super, tarfile, files, exdir,
                     restore_times, tar, post_process) {

  private$options <- list(
    tarfile = normalizePath(tarfile),
    files = files,
    exdir = exdir,
    restore_times = restore_times,
    tar = tar)

  private$options$args <- eup_get_args(private$options)
  super$initialize(tar, private$options$args, post_process = post_process,
                   stdout = "|", stderr = "|")
  invisible(self)
}

eup_get_args <- function(options) {
  c(
    "-x", "-f", options$tarfile,
    "-C", options$exdir,
    get_untar_decompress_arg(options$tarfile),
    if (! options$restore_times) "-m",
    options$files
  )
}

get_untar_decompress_arg <- function(tarfile) {
  type <- detect_package_archive_type(tarfile)
  switch(
    type,
    "gzip" = "-z",
    "bzip2" = "-j",
    "xz" = "-J",
    "zip" = stop("Not a tar file, looks like a zip file"),
    "unknown" = character()
  )
}

detect_package_archive_type <- function(file) {
  buf <- readBin(file, what = "raw", n = 6)
  if (is_gzip(buf)) {
    "gzip"
  } else if (is_zip(buf)) {
    "zip"
  } else if (is_bzip2(buf)) {
    "bzip2"
  } else if (is_xz(buf)) {
    "xz"
  } else {
    "unknown"
  }
}

is_gzip <- function(buf) {
  if (!is.raw(buf)) buf <- readBin(buf, what = "raw", n = 3)
  length(buf) >= 3 &&
    buf[1] == 0x1f &&
    buf[2] == 0x8b &&
    buf[3] == 0x08
}

is_bzip2 <- function(buf) {
  if (!is.raw(buf)) buf <- readBin(buf, what = "raw", n = 3)
  length(buf) >= 3 &&
    buf[1] == 0x42 &&
    buf[2] == 0x5a &&
    buf[3] == 0x68
}

is_xz <- function(buf) {
  if (!is.raw(buf)) buf <- readBin(buf, what = "raw", n = 6)
  length(buf) >= 6 &&
    buf[1] == 0xFD &&
    buf[2] == 0x37 &&
    buf[3] == 0x7A &&
    buf[4] == 0x58 &&
    buf[5] == 0x5A &&
    buf[6] == 0x00
}

is_zip <- function(buf) {
  if (!is.raw(buf)) buf <- readBin(buf, what = "raw", n = 4)
  length(buf) >= 4 &&
    buf[1] == 0x50 &&
    buf[2] == 0x4b &&
    (buf[3] == 0x03 || buf[3] == 0x05 || buf[5] == 0x07) &&
    (buf[4] == 0x04 || buf[4] == 0x06 || buf[4] == 0x08)
}

runtar_init <- function(self, private, super, tarfile, files, exdir,
                        restore_times, tar, post_process) {

  options <- list(
    tarfile = normalizePath(tarfile),
    files = files,
    exdir = exdir,
    restore_times = restore_times,
    tar = tar,
    post_process = post_process)

  process_options <- callr::r_process_options()
  process_options$func <- function(options) {
    # nocov start
    ret <- utils::untar(
      tarfile = options$tarfile,
      files = options$files,
      list = FALSE,
      exdir = options$exdir,
      compressed = NA,
      restore_times = options$restore_times,
      tar = "internal"
    )

    if (!is.null(options$post_process)) options$post_process() else ret
    # nocov end
  }
  process_options$args <- list(options = options)
  super$initialize(process_options)
}

collapse_quote_transformer <- function(code, envir) {
  collapse_re <- "[*]$"
  quote_re <- "^[|]"
  should_collapse <- grepl(collapse_re, code)
  should_quote <- !grepl(quote_re, code)
  code <- sub(collapse_re, "",
    sub(quote_re, "", code))
  res <- eval(parse(text = code, keep.source = FALSE), envir = envir)
  if (should_quote) {
    res <- glue::single_quote(res)
  }
  if (should_collapse) {
    res <- glue::glue_collapse(res, sep = ", ", last = " and ")
  }
  res
}

abort <- function(msg, type = NULL, ..., .envir = parent.frame()) {
  stop(
    error_cnd(
      .subclass = type, ...,
      message = glue::glue(msg,
        .envir = parent.frame(),
        .transformer = collapse_quote_transformer)
      ))
}

warn <- function(msg, type = NULL, ..., .envir = parent.frame()) {
  warning(
    warning_cnd(
      .subclass = type, ...,
      message = glue::glue(msg,
        .envir = parent.frame(),
        .transformer = collapse_quote_transformer)
      ))
}

is_loaded <- function(package) {
  package %in% loadedNamespaces()
}

create_temp_dir <- function(..., tmpdir = tempdir()) {
  f <- tempfile(tmpdir = tmpdir, ...)
  dir.create(f)
  f
}

library_cache <- function(lib) {
  lib_cache <- file.path(lib, "_cache")
  dir.create(lib_cache, recursive = TRUE, showWarnings = FALSE)
  lib_cache
}

lock_cache <- function(cache, pkg_name, lock = TRUE) {
  use_lock <- !identical(lock, FALSE)
  my_lock <- NULL
  if (use_lock) {
    lockfile <- file.path(cache, glue::glue("{pkg_name}.lock"))
    # TODO: timeout and fail?
    my_lock <- filelock::lock(lockfile)
  }
  my_lock
}

unlock <- function(lock) {
  if (is.null(lock)) {
    return()
  }
  filelock::unlock(lock)
}


sysname <- function() {
  res <- tolower(Sys.info()[["sysname"]])
  map <- c(darwin = "mac", "sunos" = "solaris")[res]
  res[!is.na(map)] <- map
  res
}

map_lgl <- get("map_lgl", asNamespace("rlang"))

map_chr <- get("map_chr", asNamespace("rlang"))

map_int <- get("map_int", asNamespace("rlang"))

rep_list <- function(n, expr) {
  lapply(integer(n), eval.parent(substitute(function(...) expr)))
}

drop_nulls <- function(x) {
  is_null <- map_lgl(x, is.null)
  x[!is_null]
}

cut_into_lines <- function(x) {
  x <- do.call(paste0, as.list(x))
  x <- gsub("\r\n", "\n", x, fixed = TRUE)
  x <- strsplit(x, "\n", fixed = TRUE)[[1]]
  if (length(x)) x else ""
}

is_count <- function(x, min = 0L)  {
  is.numeric(x) && length(x) == 1 && !is.na(x) &&
    as.integer(x) == x && x >= min
}

verify_extracted_package <- function(filename, parent_path) {

  pkg_name <- dir(parent_path)
  pkg_path <- file.path(parent_path, pkg_name)

  if (length(pkg_name) == 0) {
    abort(type = "invalid_input",
      "{filename} is not a valid R package, it is an empty archive")

  } else if (length(pkg_name) > 1) {
    abort(type = "invalid_input",
      "{filename} is not a valid R package, it should contain a
      single directory")
  }

  rel_package_files <- c(
    file.path(pkg_name, "Meta", "package.rds"),
    file.path(pkg_name, "DESCRIPTION")
  )
  package_files <- file.path(parent_path, rel_package_files)

  has_files <- file.exists(package_files)
  if (!all(has_files)) {
    miss <- rel_package_files[! has_files]
    abort(type = "invalid_input",
      "{filename} is not a valid binary, it does not contain {miss*}.",
      package = pkg_name)
  }

  rel_dsc_file <- file.path(pkg_name, "DESCRIPTION")
  dsc_file <- file.path(pkg_path, "DESCRIPTION")
  dsc <- tryCatch(
    desc::desc(dsc_file),
    error = function(e) {
      abort(type = "invalid_input",
        "{filename} is not a valid binary, invalid {rel_dsc_file}.",
        package = pkg_name)
    }
  )

  if (!length(dsc$fields())) {
    abort(type = "invalid_input",
      "{filename} is not a valid binary, {rel_dsc_file} is empty.",
      package = pkg_name)
  }

  dsc_pkg <- dsc$get("Package")
  if (is.na(dsc_pkg)) {
    abort(type = "invalid_input",
      "{filename} has no `Package` entry in {rel_dsc_file}",
      package = pkg_name)
  }

  if (pkg_name != str_trim(dsc_pkg[[1]])) {
    abort(type = "invalid_input",
      "{filename} is not a valid binary, package name mismatch in
      archive and in {rel_dsc_file}",
      package = pkg_name)
  }

  if (is.na(dsc$get("Built"))) {
    abort(type = "invalid_input",
      "{filename} is not a valid binary, no 'Built' entry in {rel_dsc_file}.",
      package = pkg_name)
  }

  list(name = pkg_name, path = pkg_path, desc = dsc)
}

make_unzip_process <- function(zipfile, files = NULL, exdir = ".",
                               restore_times = TRUE, post_process = NULL) {
  r_unzip_process()$new(zipfile, files, exdir, restore_times, post_process)
}

r_unzip_process <- function() {
  R6::R6Class(
    "r_unzip_proces",
    inherit = callr::r_process,

    public = list(
      initialize = function(zipfile, files = NULL, exdir = ".",
                            restore_times = TRUE, post_process = NULL)
        runzip_init(self, private, super, zipfile, files, exdir,
                    restore_times, post_process)
    ),
    
    private = list(
      options = NULL
    )
  )
}

runzip_init <- function(self, private, super, zipfile, files, exdir,
                        restore_times, post_process) {

  options <- list(
    zipfile = normalizePath(zipfile),
    files = files,
    exdir = exdir,
    restore_times = restore_times,
    post_process = post_process)

  process_options <- callr::r_process_options()
  process_options$func <- function(options) {
    # nocov start
    ret <- utils::unzip(
      zipfile = options$zipfile,
      files = options$files,
      list = FALSE,
      exdir = options$exdir,
      setTimes = options$restore_times,
      unzip = "internal"
    )

    if (!is.null(options$post_process)) options$post_process() else ret
    # nocov end
  }
  process_options$args <- list(options = options)
  super$initialize(process_options)
}

cnd <- function(.subclass, ..., message = "") {
  structure(
    list(message = message, ...),
    class = c(.subclass, "condition"))
}

error_cnd <- function(.subclass = NULL, ..., message = "", trace = NULL) {
  structure(
    list(message = message, trace = trace, ...),
    class = c(.subclass, "error", "condition"))
}

warning_cnd <- function(.subclass = NULL, ..., message = "") {
  structure(
    list(message = message, ...),
    class = c(.subclass, "warning", "condition"))
}

globalVariables(c("self", "private", "super", "tar"))
