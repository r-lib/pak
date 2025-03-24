
#' Installation plans
#'
#' An installation plan contains all data that is needed to install a
#' set of package files. It is usually created from an
#' [installation proposal][pkg_installation_proposal] with
#' [solving][pkg_solution] the dependencies and [downloading][pkg_downloads]
#' the package files.
#'
#' It is also possible to create an installation plan a different way. An
#' installation plan object must be a data frame, with at least the
#' following columns:
#'
#' * `package`: The name of the package.
#' * `type`: The type of the [package reference][pkg_refs].
#' * `binary`: Whether the package is a binary package.
#' * `file`: Full path to the package file or directory.
#' * `dependencies`: A list column that lists the names of the dependent
#'   packages for each package.
#' * `needscompilation`: Whether the package needs compilation. This should
#'   be `FALSE` for binary packages.
#'
#' For installation plans created via [pkg_installation_proposal], the plan
#' contains all columns from [`pkg_download_result`][pkg_download_result]
#' objects, and some additional ones:
#'
#' * `library`: the library the package is supposed to be installed to.
#' * `direct`: whether the package was directly requested or it is
#'   installed as a dependency.
#' * vignettes: whether the vignettes need to be (re)built.
#' * `packaged`: whether `R CMD build` was already called for the package.
#'
#' @seealso [pkg_installation_proposal] to create install plans,
#' [install_package_plan()] to install plans from any source.
#'
#' @examples
#' \dontrun{
#' pdi <- new_pkg_installation_proposal(
#'   "pak",
#'   config = list(library = tempfile())
#' )
#' pdi$resolve()
#' pdi$solve()
#' pdi$download()
#' pdi$get_install_plan()
#' }
#'
#' @name install_plans
NULL

#' Perform a package installation plan
#'
#' See ['Installation plans'][install_plans] for the details and the format.
#'
#' @param plan Package plan object, a data frame, see
#' ['Installation plans'][install_plans] for the format.
#' @param lib Library directory to install to.
#' @param num_workers Number of worker processes to use.
#' @param cache Package cache to use, or `NULL`.
#' @return Information about the installation process.
#'
#' @export

install_package_plan <- function(plan, lib = .libPaths()[[1]],
                                 num_workers = 1, cache = NULL) {

  start <- Sys.time()
  cli::ansi_hide_cursor()
  on.exit(cli::ansi_show_cursor())

  cli::cli_div(
    theme = list(".timestamp" = list(
      color = "darkgrey",
      before = "(",
      after = ")"
    ))
  )

  required_columns <- c(
    "type", "binary", "dependencies", "file", "needscompilation", "package"
  )
  assert_that(
    inherits(plan, "data.frame"),
    all(required_columns %in% colnames(plan)),
    is_string(lib),
    is_count(num_workers, min = 1L)
  )

  if (! "vignettes" %in% colnames(plan)) plan$vignettes <- FALSE
  if (! "metadata" %in% colnames(plan)) {
    plan$metadata <- replicate(nrow(plan), character(), simplify = FALSE)
  }
  if (! "packaged" %in% colnames(plan)) plan$packaged <- TRUE

  plan <- add_recursive_dependencies(plan)

  config <- list(
    lib = lib,
    num_workers = num_workers,
    show_time = tolower(Sys.getenv("PKG_OMIT_TIMES")) != "true"
  )
  state <- make_start_state(plan, config)
  state$cache <- cache
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

# See https://github.com/r-lib/pkgdepends/issues/160
# for why this is needed. We only need to do it for the packages that
# are built from source, binaries can always be extracted.

add_recursive_dependencies <- function(plan) {
  # If all packages are source packages then there is nothing to do,
  # we cannot get into a bad situation. Similarly if we only have
  # binary packages.
  if (all(plan$binary) || all(!plan$binary)) return(plan)

  # Otherwise for every source package, we need to add its recursive
  # dependencies.
  idx  <- structure(seq_len(nrow(plan)), names = plan$package)
  xdps <- structure(plan$dependencies, names = plan$package)
  done <- structure(logical(nrow(plan)), names = plan$package)
  srcidx <- which(!plan$binary)

  do <- function(i) {
    if (done[[i]]) return()
    miss <- idx[names(!done[xdps[[i]]])]
    done[[i]] <<- TRUE
    for (m in miss) do(m)
    xdps[[i]] <<- unique(c(xdps[[i]], unlist(xdps[xdps[[i]]])))
  }

  for (i in srcidx) do(i)

  plan$dependencies[srcidx] <- unname(xdps[srcidx])
  plan
}

make_start_state <- function(plan, config) {

  ## We store the data about build and installation here
  install_cols <- data.frame(
    stringsAsFactors = FALSE,
    row.names = NULL,
    package_done = plan$packaged,
    package_time = I(rep_list(nrow(plan), as.POSIXct(NA))),
    package_error = I(rep_list(nrow(plan), list())),
    package_stdout = I(rep_list(nrow(plan), character())),
    build_done = (plan$type %in% c("deps", "installed")) | plan$binary,
    build_time = I(rep_list(nrow(plan), as.POSIXct(NA))),
    build_error = I(rep_list(nrow(plan), list())),
    build_stdout = I(rep_list(nrow(plan), character())),
    install_done = plan$type %in% c("deps", "installed"),
    install_time = I(rep_list(nrow(plan), as.POSIXct(NA))),
    install_error = I(rep_list(nrow(plan), list())),
    install_stdout = I(rep_list(nrow(plan), character())),
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
    vlapply(res, function(x) "ready" %in% x)

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

  ## Read out stdout. If process is done, then read out all
  if (proc$is_alive()) {
    deadline <- Sys.time() + as.difftime(1, units = "secs")
    while (Sys.time() < deadline && proc$poll_io(0)[["output"]] == "ready") {
      state$workers[[evidx]]$stdout <-
        c(state$workers[[evidx]]$stdout, out <- proc$read_output(n = 10000))
    }
  } else {
    state$workers[[evidx]]$stdout <-
      c(state$workers[[evidx]]$stdout, out <- proc$read_all_output())
  }

  ## If there is still output, then wait a bit more
  if (proc$is_alive() || proc$is_incomplete_output()) {
    return(state)
  }

  ## Otherwise we are done. Remove worker
  worker <- state$workers[[evidx]]
  state$workers[evidx] <- list(NULL)

  if (is.function(proc$get_result)) proc$get_result()

  ## Cut stdout to lines
  worker$stdout <- cut_into_lines(worker$stdout)

  ## Record what was done
  stop_task(state, worker)
}

select_next_task <- function(state) {

  ## Cannot run more workers?
  if (length(state$workers) >= state$config$num_workers) {
    return(task("idle"))
  }

  ## Can we select a package tree to build into a source package? Do that.
  can_package <- which(
    ! state$plan$package_done &
    viapply(state$plan$deps_left, length) == 0 &
    is.na(state$plan$worker_id))

  if (any(can_package)) {
    pkgidx <- can_package[1]
    return(task("package", pkgidx = pkgidx, phase = "uncompress"))
  }

  ## Can we select a source package build? Do that.
  can_build <- which(
    ! state$plan$build_done &
    viapply(state$plan$deps_left, length) == 0 &
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
    todo <- state$plan$package[!state$plan$install_done]
    throw(pkg_error(
      "Cannot select new package installation task.",
      i = "{length(todo)} package{?s} still waiting to install: {.pkg {todo}}.",
      i = msg_internal_error()
    ))
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

  } else if (task$name == "package") {
    start_task_package(state, task)

  } else if (task$name == "build") {
    start_task_build(state, task)

  } else if (task$name == "install") {
    start_task_install(state, task)

  } else {
    throw(pkg_error(
      "Unknown task: {.val {task$name}}.",
      i = msg_internal_error()
    ))
  }
}

get_worker_id <- (function() {
  id <- 0
  function() {
    id <<- id + 1
    as.character(id)
  }
})()

# TODO: test this on Windows
# nocov start

get_rtools_path <- function() {
  if (!is.null(pkgd_data$rtools_path)) return(pkgd_data$rtools_path)
  # no need to mess with Rtools on R >= 4.3.0, R puts Rtools on the PATH
  # the error message for missing Rtools will be worse, though :(
  if (getRversion() >= "4.3.0") {
    pkgd_data$rtools_path <- character()
  } else {
    pkgd_data$rtools_path <- pkgbuild::without_cache({
      if (suppressMessages(pkgbuild::has_rtools())) {
        gsub("/", "\\", pkgbuild::rtools_path(), fixed = TRUE)
      }
    })
  }
  pkgd_data$rtools_path
}

# nocov end

make_build_process <- function(path, pkg, tmp_dir, lib, vignettes,
                               needscompilation, binary, cmd_args, metadata = NULL) {

  # For windows, we need ensure the zip.exe bundled with the zip package is on the PATH
  # TODO: test this on Windows
  # nocov start
  if (is_windows()) {
    zip_tool_path <- asNamespace("zip")$get_tool("zip")
    rtools <- get_rtools_path()
    withr_local_path(c(dirname(zip_tool_path), rtools))
  }
  # nocov end

  # We also allow an extra subdirectory, e.g. in .tar.gz files downloaded
  # from GHA
  if (!file.exists(file.path(path, "DESCRIPTION")) &&
      length(subdir <- dir(path)) == 1) {
    path <- file.path(path, subdir)
  }
  if (!file.exists(file.path(path, "DESCRIPTION")) &&
      length(subdir <- dir(path)) == 1) {
    path <- file.path(path, subdir)
  }

  warn_for_long_paths(path, pkg)

  ## with_libpath() is needed for newer callr, which forces the current
  ## lib path in the child process.
  mkdirp(tmplib <- tempfile("pkg-lib"))
  # Otherwise it is loaded with a modified library path, so potentially from the
  # wrong place, especially in pak
  loadNamespace("pkgbuild")
  loadNamespace("desc")
  loadNamespace("callr")
  loadNamespace("processx")
  loadNamespace("cli")
  loadNamespace("ps")
  # loadNamespace("R6") # not needed, built time dependency

  add_metadata(path, metadata)

  withr_with_libpaths(c(tmplib, lib), action = "prefix",
    pkgbuild::pkgbuild_process$new(
      path, tmp_dir, binary = binary, vignettes = vignettes,
      needs_compilation = needscompilation, compile_attributes = FALSE,
      args = c("--no-lock", cmd_args, if (binary) sprintf("--library=%s", tmplib))
    )
  )
}

# TODO: test this, if possible
# nocov start

warn_for_long_paths <- function(path, pkg) {
  if (!is_windows()) return()
  pkg_paths <- dir(path, recursive = TRUE, full.names = TRUE)
  # No files here for R CMD INSTALL --build, path is a file there
  max_len <- max(c(0, nchar(pkg_paths)))
  if (max_len < 255) return(path)
  alert(
    "warning",
    paste0(
      "The {.pkg {pkg}} package has very long paths. The installation ",
      "will probably fail because most Windows versions do not support ",
      "long paths. Please tell the package authors about this."
    ),
    wrap = TRUE
  )
}

# nocov end

start_task_package <- function(state, task) {
  pkgidx <- task$args$pkgidx
  path <- state$plan$file[pkgidx]
  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]

  state$plan$package_time[[pkgidx]] <- Sys.time()
  alert("info", "Packaging {.pkg {pkg}} {.version {version}}")

  if (file.info(path)$isdir) {
    ## Just build tree_dir
    task$args$tree_dir <- path
    start_task_package_build(state, task)
  } else {
    ## Uncompress to tree_dir, then build it
    task$args$tree_dir <- file.path(tempdir(), "X", pkg)
    unlink(task$args$tree_dir, recursive = TRUE)
    start_task_package_uncompress(state, task)
  }
}

start_task_package_uncompress <- function(state, task) {
  pkgidx <- task$args$pkgidx
  path <- state$plan$file[pkgidx]

  needscompilation <- !identical(state$plan$needscompilation[pkgidx], "no")
  lib <- state$config$lib

  task$args$phase <- "uncompress"
  px <- make_uncompress_process(path, task$args$tree_dir)
  worker <- list(id = get_worker_id(), task = task, process = px,
                 stdout = character())
  state$workers <- c(
    state$workers, structure(list(worker), names = worker$id))
  state$plan$worker_id[pkgidx] <- worker$id
  state
}

start_task_package_build <- function(state, task) {
  pkgidx <- task$args$pkgidx
  pkg <- state$plan$package[pkgidx]

  # GH (and similar) packages might be in a subdirectory
  subdir <- c(state$plan$metadata[[pkgidx]]["RemoteSubdir"])[1]
  if (is.null(subdir) || is.na(subdir)) {
    subdir <- "."
  }

  # The actual package might be in a subdirectory of the tarball,
  # e.g. when the tree was downloaded from GitHub. So the package is
  # either in tree_dir/subdir or tree_dir/something/subdir
  root_dir <- task$args$tree_dir
  pkg_dir <- file.path(root_dir, subdir)
  dir_root_dir <- dir(root_dir)

  if (! "DESCRIPTION" %in% dir_root_dir && length(dir_root_dir) == 1) {
    pkg_dir <- file.path(root_dir, dir_root_dir, subdir)
  }

  vignettes <- state$plan$vignettes[pkgidx]
  needscompilation <- !identical(state$plan$needscompilation[pkgidx], "no")
  lib <- state$config$lib
  metadata <- state$plan$metadata[[pkgidx]]

  task$args$phase <- "build"
  px <- make_build_process(pkg_dir, pkg, create_temp_dir(), lib, vignettes,
                           needscompilation, binary = FALSE,
                           cmd_args = NULL, metadata = metadata)
  worker <- list(id = get_worker_id(), task = task, process = px,
                 stdout = character())
  state$workers <- c(
    state$workers, structure(list(worker), names = worker$id))
  state$plan$worker_id[pkgidx] <- worker$id
  state$plan$build_time[[pkgidx]] <- Sys.time()
  state
}

start_task_build <- function(state, task) {
  pkgidx <- task$args$pkgidx
  path <- state$plan$file[pkgidx]
  vignettes <- state$plan$vignettes[pkgidx]
  needscompilation <- !identical(state$plan$needscompilation[pkgidx], "no")
  tmp_dir <- create_temp_dir()
  lib <- state$config$lib

  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]
  alert("info", "Building {.pkg {pkg}} {.version {version}}")

  if ("install_args" %in% names(state$plan)) {
    cmd_args <- state$plan$install_args[pkgidx]
    if (identical(cmd_args, "")) cmd_args <- NULL
  } else {
    cmd_args <- NULL
  }
  px <- make_build_process(path, pkg, tmp_dir, lib, vignettes, needscompilation,
                           binary = TRUE, cmd_args = cmd_args)
  worker <- list(id = get_worker_id(), task = task, process = px,
                 stdout = character())
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
    stdout = character())

  state$workers <- c(
    state$workers, structure(list(worker), names = worker$id))
  state$plan$worker_id[pkgidx] <- worker$id
  state$plan$install_time[[pkgidx]] <- Sys.time()
  state
}

stop_task <- function(state, worker) {
  if (worker$task$name == "package") {
    stop_task_package(state, worker)

  } else if (worker$task$name == "build") {
    stop_task_build(state, worker)

  } else if (worker$task$name == "install") {
    stop_task_install(state, worker)

  } else {
    throw(pkg_error(
      "Unknown task: {.val {worker$task$name}}.",
      i = msg_internal_error()
    ))
  }
}

stop_task_package <- function(state, worker) {
  if (worker$task$args$phase == "uncompress") {
    stop_task_package_uncompress(state, worker)
  } else {
    stop_task_package_build(state, worker)
  }
}

stop_task_package_uncompress <- function(state, worker) {
  pkgidx <- worker$task$args$pkgidx
  success <- worker$process$get_exit_status() == 0

  if (!success) {
    pkg <- state$plan$package[pkgidx]
    version <- state$plan$version[pkgidx]
    time <- Sys.time() - state$plan$package_time[[pkgidx]]
    ptime <- format_time$pretty_sec(as.numeric(time, units = "secs"))
    alert("danger", "Failed to uncompress {.pkg {pkg}} {.version {version}}")
    update_progress_bar(state, 1L)

    state$plan$package_done[[pkgidx]] <- TRUE
    state$plan$package_time[[pkgidx]] <- time
    state$plan$package_error[[pkgidx]] <- ! success
    state$plan$package_stdout[[pkgidx]] <- worker$stdout
    state$plan$worker_id[[pkgidx]] <- NA_character_

    throw(pkg_error(
      "Failed to uncompress {.pkg {pkg}} from
      {.path {state$plan$file[[pkgidx]]}}.",
      .data = list(
        package = pkg,
        version = version,
        time = time,
        stdout = worker$stdout
      ),
      .class = "package_uncompress_error"
    ))
  }

  start_task_package_build(state, worker$task)
}

stop_task_package_build <- function(state, worker) {
  pkgidx <- worker$task$args$pkgidx
  success <- worker$process$get_exit_status() == 0

  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]
  time <- Sys.time() - state$plan$package_time[[pkgidx]]
  ptime <- format_time$pretty_sec(as.numeric(time, units = "secs"))

  if (success) {
    alert("success", paste0(
      "Packaged {.pkg {pkg}} {.version {version}}",
      if (isTRUE(state$config$show_time)) " {.timestamp {ptime}}"
    ))
    ## Need to save the name of the built package
    state$plan$file[pkgidx] <- worker$process$get_built_file()
  } else {
    alert("danger", "Failed to create source package {.pkg {pkg}} \\
           {.version {version}}")
    if (!identical(worker$stdout, "")) {
      cli::cli_h1("Standard output")
      cli::cli_verbatim(worker$stdout)
    } else {
      alert("info", "Standard output is empty")                     # nocov
    }
  }
  update_progress_bar(state, 1L)

  state$plan$package_done[[pkgidx]] <- TRUE
  state$plan$package_time[[pkgidx]] <- time
  state$plan$package_error[[pkgidx]] <- ! success
  state$plan$package_stdout[[pkgidx]] <- worker$stdout
  state$plan$worker_id[[pkgidx]] <- NA_character_

  if (!success) {
    throw(pkg_error(
      "Failed to create source package {.pkg {pkg}} from source tree at
      {.path {state$plan$file[[pkgidx]]}}",
      .data = list(
        package = pkg,
        version = version,
        stdout = worker$stdout,
        time = time
      ),
      .class = "package_packaging_error"
    ))
  }

  prms <- state$plan$params[[pkgidx]]
  if (!is.null(state$cache) && !is_true_param(prms, "nocache")) {
    tryCatch(
      state$cache$add(state$plan$file[pkgidx], state$plan$target[pkgidx],
                      package = pkg, version = version, built = TRUE,
                      sha256 = state$plan$extra[[pkgidx]]$remotesha,
                      vignettes = state$plan$vignettes[pkgidx],
                      platform = "source"),
      error = function(err) {
        alert("warning", "Failed to add {.pkg {pkg}} \\
               {.version {version}} to the cache")
      }
    )
  }

  state
}

stop_task_build <- function(state, worker) {

  ## TODO: make sure exit status is non-zero on build error!
  success <- worker$process$get_exit_status() == 0

  pkgidx <- worker$task$args$pkgidx
  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]
  time <- Sys.time() - state$plan$build_time[[pkgidx]]
  ptime <- format_time$pretty_sec(as.numeric(time, units = "secs"))
  prms <- state$plan$params[[pkgidx]]

  if (success) {
    alert("success", paste0(
      "Built {.pkg {pkg}} {.version {version}}",
      if (isTRUE(state$config$show_time)) " {.timestamp {ptime}}"
    ))
    ## Need to save the name of the built package
    state$plan$file[pkgidx] <- worker$process$get_built_file()
  } else {
    ignore_error <- is_true_param(prms, "ignore-build-errors")
    alert(
      if (ignore_error) "warning" else "danger",
      paste0(
        "Failed to build {.pkg {pkg}} {.version {version}}",
        if (isTRUE(state$config$show_time)) " {.timestamp {ptime}}"
      )
    )
  }
  update_progress_bar(state, 1L)

  state$plan$build_done[[pkgidx]] <- TRUE
  state$plan$build_time[[pkgidx]] <- time
  state$plan$build_stdout[[pkgidx]] <- worker$stdout
  state$plan$worker_id[[pkgidx]] <- NA_character_

  if (success) {
    # a bit silly, but for compatibility...
    state$plan$build_error[[pkgidx]] <- FALSE
  } else {
    build_error <- list(
      package = pkg,
      version = version,
      stdout = worker$stdout,
      time = time
    )
    state$plan$build_error[[pkgidx]] <- build_error
    if (ignore_error) {
      # upstream will probably fail as well, but march on, neverthelesss
      state$plan$install_done[[pkgidx]] <- TRUE
      ## Need to remove from the dependency list
      state$plan$deps_left <- lapply(state$plan$deps_left, setdiff, pkg)
    } else {
      throw(pkg_error(
        "Failed to build source package {.pkg {pkg}}.",
        .data = build_error,
        .class = "package_build_error"
      ))
    }
  }

  if (success && !is.null(state$cache) && !is_true_param(prms, "nocache")) {
    ptfm <- current_r_platform()
    rv <- current_r_version()
    target <- paste0(state$plan$target[pkgidx], "-", ptfm, "-", rv)
    tryCatch(
      state$cache$add(state$plan$file[pkgidx], target,
                      package = pkg, version = version, built = TRUE,
                      sha256 = state$plan$extra[[pkgidx]]$remotesha,
                      vignettes = state$plan$vignettes[pkgidx],
                      platform = ptfm, rversion = rv),
      error = function(err) {
        alert("warning", "Failed to add {.pkg {pkg}} \\
               {.version {version}} ({ptfm}) to the cache")
      }
    )
  }


  state
}

installed_note <- function(pkg) {
  github_note <- function() {
    meta <- pkg$metadata[[1]]
    paste0("(github::", meta[["RemoteUsername"]], "/", meta[["RemoteRepo"]],
           "@", substr(meta[["RemoteSha"]], 1, 7), ")")
  }

  gitlab_note <- function() {
    meta <- pkg$metadata[[1]]
    paste0(
      "(gitlab::", meta[["RemoteUsername"]], "/", meta[["RemoteRepo"]],
      "@", substr(meta[["RemoteSha"]], 1, 7), ")"
    )
  }

  git_note <- function() {
    meta <- pkg$metadata[[1]]
    paste0(
      "(git::",
      meta[["RemoteUrl"]],
      "@",
      substr(meta[["RemoteSha"]], 1, 7),
      ")"
    )
  }

  switch(
    pkg$type,
    cran = "",
    bioc = "(Bioconductor)",
    standard = "",
    local = "(local)",
    github = github_note(),
    gitlab = gitlab_note(),
    git = git_note()
  )
}

stop_task_install <- function(state, worker) {

  ## TODO: make sure the install status is non-zero on exit
  success <- worker$process$get_exit_status() == 0

  pkgidx <- worker$task$args$pkgidx
  pkg <- state$plan$package[pkgidx]
  version <- state$plan$version[pkgidx]
  time <- Sys.time() - state$plan$install_time[[pkgidx]]
  ptime <- format_time$pretty_sec(as.numeric(time, units = "secs"))
  note <- installed_note(state$plan[pkgidx,])

  if (success) {
    alert("success", paste0(
      "Installed {.pkg {pkg}} {.version {version}} {note}",
      if (isTRUE(state$config$show_time)) " {.timestamp {ptime}}"
    ))
  } else {
    alert("danger", "Failed to install {.pkg {pkg}} {.version {version}}")
  }
  update_progress_bar(state, 1L)

  state$plan$install_done[[pkgidx]] <- TRUE
  state$plan$install_time[[pkgidx]] <- time
  state$plan$install_error[[pkgidx]] <- ! success
  state$plan$install_stdout[[pkgidx]] <- worker$stdout
  state$plan$worker_id[[pkgidx]] <- NA_character_

  if (!success) {
    throw(pkg_error(
      "Failed to install binary package {.pkg {pkg}}.",
      .class = "package_install_error"
    ))
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

#' @export

print.pkginstall_result <- function(x, ...) {
  newly <- sum(x$lib_status == "new" & x$type != "deps")
  upd   <- sum(x$lib_status == "update")
  noupd <- sum(x$lib_status == "no-update")
  curr  <- sum(x$lib_status == "current")

  build_time <- sum(unlist(x$build_time), na.rm = TRUE)
  inst_time <- sum(unlist(x$install_time), na.rm = TRUE)

  res <- c(
    "Summary:",
    if (newly) paste0(emoji("sparkles", ""), " ", newly, " new"),
    if (upd)   paste0(emoji("rocket", ""), " ", upd, " updated"),
    if (noupd + curr) paste0(emoji("hand", ""), " ", noupd + curr, " kept"),
    if (! tolower(Sys.getenv("PKG_OMIT_TIMES")) == "true") {
      paste0("in ", format_time$pretty_sec(build_time + inst_time))
    }
  )

  cli::cli_alert_success(paste0(res, collapse = "  "))

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
      if (ps::ps_is_supported()) proc$kill_tree() else proc$kill()
    }
  }
}
