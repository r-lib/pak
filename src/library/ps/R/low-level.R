

#' Create a process handle
#'
#' @param pid Process id. Integer scalar. `NULL` means the current R
#'   process.
#' @param time Start time of the process. Usually `NULL` and ps will query
#'   the start time.
#' @return `ps_handle()` returns a process handle (class `ps_handle`).
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p

ps_handle <- function(pid = NULL, time = NULL) {
  if (!is.null(pid)) assert_pid(pid)
  if (!is.null(time)) assert_time(time)
  .Call(psll_handle, pid, time)
}

#' @rdname ps_handle
#' @export

as.character.ps_handle <- function(x, ...) {
  pieces <- .Call(psll_format, x)
  paste0("<ps::ps_handle> PID=", pieces[[2]], ", NAME=", pieces[[1]],
         ", AT=", format_unix_time(pieces[[3]]))
}

#' @param x Process handle.
#' @param ... Not used currently.
#'
#' @rdname ps_handle
#' @export

format.ps_handle <- function(x, ...) {
  as.character(x, ...)
}

#' @rdname ps_handle
#' @export

print.ps_handle <- function(x, ...)  {
  cat(format(x, ...),  "\n", sep = "")
  invisible(x)
}

#' Pid of a process handle
#'
#' This function works even if the process has already finished.
#'
#' @param p Process handle.
#' @return Process id.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_pid(p)
#' ps_pid(p) == Sys.getpid()

ps_pid <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_pid, p)
}

#' Start time of a process
#'
#' The pid and the start time pair serves as the identifier of the process,
#' as process ids might be reused, but the chance of starting two processes
#' with identical ids within the resolution of the timer is minimal.
#'
#' This function works even if the process has already finished.
#'
#' @param p Process handle.
#' @return `POSIXct` object, start time, in GMT.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_create_time(p)

ps_create_time <- function(p = ps_handle()) {
  assert_ps_handle(p)
  format_unix_time(.Call(psll_create_time, p))
}

#' Checks whether a process is running
#'
#' It returns `FALSE` if the process has already finished.
#'
#' It uses the start time of the process to work around pid reuse. I.e.
#  it returns the correct answer, even if the process has finished and
#  its pid was reused.
#'
#' @param p Process handle.
#' @return Logical scalar.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_is_running(p)

ps_is_running <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_is_running, p)
}

#' Parent pid or parent process of a process
#'
#' `ps_ppid()` returns the parent pid, `ps_parent()` returns a `ps_handle`
#' of the parent.
#'
#' On POSIX systems, if the parent process terminates, another process
#' (typically the pid 1 process) is marked as parent. `ps_ppid()` and
#' `ps_parent()` will return this process then.
#'
#' Both `ps_ppid()` and `ps_parent()` work for zombie processes.
#'
#' @param p Process handle.
#' @return `ps_ppid()` returns and integer scalar, the pid of the parent
#'   of `p`. `ps_parent()` returns a `ps_handle`.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_ppid(p)
#' ps_parent(p)

ps_ppid <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_ppid, p)
}

#' @rdname  ps_ppid
#' @export

ps_parent <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_parent, p)
}

#' Process name
#'
#' The name of the program, which is typically the name of the executable.
#'
#' On on Unix this can change, e.g. via an exec*() system call.
#'
#' `ps_name()` works on zombie processes.
#'
#' @param p Process handle.
#' @return Character scalar.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_name(p)
#' ps_exe(p)
#' ps_cmdline(p)

ps_name <- function(p = ps_handle()) {
  assert_ps_handle(p)
  n <- .Call(psll_name, p)
  if (nchar(n) >= 15) {
    ## On UNIX the name gets truncated to the first 15 characters.
    ## If it matches the first part of the cmdline we return that
    ## one instead because it's usually more explicative.
    ## Examples are "gnome-keyring-d" vs. "gnome-keyring-daemon".
    cmdline <- tryCatch(
      ps_cmdline(p),
      error = function(e) NULL
    )
    if (!is.null(cmdline)) {
      exname <- basename(cmdline[1])
      if (str_starts_with(exname, n)) n <- exname
    }
  }
  n
}

#' Full path of the executable of a process
#'
#' Path to the executable of the process. May also be an empty string or
#' `NA` if it cannot be determined.
#'
#' For a zombie process it throws a `zombie_process` error.
#'
#' @param p Process handle.
#' @return Character scalar.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_name(p)
#' ps_exe(p)
#' ps_cmdline(p)

ps_exe <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_exe, p)
}

#' Command line of the process
#'
#' Command line of the process, i.e. the executable and the command line
#' arguments, in a character vector. On Unix the program might change its
#' command line, and some programs actually do it.
#'
#' For a zombie process it throws a `zombie_process` error.
#'
#' @param p Process handle.
#' @return Character vector.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_name(p)
#' ps_exe(p)
#' ps_cmdline(p)

ps_cmdline <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_cmdline, p)
}

#' Current process status
#'
#' One of the following:
#' * `"idle"`: Process being created by fork, macOS only.
#' * `"running"`: Currently runnable on macOS and Windows. Actually
#'     running on Linux.
#' * `"sleeping"` Sleeping on a wait or poll.
#' * `"disk_sleep"` Uninterruptible sleep, waiting for an I/O operation
#'    (Linux only).
#' * `"stopped"` Stopped, either by a job control signal or because it
#'    is being traced.
#' * `"tracing_stop"` Stopped for tracing (Linux only).
#' * `"zombie"` Zombie. Finished, but parent has not read out the exit
#'    status yet.
#' * `"dead"` Should never be seen (Linux).
#' * `"wake_kill"` Received fatal signal (Linux only).
#' * `"waking"` Paging (Linux only, not valid since the 2.6.xx kernel).
#'
#' Works for zombie processes.
#'
#' @param p Process handle.
#' @return Character scalar.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_status(p)

ps_status <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_status, p)
}


#' Owner of the process
#'
#' The name of the user that owns the process. On Unix it is calculated
#' from the real user id.
#'
#' On Unix, a numeric uid id returned if the uid is not in the user
#' database, thus a username cannot be determined.
#'
#' Works for zombie processes.
#'
#' @param p Process handle.
#' @return String scalar.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_username(p)

ps_username <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_username, p)
}

#' Process current working directory as an absolute path.
#'
#' For a zombie process it throws a `zombie_process` error.
#'
#' @param p Process handle.
#' @return String scalar.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_cwd(p)

ps_cwd <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_cwd, p)
}

#' User ids and group ids of the process
#'
#' User ids and group ids of the process. Both return integer vectors with
#' names: `real`, `effective` and `saved`.
#'
#' Both work for zombie processes.
#'
#' They are not implemented on Windows, they throw a `not_implemented`
#' error.
#'
#' @param p Process handle.
#' @return Named integer vector of length 3, with names: `real`,
#'   `effective` and `saved`.
#'
#' @seealso [ps_username()] returns a user _name_ and works on all
#'   platforms.
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ps::ps_os_type()["POSIX"] && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_uids(p)
#' ps_gids(p)

ps_uids <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_uids, p)
}

#' @rdname ps_uids
#' @export

ps_gids <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_gids, p)
}

#' Terminal device of the process
#'
#' Returns the terminal of the process. Not implemented on Windows, always
#' returns `NA_character_`. On Unix it returns `NA_character_` if the
#' process has no terminal.
#'
#' Works for zombie processes.
#'
#' @param p Process handle.
#' @return Character scalar.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_terminal(p)

ps_terminal <- function(p = ps_handle()) {
  assert_ps_handle(p)
  ttynr <- .Call(psll_terminal, p)
  if (is.na(ttynr)) {
    NA_character_
  } else {
    tmap <- get_terminal_map()
    tmap[[as.character(ttynr)]]
  }
}

#' Environment variables of a process
#'
#' `ps_environ()` returns the environment variables of the process, in a
#' named vector, similarly to the return value of `Sys.getenv()`
#' (without arguments).
#'
#' Note: this usually does not reflect changes made after the process
#' started.
#'
#' `ps_environ_raw()` is similar to `p$environ()` but returns the
#' unparsed `"var=value"` strings. This is faster, and sometimes good
#' enough.
#'
#' These functions throw a `zombie_process` error for zombie processes.
#'
#' @param p Process handle.
#' @return `ps_environ()` returns a named character vector (that has a
#' `Dlist` class, so it is printed nicely), `ps_environ_raw()` returns a
#' character vector.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' env <- ps_environ(p)
#' env[["R_HOME"]]

ps_environ <- function(p = ps_handle()) {
  assert_ps_handle(p)
  parse_envs(.Call(psll_environ, p))
}

#' @rdname ps_environ
#' @export

ps_environ_raw <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_environ, p)
}

#' Number of threads
#'
#' Throws a `zombie_process()` error for zombie processes.
#'
#' @param p Process handle.
#' @return Integer scalar.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_num_threads(p)

ps_num_threads <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_num_threads, p)
}

#' CPU times of the process
#'
#' All times are measured in seconds:
#' * `user`: Amount of time that this process has been scheduled in user
#'   mode.
#' * `system`: Amount of time that this process has been scheduled in
#'   kernel mode
#' * `children_user`: On Linux, amount of time that this process's
#'   waited-for children have been scheduled in user mode.
#' * `children_system`: On Linux, Amount of time that this process's
#'   waited-for children have been scheduled in kernel mode.
#'
#' Throws a `zombie_process()` error for zombie processes.
#'
#' @param p Process handle.
#' @return Named real vector or length four: `user`, `system`,
#'   `children_user`,  `children_system`. The last two are `NA` on
#'   non-Linux systems.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_cpu_times(p)
#' proc.time()

ps_cpu_times <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_cpu_times, p)
}

#' Memory usage information
#'
#' @details
#'
#' `ps_memory_info()` returns information about memory usage.
#'
#' It returns a named list. Portable fields:
#' * `rss`: "Resident Set Size", this is the non-swapped physical memory a
#'   process has used (bytes). On UNIX it matches "top"‘s 'RES' column (see doc). On
#'   Windows this is an alias for `wset` field and it matches "Memory"
#'   column of `taskmgr.exe`.
#' * `vmem`: "Virtual Memory Size", this is the total amount of virtual
#'   memory used by the process (bytes). On UNIX it matches "top"‘s 'VIRT' column
#'   (see doc). On Windows this is an alias for the `pagefile` field and
#'   it matches the "Working set (memory)" column of `taskmgr.exe`.
#'
#' Non-portable fields:
#' * `shared`: (Linux) memory that could be potentially shared with other
#'   processes (bytes). This matches "top"‘s 'SHR' column (see doc).
#' * `text`: (Linux): aka 'TRS' (text resident set) the amount of memory
#'   devoted to executable code (bytes). This matches "top"‘s 'CODE' column (see
#'   doc).
#' * `data`: (Linux): aka 'DRS' (data resident set) the amount of physical
#'   memory devoted to other than executable code (bytes). It matches "top"‘s
#'   'DATA' column (see doc).
#' * `lib`: (Linux): the memory used by shared libraries (bytes).
#' * `dirty`: (Linux): the amount of memory in dirty pages (bytes).
#' * `pfaults`: (macOS): number of page faults.
#' * `pageins`: (macOS): number of actual pageins.
#'
#' For the explanation of Windows fields see the
#' [PROCESS_MEMORY_COUNTERS_EX](https://learn.microsoft.com/en-us/windows/win32/api/psapi/ns-psapi-process_memory_counters_ex)
#' structure.
#'
#' `ps_memory_full_info()` returns all fields as `ps_memory_info()`, plus
#' additional information, but typically takes slightly longer to run, and
#' might not have access to some processes that `ps_memory_info()` can
#' query:
#'
#' * `uss`: Unique Set Size, this is the memory which is unique to a
#'   process and which would be freed if the process was terminated right
#'   now.
#' * `pss` (Linux only): Proportional Set Size, is the amount of memory
#'   shared with other processes, accounted in a way that the amount is
#'   divided evenly between the processes that share it. I.e. if a process
#'   has 10 MBs all to itself and 10 MBs shared with another process its
#'   PSS will be 15 MBs.
#' * `swap` (Linux only): amount of memory that has been swapped out to
#'   disk.
#'
#' They both throw a `zombie_process()` error for zombie processes.
#'
#' @param p Process handle.
#' @return Named real vector.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' p
#' ps_memory_info(p)
#' ps_memory_full_info(p)

ps_memory_info <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_memory_info, p)
}

#' @export
#' @rdname ps_memory_info

ps_memory_full_info <- function(p = ps_handle()) {
  assert_ps_handle(p)
  type <- ps_os_type()
  if (type[["LINUX"]]) {
    match <- function(re) {
      mt <- gregexpr(re, smaps, perl = TRUE)[[1]]
      st <- substring(
        smaps,
        attr(mt, "capture.start"),
        attr(mt, "capture.start") + attr(mt, "capture.length") - 1
      )
      sum(as.integer(st), na.rm = TRUE) * 1024
    }

    info <- ps_memory_info(p)
    smaps <- .Call(ps__memory_maps, p)
    info[["uss"]] <- match("\nPrivate.*:\\s+(\\d+)")
    info[["pss"]] <- match("\nPss:\\s+(\\d+)")
    info[["swap"]] <- match("\nSwap:\\s+(\\d+)")
    info

  } else if (type[["MACOS"]]) {
    info <- ps_memory_info(p)
    info[["uss"]] <- .Call(psll_memory_uss, p)
    info

  } else if (type[["WINDOWS"]]) {
    info <- ps_memory_info(p)
    info[["uss"]] <- .Call(psll_memory_uss, p)
    info
  }
}

#' Send signal to a process
#'
#' Send a signal to the process. Not implemented on Windows. See
#' [signals()] for the list of signals on the current platform.
#'
#' It checks if the process is still running, before sending the signal,
#' to avoid signalling the wrong process, because of pid reuse.
#'
#' @param p Process handle.
#' @param sig Signal number, see [signals()].
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ps::ps_os_type()["POSIX"] && ! ps:::is_cran_check()
#' px <- processx::process$new("sleep", "10")
#' p <- ps_handle(px$get_pid())
#' p
#' ps_send_signal(p, signals()$SIGINT)
#' p
#' ps_is_running(p)
#' px$get_exit_status()

ps_send_signal <- function(p = ps_handle(), sig) {
  assert_ps_handle(p)
  assert_signal(sig)
  .Call(psll_send_signal, p, sig)
}

#' Suspend (stop) the process
#'
#' Suspend process execution with `SIGSTOP` preemptively checking
#' whether PID has been reused. On Windows this has the effect of
#' suspending all process threads.
#'
#' @param p Process handle.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ps::ps_os_type()["POSIX"] && ! ps:::is_cran_check()
#' px <- processx::process$new("sleep", "10")
#' p <- ps_handle(px$get_pid())
#' p
#' ps_suspend(p)
#' ps_status(p)
#' ps_resume(p)
#' ps_status(p)
#' ps_kill(p)

ps_suspend <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_suspend, p)
}

#' Resume (continue) a stopped process
#'
#' Resume process execution with SIGCONT preemptively checking
#' whether PID has been reused. On Windows this has the effect of resuming
#' all process threads.
#'
#' @param p Process handle.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ps::ps_os_type()["POSIX"] && ! ps:::is_cran_check()
#' px <- processx::process$new("sleep", "10")
#' p <- ps_handle(px$get_pid())
#' p
#' ps_suspend(p)
#' ps_status(p)
#' ps_resume(p)
#' ps_status(p)
#' ps_kill(p)

ps_resume <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_resume, p)
}

#' Terminate a Unix process
#'
#' Send a `SIGTERM` signal to the process. Not implemented on Windows.
#'
#' Checks if the process is still running, to work around pid reuse.
#'
#' @param p Process handle.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ps::ps_os_type()["POSIX"] && ! ps:::is_cran_check()
#' px <- processx::process$new("sleep", "10")
#' p <- ps_handle(px$get_pid())
#' p
#' ps_terminate(p)
#' p
#' ps_is_running(p)
#' px$get_exit_status()

ps_terminate <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_terminate, p)
}

#' Kill a process
#'
#' Kill the current process with SIGKILL preemptively checking
#' whether PID has been reused. On Windows it uses `TerminateProcess()`.
#'
#' @param p Process handle.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ps::ps_os_type()["POSIX"] && ! ps:::is_cran_check()
#' px <- processx::process$new("sleep", "10")
#' p <- ps_handle(px$get_pid())
#' p
#' ps_kill(p)
#' p
#' ps_is_running(p)
#' px$get_exit_status()

ps_kill <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_kill, p)
}

#' List of child processes (process objects) of the process. Note that
#' this typically requires enumerating all processes on the system, so
#' it is a costly operation.
#'
#' @param p Process handle.
#' @param recursive Whether to include the children of the children, etc.
#' @return List of `ps_handle` objects.
#'
#' @family process handle functions
#' @export
#' @importFrom utils head tail
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_parent(ps_handle())
#' ps_children(p)

ps_children <- function(p = ps_handle(), recursive = FALSE) {
  assert_ps_handle(p)
  assert_flag(recursive)

  mypid <- ps_pid(p)
  mytime <- ps_create_time(p)
  map <- ps_ppid_map()
  ret <- list()

  if (!recursive) {
    for (i in seq_len(nrow(map))) {
      if (map$ppid[i] == mypid) {
        tryCatch({
          child  <- ps_handle(map$pid[i])
          if (mytime <= ps_create_time(child)) {
            ret <- c(ret, child)
          } },
          no_such_process = function(e) NULL,
          zombie_process = function(e) NULL)
      }
    }

  } else {
    seen <- integer()
    stack <- mypid
    while (length(stack)) {
      pid <- tail(stack, 1)
      stack <- head(stack, -1)
      if (pid %in% seen) next           # nocov (happens _very_ rarely)
      seen <- c(seen, pid)
      child_pids <- map[ map[,2] ==  pid, 1]
      for (child_pid in child_pids) {
        tryCatch({
          child <- ps_handle(child_pid)
          if (mytime <= ps_create_time(child)) {
            ret <- c(ret, child)
            stack <- c(stack, child_pid)
          } },
          no_such_process = function(e) NULL,
          zombie_process = function(e) NULL)
      }
    }
  }

  ## This will throw if p has finished
  ps_ppid(p)

  ret
}

#' Query the ancestry of a process
#'
#' Query the parent processes recursively, up to the first process.
#' (On some platforms, like Windows, the process tree is not a tree
#' and may contain loops, in which case `ps_descent()` only goes up
#' until the first repetition.)
#'
#' @param p Process handle.
#' @return A list of process handles, starting with `p`, each one
#' is the parent process of the previous one.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' ps_descent()

ps_descent <- function(p = ps_handle()) {
  assert_ps_handle(p)
  windows <- ps_os_type()[["WINDOWS"]]

  branch <- list()
  branch_pids <- integer()
  current <- p
  current_pid <- ps_pid(p)
  if (windows) current_time <- ps_create_time(p)

  while (TRUE) {
    branch <- c(branch, list(current))
    branch_pids <- c(branch_pids, current_pid)
    parent <- fallback(ps_parent(current), NULL)

    # Might fail on Windows, if the process does not exist
    if (is.null(parent)) break;

    # If the parent pid is the same, we stop.
    # Also, Windows might have loops
    parent_pid <- ps_pid(parent)
    if (parent_pid %in% branch_pids) break;

    # Need to check for pid reuse on Windows
    if (windows) {
      parent_time <- ps_create_time(parent)
      if (current_time <= parent_time) break
      current_time <- parent_time
    }

    current <- parent
    current_pid <- parent_pid
  }

  branch
}

ps_ppid_map <- function() {
  pids <- ps_pids()

  processes <- not_null(lapply(pids, function(p) {
    tryCatch(ps_handle(p), error = function(e) NULL) }))

  pids <- map_int(processes, ps_pid)
  ppids <- map_int(processes, function(p) fallback(ps_ppid(p), NA_integer_))

  ok <- !is.na(ppids)

  data_frame(
    pid = pids[ok],
    ppid = ppids[ok]
  )
}

#' Number of open file descriptors
#'
#' Note that in some IDEs, e.g. RStudio or R.app on macOS, the IDE itself
#' opens files from other threads, in addition to the files opened from the
#' main R thread.
#'
#' For a zombie process it throws a `zombie_process` error.
#'
#' @param p Process handle.
#' @return Integer scalar.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' ps_num_fds(p)
#' f <- file(tmp <- tempfile(), "w")
#' ps_num_fds(p)
#' close(f)
#' unlink(tmp)
#' ps_num_fds(p)

ps_num_fds <- function(p = ps_handle()) {
  assert_ps_handle(p)
  .Call(psll_num_fds, p)
}

#' Open files of a process
#'
#' Note that in some IDEs, e.g. RStudio or R.app on macOS, the IDE itself
#' opens files from other threads, in addition to the files opened from the
#' main R thread.
#'
#' For a zombie process it throws a `zombie_process` error.
#'
#' @param p Process handle.
#' @return Data frame with columns: `fd` and `path`. `fd` is numeric
#'    file descriptor on POSIX systems, `NA` on Windows. `path` is an
#'    absolute path to the file.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' ps_open_files(p)
#' f <- file(tmp <- tempfile(), "w")
#' ps_open_files(p)
#' close(f)
#' unlink(tmp)
#' ps_open_files(p)

ps_open_files <- function(p = ps_handle()) {
  assert_ps_handle(p)

  l <- not_null(.Call(psll_open_files, p))

  d <- data_frame(
    fd = vapply(l, "[[", integer(1), 2),
    path = vapply(l, "[[", character(1), 1))

  d
}

#' List network connections of a process
#'
#' For a zombie process it throws a `zombie_process` error.
#'
#' @param p Process handle.
#' @return Data frame, with columns:
#'    * `fd`: integer file descriptor on POSIX systems, `NA` on Windows.
#'    * `family`: Address family, string, typically `AF_UNIX`, `AF_INET` or
#'       `AF_INET6`.
#'    * `type`: Socket type, string, typically `SOCK_STREAM` (TCP) or
#'       `SOCK_DGRAM` (UDP).
#'    * `laddr`: Local address, string, `NA` for UNIX sockets.
#'    * `lport`: Local port, integer, `NA` for UNIX sockets.
#'    * `raddr`: Remote address, string, `NA` for UNIX sockets. This is
#'      always `NA` for `AF_INET` sockets on Linux.
#'    * `rport`: Remote port, integer, `NA` for UNIX sockets.
#'    * `state`: Socket state, e.g. `CONN_ESTABLISHED`, etc. It is `NA`
#'      for UNIX sockets.
#'
#' @family process handle functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' p <- ps_handle()
#' ps_connections(p)
#' sc <- socketConnection("httpbin.org", port = 80)
#' ps_connections(p)
#' close(sc)
#' ps_connections(p)

ps_connections <- function(p = ps_handle()) {
  assert_ps_handle(p)
  if (ps_os_type()[["LINUX"]]) return(psl_connections(p))

  l <- not_null(.Call(psll_connections, p))

  d <- data_frame(
    fd = vapply(l, "[[", integer(1), 1),
    family = match_names(ps_env$constants$address_families,
                       vapply(l, "[[", integer(1), 2)),
    type = match_names(ps_env$constants$socket_types,
                       vapply(l, "[[", integer(1), 3)),
    laddr = vapply(l, "[[", character(1), 4),
    lport = vapply(l, "[[", integer(1), 5),
    raddr = vapply(l, "[[", character(1), 6),
    rport = vapply(l, "[[", integer(1), 7),
    state = match_names(ps_env$constants$tcp_statuses,
                        vapply(l, "[[", integer(1), 8)))

  d$laddr[d$laddr == ""] <- NA_character_
  d$raddr[d$raddr == ""] <- NA_character_

  d$lport[d$lport == 0] <- NA_integer_
  d$rport[d$rport == 0] <- NA_integer_

  d
}

#' Interrupt a process
#'
#' Sends `SIGINT` on POSIX, and 'CTRL+C' or 'CTRL+BREAK' on Windows.
#'
#' @param p Process handle.
#' @param ctrl_c On Windows, whether to send 'CTRL+C'. If `FALSE`, then
#'   'CTRL+BREAK' is sent. Ignored on non-Windows platforms.
#'
#' @family process handle functions
#' @export

ps_interrupt  <- function(p = ps_handle(), ctrl_c = TRUE) {
  assert_ps_handle(p)
  assert_flag(ctrl_c)
  if (ps_os_type()[["WINDOWS"]]) {
    interrupt <- get_tool("interrupt")
    .Call(psll_interrupt, p, ctrl_c, interrupt)
  } else {
    .Call(psll_interrupt, p, ctrl_c, NULL)
  }
}

#' @return `ps_windows_nice_values()` return a character vector of possible
#' priority values on Windows.
#' @export
#' @rdname ps_get_nice

ps_windows_nice_values <- function() {
 c("realtime",
   "high",
   "above_normal",
   "normal",
   "idle",
   "below_normal")
}

#' Get or set the priority of a process
#'
#' `ps_get_nice()` returns the current priority, `ps_set_nice()` sets a
#' new priority, `ps_windows_nice_values()` list the possible priority
#' values on Windows.
#'
#' Priority values are different on Windows and Unix.
#'
#' On Unix, priority is an integer, which is maximum 20. 20 is the lowest
#' priority.
#'
#' ## Rules:
#' * On Windows you can only set the priority of the processes the current
#'   user has `PROCESS_SET_INFORMATION` access rights to. This typically
#'   means your own processes.
#' * On Unix you can only set the priority of the your own processes.
#'   The superuser can set the priority of any process.
#' * On Unix you cannot set a higher priority, unless you are the superuser.
#'   (I.e. you cannot set a lower number.)
#' * On Unix the default priority of a process is zero.
#'
#' @param p Process handle.
#' @return `ps_get_nice()` returns a string from
#' `ps_windows_nice_values()` on Windows. On Unix it returns an integer
#' smaller than or equal to 20.
#'
#' @export

ps_get_nice <- function(p = ps_handle()) {
  assert_ps_handle(p)
  code <- .Call(psll_get_nice, p)
  if (ps_os_type()[["WINDOWS"]]) {
    ps_windows_nice_values()[code]
  } else {
    code
  }
}

#' @param value On Windows it must be a string, one of the values of
#' `ps_windows_nice_values()`. On Unix it is a priority value that is
#' smaller than or equal to 20.
#' @return `ps_set_nice()` return `NULL` invisibly.
#'
#' @export
#' @rdname ps_get_nice

ps_set_nice <- function(p = ps_handle(), value) {
  assert_ps_handle(p)
  assert_nice_value(value)
  if (ps_os_type()[["POSIX"]]) {
    value <- as.integer(value)
  } else {
    value <- match(value, ps_windows_nice_values())
  }
  invisible(.Call(psll_set_nice, p, value))
}

#' List the dynamically loaded libraries of a process
#'
#' Note: this function currently only works on Windows.
#' @param p Process handle.
#' @return Data frame with one column currently: `path`, the
#' absolute path to the loaded module or shared library. On Windows
#' the list includes the executable file itself.
#'
#' @export
#' @family process handle functions
#' @family shared library tools
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check() && ps::ps_os_type()[["WINDOWS"]]
#' # The loaded DLLs of the current process
#' ps_shared_libs()

ps_shared_libs <- function(p = ps_handle()) {
  assert_ps_handle(p)
  if (!ps_os_type()[["WINDOWS"]]) {
    stop("`ps_shared_libs()` is currently only supported on Windows")
  }

  l <- .Call(psll_dlls, p)

  d <- data_frame(
    path = map_chr(l, "[[", 1)
  )

  d
}

#' Query or set CPU affinity
#'
#' `ps_get_cpu_affinity()` queries the
#' [CPU affinity](https://www.linuxjournal.com/article/6799?page=0,0) of
#' a process. `ps_set_cpu_affinity()` sets the CPU affinity of a process.
#'
#' CPU affinity consists in telling the OS to run a process on a limited
#' set of CPUs only (on Linux cmdline, the `taskset` command is typically
#' used).
#'
#' These functions are only supported on Linux and Windows. They error on macOS.
#'
#' @param p Process handle.
#' @param affinity Integer vector of CPU numbers to restrict a process to.
#' CPU numbers start with zero, and they have to be smaller than the
#' number of (logical) CPUs, see [ps_cpu_count()].
#'
#' @return `ps_get_cpu_affinity()` returns an integer vector of CPU
#' numbers, starting with zero.
#'
#' `ps_set_cpu_affinity()` returns `NULL`, invisibly.
#'
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check() && ! ps::ps_os_type()[["MACOS"]]
#' # current
#' orig <- ps_get_cpu_affinity()
#' orig
#'
#' # restrict
#' ps_set_cpu_affinity(affinity = 0:0)
#' ps_get_cpu_affinity()
#'
#' # restore
#' ps_set_cpu_affinity(affinity = orig)
#' ps_get_cpu_affinity()

ps_get_cpu_affinity <- function(p = ps_handle()) {
  assert_ps_handle(p)
  type <- ps_os_type()
  if (!type[["LINUX"]] && !type[["WINDOWS"]]) {
    stop("`ps_cpu_affinity()` is only supported on Windows and Linux")
  }

  .Call(psll_get_cpu_aff, p)
}

#' @export
#' @rdname ps_get_cpu_affinity

ps_set_cpu_affinity <- function(p = ps_handle(), affinity) {
  assert_ps_handle(p)
  type <- ps_os_type()
  if (!type[["LINUX"]] && !type[["WINDOWS"]]) {
    stop("`ps_cpu_affinity()` is only supported on Windows and Linux")
  }

  # check affinity values
  cnt <- ps_cpu_count()
  stopifnot(is.integer(affinity), all(affinity < cnt))

  invisible(.Call(psll_set_cpu_aff, p, affinity))
}
