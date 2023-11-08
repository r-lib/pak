
#' Ids of all processes on the system
#'
#' @return Integer vector of process ids.
#' @export

ps_pids <- function() {
  os <- ps_os_type()
  pp <- if (os[["MACOS"]])
    ps_pids_macos()
  else if (os[["LINUX"]])
    ps_pids_linux()
  else if (os[["WINDOWS"]])
    ps_pids_windows()
  else
    stop("Not implemented for this platform")

  sort(pp)
}

ps_pids_windows <- function() {
  sort(.Call(ps__pids))
}

ps_pids_macos <- function() {
  ls <- .Call(ps__pids)
  ## 0 is missing from the list, usually, even though it is a process
  if (! 0L %in% ls && ps_pid_exists_macos(0L)) {
    ls <- c(ls, 0L)
  }
  ls
}

ps_pid_exists_macos <- function(pid) {
  .Call(psp__pid_exists, as.integer(pid))
}

ps_pids_linux <- function() {
  sort(as.integer(dir("/proc", pattern = "^[0-9]+$")))
}

#' Boot time of the system
#'
#' @return A `POSIXct` object.
#'
#' @export

ps_boot_time <- function() {
  format_unix_time(.Call(ps__boot_time))
}

#' List users connected to the system
#'
#' @return A data frame with columns
#'  `username`, `tty`, `hostname`, `start_time`, `pid`. `tty` and `pid`
#'  are `NA` on Windows. `pid` is the process id of the login process.
#'  For local users the `hostname` column is the empty string.
#'
#' @export

ps_users <- function() {
  l <- not_null(.Call(ps__users))

  d <- data_frame(
    username = vapply(l, "[[", character(1), 1),
    tty = vapply(l, "[[", character(1), 2),
    hostname = vapply(l, "[[", character(1), 3),
    start_time = format_unix_time(vapply(l, "[[", double(1), 4)),
    pid = vapply(l, "[[", integer(1),  5)
  )

  d
}

#' Number of logical or physical CPUs
#'
#' If cannot be determined, it returns `NA`. It also returns `NA` on older
#' Windows systems, e.g. Vista or older and Windows Server 2008 or older.
#'
#' @param logical Whether to count logical CPUs.
#' @return Integer scalar.
#'
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' ps_cpu_count(logical = TRUE)
#' ps_cpu_count(logical = FALSE)

ps_cpu_count <- function(logical = TRUE) {
  assert_flag(logical)
  if (logical) ps_cpu_count_logical() else ps_cpu_count_physical()
}

 ps_cpu_count_logical <- function() {
   .Call(ps__cpu_count_logical)
 }

ps_cpu_count_physical <- function() {
  if (ps_os_type()[["LINUX"]]) {
    ps_cpu_count_physical_linux()
  } else {
    .Call(ps__cpu_count_physical)
  }
}

#' Query the size of the current terminal
#'
#' If the standard output of the current R process is not a terminal,
#' e.g. because it is redirected to a file, or the R process is running in
#' a GUI, then it will throw an error. You need to handle this error if
#' you want to use this function in a package.
#'
#' If an error happens, the error message is different depending on
#' what type of device the standard output is. Some common error messages
#' are:
#' * "Inappropriate ioctl for device."
#' * "Operation not supported on socket."
#' * "Operation not supported by device."
#'
#' Whatever the error message, `ps_tty_size` always fails with an error of
#' class `ps_unknown_tty_size`, which you can catch.
#'
#' @export
#' @examples
#' # An example that falls back to the 'width' option
#' tryCatch(
#'   ps_tty_size(),
#'   ps_unknown_tty_size = function(err) {
#'     c(width = getOption("width"), height = NA_integer_)
#'   }
#' )

ps_tty_size <- function() {
  tryCatch(
    ret <- .Call(ps__tty_size),
    error = function(err) {
      class(err) <- c("ps_unknown_tty_size", class(err))
      stop(err)
    }
  )
  c(width = ret[1], height = ret[2])
}

#' List all processes that loaded a shared library
#'
#' @details
#' ## Notes:
#' This function currently only works on Windows.
#'
#' On Windows, a 32 bit R process can only list other 32 bit processes.
#' Similarly, a 64 bit R process can only list other 64 bit processes.
#' This is a limitation of the Windows API.
#'
#' Even though Windows file systems are (almost always) case
#' insensitive, the matching of `paths`, `user` and also `filter`
#' are case sensitive. This might change in the future.
#'
#' This function can be very slow on Windows, because it needs to
#' enumerate all shared libraries of all processes in the system,
#' unless the `filter` argument is set. Make sure you set `filter`
#' if you can.
#'
#' If you want to look up multiple shared libraries, list all of them
#' in `paths`, instead of calling `ps_shared_lib_users` for each
#' individually.
#'
#' If you are after libraries loaded by R processes, you might want to
#' set `filter` to `c("Rgui.exe", "Rterm.exe", "rsession.exe")` The
#' last one is for RStudio.
#'
#' @param paths Character vector of paths of shared libraries to
#' look up. They must be absolute paths. They don't need to exist.
#' Forward slashes are converted to backward slashes on Windows, and
#' the output will always have backward slashes in the paths.
#' @param user Character scalar or `NULL`. If not `NULL`, then only
#' the processes of this user are considered. It defaults to the
#' current user.
#' @param filter Character vector or `NULL`. If not NULL, then it is
#' a vector of glob expressions, used to filter the process names.
#' @return A data frame with columns:
#' * `dll`: the file name of the dll file, without the path,
#' * `path`: path to the shared library,
#' * `pid`: process ID of the process,
#' * `name`: name of the process,
#' * `username`: username of process owner,
#' * `ps_handle`: `ps_handle` object, that can be used to further
#'   query and manipulate the process.
#'
#' @export
#' @family shared library tools
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check() && ps::ps_os_type()[["WINDOWS"]]
#' dlls <- vapply(getLoadedDLLs(), "[[", character(1), "path")
#' psdll <- dlls[["ps"]][[1]]
#' r_procs <- c("Rgui.exe", "Rterm.exe", "rsession.exe")
#' ps_shared_lib_users(psdll, filter = r_procs)

ps_shared_lib_users <- function(paths, user = ps_username(),
                                filter = NULL) {
  os <- ps_os_type()
  if (!os[["WINDOWS"]]) {
    stop("`ps_shared_lib_users()` currently only works on Windows")
  }
  assert_character(paths)
  if (!is.null(user)) assert_string(user)
  if (!is.null(filter)) assert_character(filter)
  if (os[["WINDOWS"]]) paths <- gsub("/", "\\", paths, fixed = TRUE)

  pids <- ps_pids()
  processes <- not_null(lapply(pids, function(p) {
    tryCatch(ps_handle(p), error = function(e) NULL) }))

  nm <- map_chr(processes, function(p)
    fallback(ps_name(p), NA_character_))

  if (!is.null(filter)) {
    selected <- glob$test_any(filter, nm)
    processes <- processes[selected]
    nm <- nm[selected]
  }

  us <- map_chr(processes, function(p)
    fallback(ps_username(p), NA_character_))

  if (!is.null(user)) {
    us2 <- short_username(us)
    selected <- (!is.na(us) & us == user) | (!is.na(us2) & us2 == user)
    processes <- processes[selected]
    nm <- nm[selected]
    us <- us[selected]
  }

  libs <- lapply(processes, function(p) {
    tryCatch(ps_shared_libs(p)$path, error = function(e) character())
  })

  # TODO: handle case insensitive OS/FS

  match <- lapply(libs, intersect, paths)
  match_len <- map_int(match, length)
  match_processes <- processes[match_len > 0]
  match_username <- us[match_len > 0]
  match_name <- nm[match_len > 0]
  match_len <- match_len[match_len > 0]
  match_pids <- map_int(match_processes, ps_pid)

  d <- data_frame(
    dll = basename(unlist(match)),
    path = unlist(match),
    pid = rep(match_pids, match_len),
    name = rep(match_name, match_len),
    username = rep(match_username, match_len),
    ps_handle = I(rep(match_processes, match_len))
  )

  # The ones without name probably finished already.
  d <- d[!is.na(d$name), , drop = FALSE]

  d
}

short_username <- function(x) {
  xs <- strsplit(x, "\\", fixed = TRUE)
  p1 <- map_chr(xs, "[", 1)
  p2 <- map_chr(xs, "[", 2)
  ifelse(!is.na(p2), p2, x)
}

# Docs from psutil, thanks!

#' Return the average system load over the last 1, 5 and 15 minutes as a
#' tuple.
#'
#' The “load” represents the processes which are in a runnable
#' state, either using the CPU or waiting to use the CPU (e.g. waiting for
#' disk I/O). On Windows this is emulated by using a Windows API that
#' spawns a thread which keeps running in background and updates results
#' every 5 seconds, mimicking the UNIX behavior. Thus, on Windows, the
#' first time this is called and for the next 5 seconds it will return a
#' meaningless (0.0, 0.0, 0.0) vector. The numbers returned only make sense
#' if related to the number of CPU cores installed on the system. So, for
#' instance, a value of 3.14 on a system with 10 logical CPUs means that
#' the system load was 31.4% percent over the last N minutes.
#'
#' @return Numeric vector of length 3.
#'
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' ps_loadavg()

ps_loadavg <- function() {
  if (is.null(ps_env$counter_name)) {
    if (ps_os_type()[["WINDOWS"]]) {
      ps_env$counter_name <- find_loadavg_counter()
    } else {
      ps_env$counter_name <- ""
    }
  }

  .Call(ps__loadavg, ps_env$counter_name)
}

find_loadavg_counter <- function() {
  key <- paste0(
    "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Perflib\\",
    "CurrentLanguage"
  )
  tryCatch({
    pc <- utils::readRegistry(key)
    idx <- seq(2, length(pc$Counter), by = 2)
    cnt <- structure(pc$Counter[idx], names = pc$Counter[idx - 1])
    nm <- paste0("\\", cnt["2"], "\\", cnt["44"])
    Encoding(nm) <- ""
    enc2utf8(nm)
  }, error = function(e) "\\System\\Processor Queue Length")
}

#' System CPU times.
#'
#' Every attribute represents the seconds the CPU has spent in the given
#' mode. The attributes availability varies depending on the platform:
#' * `user`: time spent by normal processes executing in user mode;
#'   on Linux this also includes guest time.
#' * `system`: time spent by processes executing in kernel mode.
#' * `idle`: time spent doing nothing.
#'
#' Platform-specific fields:
#'
#' * `nice` (UNIX): time spent by niced (prioritized) processes executing
#'   in user mode; on Linux this also includes guest_nice time.
#' * `iowait` (Linux): time spent waiting for I/O to complete. This is not
#'   accounted in idle time counter.
#' * `irq` (Linux): time spent for servicing hardware interrupts.
#' * `softirq` (Linux): time spent for servicing software interrupts.
#' * `steal` (Linux 2.6.11+): time spent by other operating systems
#'   running in a virtualized environment.
#' * `guest` (Linux 2.6.24+): time spent running a virtual CPU for guest
#'   operating systems under the control of the Linux kernel.
#' * `guest_nice` (Linux 3.2.0+): time spent running a niced guest
#'   (virtual CPU for guest operating systems under the control of the
#'   Linux kernel).
#'
#' @return Named list
#'
#' @export
#' @examplesIf ps::ps_is_supported()
#' ps_system_cpu_times()

ps_system_cpu_times <- function() {
  os <- ps_os_name()
  if (os == "LINUX") {
    ps__system_cpu_times_linux()
  } else {
    .Call(ps__system_cpu_times)
  }
}
