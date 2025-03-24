
#' @useDynLib ps, .registration = TRUE
NULL

#' Process table
#'
#' Data frame with the currently running processes.
#'
#' Columns shown by default, if `columns` is not given or `NULL`:
#' * `pid`: Process ID.
#' * `ppid`: Process ID of parent process.
#' * `name`: Process name.
#' * `username`: Name of the user (real uid on POSIX).
#' * `status`: I.e. *running*, *sleeping*, etc.
#' * `user`: User CPU time.
#' * `system`: System CPU time.
#' * `rss`: Resident set size, the amount of memory the process currently
#'    uses. Does not include memory that is swapped out. It does include
#'    shared libraries.
#' * `vms`: Virtual memory size. All memory the process has access to.
#' * `created`: Time stamp when the process was created.
#' * `ps_handle`: `ps_handle` objects, in a list column.
#'
#' Additional columns that can be requested via `columns`:
#' * `cmdline`: Command line, in a single string, from [ps_cmdline()].
#' * `vcmdline`: Like `cmdline`, but each command line argument in a
#'   separate string.
#' * `cwd`: Current working directory, from [ps_cwd()].
#' * `exe`: Path of the executable of the process, from [ps_exe()].
#' * `num_fds`: Number of open file descriptors, from [ps_num_fds()].
#' * `num_threads`: Number of threads, from [ps_num_threads()].
#' * `cpu_children_user`: See [ps_cpu_times()].
#' * `cpu_children_system`: See [ps_cpu_times()].
#' * `terminal`: Terminal device, from [ps_terminal()].
#' * `uid_real`: Real user id, from [ps_uids()].
#' * `uid_effective`: Effective user id, from [ps_uids()].
#' * `uid_saved`: Saved user id, from [ps_uids()].
#' * `gid_real`: Real group id, from [ps_gids()].
#' * `gid_effective`: Effective group id, from [ps_gids()].
#' * `gid_saved`: Saved group id, from [ps_gids()].
#' * `mem_shared`: See [ps_memory_info()].
#' * `mem_text`: See [ps_memory_info()].
#' * `mem_data`: See [ps_memory_info()].
#' * `mem_lib`: See [ps_memory_info()].
#' * `mem_dirty`: See [ps_memory_info()].
#' * `mem_pfaults`: See [ps_memory_info()].
#' * `mem_pageins`: See [ps_memory_info()].
#' * `mem_maxrss`: See [ps_memory_full_info()].
#' * `mem_uss`: See [ps_memory_full_info()].
#' * `mem_pss`: See [ps_memory_full_info()].
#' * `mem_swap`: See [ps_memory_full_info()].
#'
#' Use `"*"` in `columns` to include all columns.
#'
#' @param user Username, to filter the results to matching processes.
#' @param after Start time (`POSIXt`), to filter the results to processes
#'   that started after this.
#' @param columns Columns to include in the result. If `NULL` (the default),
#'   then a default set of columns are returned, see below. The columns are
#'   shown in the same order they are specified in `columns`, but each
#'   column is included at most once. Use `"*"` to include all possible
#'   columns, and prefix a column name with `-` to remove it.
#' @return Data frame, see columns below.
#'
#' @export

ps <- function(user = NULL, after = NULL, columns = NULL) {
  if (!is.null(user)) assert_string(user)
  if (!is.null(after)) assert_time(after)

  columns <- unique(columns %||% ps_default_columns)
  if ("*" %in% columns) {
    columns <- append(columns, ps_all_columns, which(columns == "*"))
    columns <- columns[columns != "*"]
  }
  if (any(bad <- !columns %in% ps_all_columns)) {
    stop(
      "Unknown column", if (sum(bad) > 1) "s", " requested: ",
      paste(columns[bad], collapse = ", ")
    )
  }

  pids <- ps_pids()
  processes <- not_null(lapply(pids, function(p) {
    tryCatch(ps_handle(p), error = function(e) NULL) }))

  ct <- NULL
  if (!is.null(after)) {
    ct <- lapply(processes, ps_create_time)
    selected <- ct >= after
    processes <- processes[selected]
    ct <- ct[selected]
  }

  us <- NULL
  if (!is.null(user)) {
    us <- map_chr(processes, function(p)
      fallback(ps_username(p), NA_character_))
    selected <- !is.na(us) & us == user
    processes <- processes[selected]
    us <- us[selected]
    ct <- ct[selected]
  }

  ct <- ct %||% lapply(processes, function(p)
    fallback(ps_create_time(p), NA_time()))

  if (c_username <- "username" %in% columns) {
    us <- us %||% map_chr(processes, function(p)
      fallback(ps_username(p), NA_character_))
  }
  if (c_pid <- "pid" %in% columns) {
    pd <- map_int(processes, function(p) fallback(ps_pid(p), NA_integer_))
  }
  if (c_ppid <- "ppid" %in% columns) {
    pp <- map_int(processes, function(p) fallback(ps_ppid(p), NA_integer_))
  }
  if (c_name <- "name" %in% columns) {
    nm <- map_chr(processes, function(p) fallback(ps_name(p), NA_character_))
  }

  if (c_status <- "status" %in% columns) {
    opt <- options(ps.no_external_ps = TRUE)
    on.exit(options(opt), add = TRUE)
    st <- map_chr(processes, function(p) fallback(ps_status(p), NA_character_))
    options(opt)
    if (ps_os_type()[["MACOS"]] && !isTRUE(getOption("ps.no_external_ps")) &&
        anyNA(st[pids != 0])) {
      misspids <- map_int(processes[is.na(st)], ps_pid)
      st[is.na(st)] <- ps_status_macos_ps(misspids)
    }
  }

  c_user <- "user" %in% columns
  c_system <- "system" %in% columns
  c_cpu_children_user <- "cpu_children_user" %in% columns
  c_cpu_children_system <- "cpu_children_system" %in% columns
  if (c_user || c_system || c_cpu_children_user || c_cpu_children_system) {
    time <- lapply(processes, function(p) fallback(ps_cpu_times(p), NULL))
    cpt <- map_dbl(time, function(x) x[["user"]] %||% NA_real_)
    cps <- map_dbl(time, function(x) x[["system"]] %||% NA_real_)
    cpct <- map_dbl(time, function(x) x[["children_user"]] %||% NA_real_)
    cpcs <- map_dbl(time, function(x) x[["children_system"]] %||% NA_real_)
  }
  c_rss <- "rss" %in% columns
  c_vms <- "vms" %in% columns
  c_mem_shared <- "mem_shared" %in% columns
  c_mem_text <- "mem_text" %in% columns
  c_mem_data <- "mem_data" %in% columns
  c_mem_lib <- "mem_lib" %in% columns
  c_mem_dirty <- "mem_dirty" %in% columns
  c_mem_pfaults <- "mem_pfaults" %in% columns
  c_mem_pageins <- "mem_pageins" %in% columns
  c_mem_maxrss <- "mem_maxrss" %in% columns
  c_mem_uss <- "mem_uss" %in% columns
  c_mem_pss <- "mem_pss" %in% columns
  c_mem_swap <- "mem_swap" %in% columns
  if (c_rss || c_vms || c_mem_shared || c_mem_text || c_mem_data ||
      c_mem_lib || c_mem_dirty || c_mem_pfaults || c_mem_pageins ||
      c_mem_maxrss || c_mem_uss || c_mem_pss || c_mem_swap) {
    if (c_mem_maxrss || c_mem_uss || c_mem_pss || c_mem_swap) {
      mem <- lapply(processes, function(p) fallback(ps_memory_full_info(p), NULL))
    } else {
      mem <- lapply(processes, function(p) fallback(ps_memory_info(p), NULL))
    }
    rss <- map_dbl(mem, function(x) x[["rss"]] %||% NA_real_)
    vms <- map_dbl(mem, function(x) x[["vms"]] %||% NA_real_)
    mem_shared <- map_dbl(mem, function(x) x["shared"] %||% NA_real_)
    mem_text <- map_dbl(mem, function(x) x["text"] %||% NA_real_)
    mem_data <- map_dbl(mem, function(x) x["data"] %||% NA_real_)
    mem_lib <- map_dbl(mem, function(x) x["lib"] %||% NA_real_)
    mem_dirty <- map_dbl(mem, function(x) x["dirty"] %||% NA_real_)
    mem_pfaults <- map_dbl(mem, function(x) x["pfaults"] %||% NA_real_)
    mem_pageins <- map_dbl(mem, function(x) x["pageins"] %||% NA_real_)
    mem_maxrss<- map_dbl(mem, function(x) x["maxrss"] %||% NA_real_)
    mem_uss <- map_dbl(mem, function(x) x["uss"] %||% NA_real_)
    mem_pss <- map_dbl(mem, function(x) x["pss"] %||% NA_real_)
    mem_swap <- map_dbl(mem, function(x) x["swap"] %||% NA_real_)
  }
  if (c_ps_handle <- "ps_handle" %in% columns) {
    ps_handle <- I(processes)
  }
  c_cmdline <- "cmdline" %in% columns
  c_vcmdline <- "vcmdline" %in% columns
  if (c_cmdline || c_vcmdline) {
    vcmdline <- I(lapply(
      processes,
      function(p) fallback(ps_cmdline(p), NULL)
    ))
    cmdline <- map_chr(
      vcmdline,
      function(x) {
        if (is.null(x)) NA_character_ else paste(x, collapse = " ")
      }
    )
  }

  if (c_cwd <- "cwd" %in% columns) {
    cwd <- map_chr(processes, function(x) fallback(ps_cwd(x), NA_character_))
  }
  if (c_exe <- "exe" %in% columns) {
    exe <- map_chr(processes, function(x) fallback(ps_exe(x), NA_character_))
  }
  if (c_num_fds <- "num_fds" %in% columns) {
    num_fds <- map_int(processes, function(x) fallback(ps_num_fds(x), NA_integer_))
  }
  if (c_num_threads <- "num_threads" %in% columns) {
    num_threads <- map_int(processes, function(x) fallback(ps_num_threads(x), NA_integer_))
  }
  if (c_terminal <- "terminal" %in% columns) {
    terminal <- map_chr(processes, function(x) fallback(ps_terminal(x), NA_character_))
  }
  c_uid_real <- "uid_real" %in% columns
  c_uid_effective <- "uid_effective" %in% columns
  c_uid_saved <- "uid_saved" %in% columns
  if (c_uid_real || c_uid_effective || c_uid_saved) {
    uids <- lapply(processes, function(x) fallback(ps_uids(x), NULL))
    uid_real <- map_int(uids, function(x) if (is.null(x)) NA_integer_ else x[["real"]])
    uid_effective <- map_int(uids, function(x) if (is.null(x)) NA_integer_ else x[["effective"]])
    uid_saved <- map_int(uids, function(x) if (is.null(x)) NA_integer_ else x[["saved"]])
  }
  c_gid_real <- "gid_real" %in% columns
  c_gid_effective <- "gid_effective" %in% columns
  c_gid_saved <- "gid_saved" %in% columns
  if (c_gid_real || c_gid_effective || c_gid_saved) {
    gids <- lapply(processes, function(x) fallback(ps_gids(x), NULL))
    gid_real <- map_int(gids, function(x) if (is.null(x)) NA_integer_ else x[["real"]])
    gid_effective <- map_int(gids, function(x) if (is.null(x)) NA_integer_ else x[["effective"]])
    gid_saved <- map_int(gids, function(x) if (is.null(x)) NA_integer_ else x[["saved"]])
  }

  c_created <- "created" %in% columns
  created <- format_unix_time(unlist(ct))

  pss <- as_data_frame(not_null(list(
    # default
    pid       = if (c_pid)       pd,
    ppid      = if (c_ppid)      pp,
    name      = if (c_name)      nm,
    username  = if (c_username)  us,
    status    = if (c_status)    st,
    user      = if (c_user)      cpt,
    system    = if (c_system)    cps,
    rss       = if (c_rss)       rss,
    vms       = if (c_vms)       vms,
    created   = if (c_created)   created,
    ps_handle = if (c_ps_handle) ps_handle,

    # optional
    cmdline   = if (c_vcmdline)  cmdline,
    vcmdline  = if (c_vcmdline)  vcmdline,
    cwd       = if (c_cwd)       cwd,
    exe       = if (c_exe)       exe,
    num_fds   = if (c_num_fds)   num_fds,
    num_threads = if (c_num_threads) num_threads,
    cpu_children_user = if (c_cpu_children_user) cpct,
    cpu_children_system = if (c_cpu_children_system) cpcs,
    terminal            = if (c_terminal)            terminal,
    uid_real            = if (c_uid_real)            uid_real,
    uid_effective       = if (c_uid_effective)       uid_effective,
    uid_saved           = if (c_uid_saved)           uid_saved,
    gid_real            = if (c_gid_real)            gid_real,
    gid_effective       = if (c_gid_effective)       gid_effective,
    gid_saved           = if (c_gid_saved)           gid_saved,
    mem_shared          = if (c_mem_shared)          mem_shared,
    mem_text            = if (c_mem_text)            mem_text,
    mem_data            = if (c_mem_data)            mem_data,
    mem_lib             = if (c_mem_lib)             mem_lib,
    mem_dirty           = if (c_mem_dirty)           mem_dirty,
    mem_pfaults         = if (c_mem_pfaults)         mem_pfaults,
    mem_pageins         = if (c_mem_pageins)         mem_pageins,
    mem_maxrss          = if (c_mem_maxrss)          mem_maxrss,
    mem_uss             = if (c_mem_uss)             mem_uss,
    mem_pss             = if (c_mem_pss)             mem_pss,
    mem_swap            = if (c_mem_swap)            mem_swap,

    NULL
  )))

  pss <- pss[order(-as.numeric(created)), ]
  pss <- pss[, columns]

  pss
}

ps_default_columns <- c(
  "pid",
  "ppid",
  "name",
  "username",
  "status",
  "user",
  "system",
  "rss",
  "vms",
  "created",
  "ps_handle",
  NULL
)

ps_all_columns <- c(
  ps_default_columns,
  "cmdline",                    # ps_cmdline
  "vcmdline",
  "cwd",                        # ps_cwd
  "exe",
  "num_fds",                    # ps_num_fds
  "num_threads",                # ps_num_threads
  "cpu_children_user",          # ps_cpu_times
  "cpu_children_system",
  "terminal",                   # ps_terminal
  "uid_real",                   # ps_uids
  "uid_effective",
  "uid_saved",
  "gid_real",                   # ps_gids
  "gid_effective",
  "gid_saved",
  "mem_shared",                 # ps_memory_info
  "mem_text",
  "mem_data",
  "mem_lib",
  "mem_dirty",
  "mem_pfaults",
  "mem_pageins",
  "mem_maxrss",                 # ps_full_memory_info
  "mem_uss",
  "mem_pss",
  "mem_swap",
  NULL
)
