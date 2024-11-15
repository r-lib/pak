
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <stdlib.h>

#include "ps-internal.h"
#include "common.h"
#include "config.h"
#include "cleancall.h"

#ifdef PS__MACOS
#include <mach/mach_time.h>
extern struct mach_timebase_info PS_MACH_TIMEBASE_INFO;
#endif


static const R_CallMethodDef callMethods[]  = {
  CLEANCALL_METHOD_RECORD,

  /* System api */
  { "ps__os_type",            (DL_FUNC) ps__os_type,            0 },
  { "ps__pids",               (DL_FUNC) ps__pids,               0 },
  { "ps__boot_time",          (DL_FUNC) ps__boot_time,          0 },
  { "ps__cpu_count_logical",  (DL_FUNC) ps__cpu_count_logical,  0 },
  { "ps__cpu_count_physical", (DL_FUNC) ps__cpu_count_physical, 0 },
  { "ps__system_cpu_times",   (DL_FUNC) ps__system_cpu_times,   0 },
  { "ps__users",              (DL_FUNC) ps__users,              0 },
  { "ps__loadavg",            (DL_FUNC) ps__loadavg,            1 },
  { "ps__tty_size",           (DL_FUNC) ps__tty_size,           0 },
  { "ps__disk_partitions",    (DL_FUNC) ps__disk_partitions,    1 },
  { "ps__disk_usage",         (DL_FUNC) ps__disk_usage,         1 },
  { "ps__disk_io_counters",   (DL_FUNC) ps__disk_io_counters,   0 },
  { "ps__fs_info",            (DL_FUNC) ps__fs_info,            3 },
  { "ps__system_memory",      (DL_FUNC) ps__system_memory,      0 },
  { "ps__system_swap",        (DL_FUNC) ps__system_swap,        0 },
  { "ps__list_apps",          (DL_FUNC) ps__list_apps,          0 },
  { "ps__stat",               (DL_FUNC) ps__stat,               2 },
  { "ps__mount_point",        (DL_FUNC) ps__mount_point,        1 },
  /* ps_handle API */
  { "psll_pid",          (DL_FUNC) psll_pid,          1 },
  { "psll_handle",       (DL_FUNC) psll_handle,       2 },
  { "psll_format",       (DL_FUNC) psll_format,       1 },
  { "psll_parent",       (DL_FUNC) psll_parent,       1 },
  { "psll_ppid",         (DL_FUNC) psll_ppid,         1 },
  { "psll_is_running",   (DL_FUNC) psll_is_running,   1 },
  { "psll_name",         (DL_FUNC) psll_name,         1 },
  { "psll_exe",          (DL_FUNC) psll_exe,          1 },
  { "psll_cmdline",      (DL_FUNC) psll_cmdline,      1 },
  { "psll_status",       (DL_FUNC) psll_status,       1 },
  { "psll_username",     (DL_FUNC) psll_username,     1 },
  { "psll_create_time",  (DL_FUNC) psll_create_time,  1 },
  { "psll_cwd",          (DL_FUNC) psll_cwd,          1 },
  { "psll_uids",         (DL_FUNC) psll_uids,         1 },
  { "psll_gids",         (DL_FUNC) psll_gids,         1 },
  { "psll_terminal",     (DL_FUNC) psll_terminal,     1 },
  { "psll_environ",      (DL_FUNC) psll_environ,      1 },
  { "psll_num_threads",  (DL_FUNC) psll_num_threads , 1 },
  { "psll_cpu_times",    (DL_FUNC) psll_cpu_times,    1 },
  { "psll_memory_info",  (DL_FUNC) psll_memory_info , 1 },
  { "psll_memory_uss",   (DL_FUNC) psll_memory_uss,   1 },
  { "psll_send_signal",  (DL_FUNC) psll_send_signal , 2 },
  { "psll_suspend",      (DL_FUNC) psll_suspend,      1 },
  { "psll_resume",       (DL_FUNC) psll_resume,       1 },
  { "psll_terminate",    (DL_FUNC) psll_terminate,    1 },
  { "psll_kill",         (DL_FUNC) psll_kill,         2 },
  { "psll_num_fds",      (DL_FUNC) psll_num_fds,      1 },
  { "psll_open_files",   (DL_FUNC) psll_open_files,   1 },
  { "psll_interrupt",    (DL_FUNC) psll_interrupt,    3 },
  { "psll_connections",  (DL_FUNC) psll_connections,  1 },
  { "psll_get_nice",     (DL_FUNC) psll_get_nice,     1 },
  { "psll_set_nice",     (DL_FUNC) psll_set_nice,     2 },
  { "psll_dlls",         (DL_FUNC) psll_dlls,         1 },
  { "psll_get_cpu_aff",  (DL_FUNC) psll_get_cpu_aff,  1 },
  { "psll_set_cpu_aff",  (DL_FUNC) psll_set_cpu_aff,  2 },
  { "psll_wait",         (DL_FUNC) psll_wait,         2 },

  /* Utils */
  { "ps__init",          (DL_FUNC) ps__init,          2 },
  { "ps__find_if_env",   (DL_FUNC) ps__find_if_env,   3 },
  { "ps__inet_ntop",     (DL_FUNC) ps__inet_ntop,     2 },
  { "ps__memory_maps",   (DL_FUNC) ps__memory_maps,   1 },

  { "psp__pid_exists",   (DL_FUNC) psp__pid_exists,   1 },
  { "psp__stat_st_rdev", (DL_FUNC) psp__stat_st_rdev, 1 },
  { "psp__zombie",       (DL_FUNC) psp__zombie,       0 },
  { "psp__waitpid",      (DL_FUNC) psp__waitpid,      1 },

  { "psw__realpath",     (DL_FUNC) psw__realpath,     1 },

  { NULL, NULL, 0 }
};

int ps_pidfd_open_support = PS_MAYBE;

/*
 * Called on module import on all platforms.
 */
void R_init_ps(DllInfo *dll) {
  cleancall_init();

  if (getenv("R_PS_DEBUG") != NULL) PS__DEBUG = 1;
  if (getenv("R_PS_TESTING") != NULL) PS__TESTING = 1;

  PROTECT(ps__last_error = ps__build_named_list(
    "ssii",
    "message", "Unknown error",
    "class", "fs_error",
    "errno", 0,
    "pid", NA_INTEGER
  ));

  R_PreserveObject(ps__last_error);
  UNPROTECT(1);

#ifdef PS__MACOS
  mach_timebase_info(&PS_MACH_TIMEBASE_INFO);
#endif

  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
