
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include "config.h"
#include <Rinternals.h>

void *ps__not_implemented(const char *what);
SEXP ps__throw_error(void);

SEXP ps__dummy(const char *what) {
  ps__not_implemented(what);
  ps__throw_error();
  return R_NilValue;
}

/* Not implemented on Linux, only on Windows and macOS */
#ifdef  PS__LINUX
#ifndef PS__MACOS
#ifndef PS__WINDOWS
SEXP ps__pids(void)          { return ps__dummy("ps_pids"); }
SEXP psll_memory_uss(SEXP p) { return ps__dummy("psll_memory_uss"); }
#endif
#endif
#endif

/* Not implemented on macOS */
#ifdef  PS__MACOS
#ifndef PS__LINUX
#ifndef PS__WINDOWS
SEXP psll_get_cpu_aff(SEXP p) { return ps__dummy("psll_get_cpu_aff"); }
SEXP psll_set_cpu_aff(SEXP p) { return ps__dummy("psll_set_cpu_aff"); }
#endif
#endif
#endif

/* Not implemented on Windows */
#ifdef PS__WINDOWS
#ifndef PS__POSIX
SEXP psp__pid_exists(SEXP x) { return ps__dummy("psp__pid_exists"); }
SEXP psp__zombie(void) { return ps__dummy("psp__zombie"); }
SEXP psp__waitpid(SEXP x) { return ps__dummy("psp__waitpid"); }
SEXP psp__stat_st_rdev(SEXP x) { return ps__dummy("psp__stat_st_rdev"); }
#endif
#endif

/* Only implemented on windows */
#ifdef PS__POSIX
#ifndef PS__WINDOWS
SEXP psw__realpath(SEXP x) { return ps__dummy("psw__realpath"); }
SEXP psll_dlls(SEXP x) { return ps__dummy("psll_dlls"); }
#endif
#endif

/* Only implemented on linux */
#ifndef PS__LINUX
#if defined(PS__WINDOWS) || defined(PS__MACOS)
SEXP ps__inet_ntop(SEXP x, SEXP y) { return ps__dummy("ps__inet_ntop"); }
SEXP ps__memory_maps(SEXP p) { return ps__dummy("ps__memory_maps"); }
#endif
#endif

/* All C functions called from R */
#ifndef PS__MACOS
#ifndef PS__LINUX
#ifndef PS__WINDOWS
SEXP ps__pids(void) { return ps__dummy("ps_pids"); }
SEXP ps__boot_time(void) { return ps__dummy("ps_boot_time"); }
SEXP ps__cpu_count_logical(void)  { return ps__dummy("ps_cpu_count"); }
SEXP ps__cpu_count_physical(void) { return ps__dummy("ps_cpu_count"); }
SEXP ps__users(void) { return ps__dummy("ps_users"); }
SEXP ps__loadavg(SEXP x) { return ps__dummy("ps_loadavg"); }
SEXP ps__tty_size(void) { return ps__dummy("ps_tty_size"); }
SEXP ps__disk_partitions(SEXP x) { return ps__dummy("ps_disk_partitions"); }
SEXP ps__disk_usage(void) { return ps__dummy("ps_disk_usage"); }
SEXP ps__system_cpu_times(void) { return ps__dummy("ps_system_cpu_times"); }
SEXP ps__system_memory(void) { return ps__dummy("ps_system_memory"); }
SEXP ps__system_swap(void) { return ps__dummy("ps_system_swap"); }

SEXP psll_handle(SEXP x, SEXP y) { return ps__dummy("ps_handle"); }
SEXP psll_format(SEXP x) { return ps__dummy("ps_format"); }
SEXP psll_parent(SEXP x) { return ps__dummy("ps_handle"); }
SEXP psll_ppid(SEXP x) { return ps__dummy("ps_handle"); }
SEXP psll_is_running(SEXP x) { return ps__dummy("ps_is_running"); }
SEXP psll_name(SEXP x) { return ps__dummy("ps_name"); }
SEXP psll_exe(SEXP x) { return ps__dummy("ps_exe"); }
SEXP psll_cmdline(SEXP x) { return ps__dummy("ps_cmdline"); }
SEXP psll_status(SEXP x) { return ps__dummy("ps_status"); }
SEXP psll_username(SEXP x) { return ps__dummy("ps_username"); }
SEXP psll_cwd(SEXP x) { return ps__dummy("ps_cwd"); }
SEXP psll_uids(SEXP x) { return ps__dummy("ps_uids"); }
SEXP psll_gids(SEXP x) { return ps__dummy("ps_gids"); }
SEXP psll_terminal(SEXP x) { return ps__dummy("ps_terminal"); }
SEXP psll_environ(SEXP x) { return ps__dummy("ps_environ"); }
SEXP psll_num_threads(SEXP x) { return ps__dummy("ps_num_threads"); }
SEXP psll_cpu_times(SEXP x) { return ps__dummy("ps_cpu_times"); }
SEXP psll_memory_info(void) { return ps__dummy("ps_memory_info"); }
SEXP psll_send_signal(SEXP x, SEXP y) { return ps__dummy("ps_send_signal"); }
SEXP psll_suspend(SEXP x) { return ps__dummy("ps_suspend"); }
SEXP psll_resume(SEXP x) { return ps__dummy("ps_resume"); }
SEXP psll_terminate(SEXP x) { return ps__dummy("ps_terminate"); }
SEXP psll_kill(SEXP x) { return ps__dummy("ps_kill"); }
SEXP psll_num_fds(SEXP x) { return ps__dummy("ps_num_fds"); }
SEXP psll_open_files(SEXP x) { return ps__dummy("ps_open_files"); }
SEXP psll_interrupt(SEXP x, SEXP y, SEXP z) { return ps__dummy("ps_interrupt"); }
SEXP psll_connections(SEXP x) { return ps__dummy("ps_connections"); }
SEXP psll_get_nice(SEXP x) { return ps__dummy("ps_get_nice"); }
SEXP psll_set_nice(SEXP x, SEXP y) { return ps__dummy("ps_set_nice"); }
SEXP psll_memory_uss(SEXP p) { return ps__dummy("psll_memory_uss"); }
SEXP psll_get_cpu_aff(SEXP p) { return ps__dummy("psll_get_cpu_aff"); }
SEXP psll_set_cpu_aff(SEXP p) { return ps__dummy("psll_set_cpu_aff"); }

SEXP ps__init(SEXP x, SEXP y) { return R_NilValue; /* this needs to run to load package */ }
SEXP ps__kill_if_env(SEXP x, SEXP y, SEXP z, SEXP a) { return ps__dummy("ps__kill_if_env"); }
SEXP ps__find_if_env(SEXP x, SEXP y, SEXP z) { return ps__dummy("ps__find_if_env"); }
SEXP ps__memory_maps(SEXP p) { return ps__dummy("ps__memory_maps"); }

SEXP psp__pid_exists(SEXP x) { return ps__dummy("psp__pid_exists"); }
SEXP psp__stat_st_rdev(SEXP x) { return ps__dummy("psp__stat_st_rdev"); }
SEXP psp__zombie(void) { return ps__dummy("psp__zombie"); }
SEXP psp__waitpid(SEXP x) { return ps__dummy("psp__waitpid"); }

SEXP psw__realpath(SEXP x) { return ps__dummy("psw__realpath"); }
SEXP psll_dlls(SEXP x) { return ps__dummy("psll_dlls"); }

SEXP ps__inet_ntop(SEXP x, SEXP y) { return ps__dummy("ps__inet_ntop"); }
#endif
#endif
#endif
