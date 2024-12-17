
#ifndef R_PS_H
#define R_PS_H

#define R_USE_C99_IN_CXX 1
#include <Rinternals.h>

/* API to be used from R */

/* ps_handle class */

SEXP psll_handle(SEXP pid, SEXP time);

SEXP psll_pid(SEXP p);
SEXP psll_create_time(SEXP p);

SEXP psll_format(SEXP p);
SEXP psll_parent(SEXP p);
SEXP psll_ppid(SEXP p);
SEXP psll_is_running(SEXP p);
SEXP psll_name(SEXP p);
SEXP psll_exe(SEXP p);
SEXP psll_cmdline(SEXP p);
SEXP psll_status(SEXP p);
SEXP psll_username(SEXP p);
SEXP psll_cwd(SEXP p);
SEXP psll_uids(SEXP p);
SEXP psll_gids(SEXP p);
SEXP psll_terminal(SEXP p);
SEXP psll_environ(SEXP p);
SEXP psll_num_threads(SEXP p);
SEXP psll_cpu_times(SEXP p);
SEXP psll_memory_info(SEXP p);
SEXP psll_memory_uss(SEXP p);
SEXP psll_send_signal(SEXP p, SEXP sig);
SEXP psll_suspend(SEXP p);
SEXP psll_resume(SEXP p);
SEXP psll_terminate(SEXP p);
SEXP psll_kill(SEXP p, SEXP grace);
SEXP psll_num_fds(SEXP p);
SEXP psll_open_files(SEXP p);
SEXP psll_interrupt(SEXP p, SEXP ctrlc, SEXP interrupt_path);
SEXP psll_connections(SEXP p);
SEXP psll_get_nice(SEXP p);
SEXP psll_set_nice(SEXP p, SEXP value);
SEXP psll_dlls(SEXP p);
SEXP psll_get_cpu_aff(SEXP p);
SEXP psll_set_cpu_aff(SEXP p, SEXP affinity);
SEXP psll_wait(SEXP p, SEXP timeout);

/* System API */

SEXP ps__os_type(void);
SEXP ps__pids(void);
SEXP ps__boot_time(void);
SEXP ps__cpu_count_logical(void);
SEXP ps__cpu_count_physical(void);
SEXP ps__system_cpu_times(void);
SEXP ps__users(void);
SEXP ps__tty_size(void);
SEXP ps__disk_partitions(SEXP all);
SEXP ps__disk_usage(SEXP paths);
SEXP ps__disk_io_counters(void);
SEXP ps__fs_info(SEXP path, SEXP abspath, SEXP mps);
SEXP ps__system_memory(void);
SEXP ps__system_swap(void);
SEXP ps__loadavg(SEXP counter_name);
SEXP ps__list_apps(void);
SEXP ps__stat(SEXP path, SEXP follow);
SEXP ps__mount_point(SEXP paths);

/* Generic utils used from R */

SEXP ps__init(SEXP psenv, SEXP constenv);
SEXP ps__find_if_env(SEXP marker, SEXP after, SEXP pid);
SEXP ps__inet_ntop(SEXP raw, SEXP fam);
SEXP ps__memory_maps(SEXP p);

SEXP psp__zombie(void);
SEXP psp__waitpid(SEXP pid);
SEXP psp__pid_exists(SEXP r_pid);
SEXP psp__stat_st_rdev(SEXP files);

SEXP psw__realpath(SEXP path);

#endif
