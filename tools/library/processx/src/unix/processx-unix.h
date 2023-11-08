#ifndef R_PROCESSX_UNIX_H
#define R_PROCESSX_UNIX_H

#include <unistd.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <pthread.h>

# ifndef O_CLOEXEC
#  define O_CLOEXEC 02000000
# endif

# ifndef SOCK_CLOEXEC
#  define SOCK_CLOEXEC O_CLOEXEC
# endif

typedef struct processx_handle_s {
  int exitcode;
  int collected;	 /* Whether exit code was collected already */
  pid_t pid;
  int fd0;			/* writeable */
  int fd1;			/* readable */
  int fd2;			/* readable */
  int waitpipe[2];		/* use it for wait() with timeout */
  int cleanup;
  double create_time;
  processx_connection_t *pipes[3];
  int ptyfd;
} processx_handle_t;

char *processx__tmp_string(SEXP str, int i);
char **processx__tmp_character(SEXP chr);

extern pthread_t processx__main_thread;
void processx__sigchld_callback(int sig, siginfo_t *info, void *ctx);
void processx__setup_sigchld(void);
void processx__remove_sigchld(void);
void processx__block_sigchld(void);
void processx__unblock_sigchld(void);

void processx__finalizer(SEXP status);

/* Child list and its functions */

typedef struct processx__child_list_s {
  pid_t pid;
  SEXP weak_status;
  struct processx__child_list_s *next;
} processx__child_list_t;

int processx__child_add(pid_t pid, SEXP status);
void processx__child_remove(pid_t pid);
processx__child_list_t *processx__child_find(pid_t pid);
void processx__freelist_add(processx__child_list_t *ptr);
void processx__freelist_free(void);

void processx__collect_exit_status(SEXP status, int retval, int wstat);

int processx__nonblock_fcntl(int fd, int set);
int processx__cloexec_fcntl(int fd, int set);

/* Control connections*/

void processx__create_control_read(processx_handle_t *handle,
				   int fd, const char *membername,
				   SEXP privatex);
void processx__create_control_write(processx_handle_t *handle,
				    int fd, const char *membername,
				    SEXP privatex);

/* Interruptible system calls */

int processx__interruptible_poll(struct pollfd fds[],
				 nfds_t nfds, int timeout);

void processx__make_socketpair(int pipe[2], const char *name);

double processx__create_time(long pid);

#endif
