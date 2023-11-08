
#ifndef _WIN32

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>

#include "../processx.h"
#include "../cleancall.h"

/* Internals */

static void processx__child_init(processx_handle_t *handle, SEXP connections,
				 int (*pipes)[2], int stdio_count,
				 char *command, char **args,
				 int error_fd, const char *pty_name, char **env,
                                 processx_options_t *options,
				 const char *tree_id);

static SEXP processx__make_handle(SEXP private, int cleanup);
static void processx__handle_destroy(processx_handle_t *handle);
void processx__create_connections(processx_handle_t *handle, SEXP private,
				  const char *encoding);

/* Define BSWAP_32 on Big Endian systems */
#ifdef WORDS_BIGENDIAN
#if (defined(__sun) && defined(__SVR4))
#include <sys/byteorder.h>
#elif (defined(__APPLE__) && defined(__ppc__) || defined(__ppc64__))
#include <libkern/OSByteOrder.h>
#define BSWAP_32 OSSwapInt32
#elif (defined(__OpenBSD__))
#define BSWAP_32(x) swap32(x)
#elif (defined(__GLIBC__))
#include <byteswap.h>
#define BSWAP_32(x) bswap_32(x)
#endif
#endif

#if defined(__APPLE__)
# include <crt_externs.h>
# define environ (*_NSGetEnviron())
#else
extern char **environ;
#endif

#include <termios.h>
#include <sys/ioctl.h>
#include <pthread.h>

extern processx__child_list_t child_list_head;
extern processx__child_list_t *child_list;
extern processx__child_list_t child_free_list_head;
extern processx__child_list_t *child_free_list;

extern int processx__notify_old_sigchld_handler;

/* We are trying to make sure that the variables in the library are
   properly set to their initial values after a library (re)load.
   This function is called from `R_init_processx`. */

void R_init_processx_unix(void) {
  processx__main_thread = pthread_self();

  child_list_head.pid = 0;
  child_list_head.weak_status = R_NilValue;
  child_list_head.next = 0;
  child_list = &child_list_head;

  child_free_list_head.pid = 0;
  child_free_list_head.weak_status = R_NilValue;
  child_free_list_head.next = 0;
  child_free_list = &child_free_list_head;

  if (getenv("PROCESSX_NOTIFY_OLD_SIGCHLD")) {
    processx__notify_old_sigchld_handler = 1;
  }
}

int processx__pty_main_open(char *sub_name, size_t sn_len) {
  int main_fd, saved_errno;
  char *p;

  main_fd = posix_openpt(O_RDWR | O_NOCTTY);
  if (main_fd == -1) return -1;

  if (grantpt(main_fd) == -1) {
    saved_errno = errno;
    close(main_fd);
    errno = saved_errno;
    return -1;
  }

  if (unlockpt(main_fd) == -1) {
    saved_errno = errno;
    close(main_fd);
    errno = saved_errno;
    return -1;
  }

  p = ptsname(main_fd);
  if (p == NULL) {
    saved_errno = errno;
    close(main_fd);
    errno = saved_errno;
    return -1;
  }

  if (strlen(p) < sn_len) {
    strncpy(sub_name, p, sn_len);
  } else {
    close(main_fd);
    errno = EOVERFLOW;
    return -1;
  }

  return main_fd;
}

/* These run in the child process, so no coverage here. */
/* LCOV_EXCL_START */

void processx__write_int(int fd, int err) {
  ssize_t dummy = write(fd, &err, sizeof(int));
  (void) dummy;
}

static void processx__child_init(processx_handle_t *handle, SEXP connections,
                                 int (*pipes)[2], int stdio_count,
                                 char *command, char **args,
                                 int error_fd, const char *pty_name, char **env,
                                 processx_options_t *options,
                                 const char *tree_id) {

  int close_fd, use_fd, fd, i;
  int min_fd = 0;

  setsid();

  /* Do we need a pty? */
  if (pty_name) {
    /* Do not mess with stdin/stdout/stderr, all handled by the pty */
    min_fd = 3;

    int sub_fd = open(pty_name, O_RDWR);
    if (sub_fd == -1) {
      processx__write_int(error_fd, -errno);
      raise(SIGKILL);
    }

#ifdef TIOCSCTTY
    if (ioctl(sub_fd, TIOCSCTTY, 0) == -1) {
      processx__write_int(error_fd, -errno);
      raise(SIGKILL);
    }
#endif

#ifdef TIOCSWINSZ
    struct winsize w;
    w.ws_row = options->pty_rows;
    w.ws_col = options->pty_cols;
    if (ioctl(sub_fd, TIOCSWINSZ, &w) == -1) {
      processx__write_int(error_fd, -errno);
      raise(SIGKILL);
    }
#endif

    struct termios tp;

    if (tcgetattr(sub_fd, &tp) == -1) {
      processx__write_int(error_fd, -errno);
      raise(SIGKILL);
    }

    if (options->pty_echo) {
      tp.c_lflag |= ECHO;
    } else {
      tp.c_lflag &= ~ECHO;
    }

    if (tcsetattr(sub_fd, TCSAFLUSH, &tp) == -1) {
      processx__write_int(error_fd, -errno);
      raise(SIGKILL);
    }

    /* TODO: set other terminal attributes and size */

    /* Duplicate pty sub to be child's stdin, stdout, and stderr */
    if (dup2(sub_fd, STDIN_FILENO) != STDIN_FILENO) {
      processx__write_int(error_fd, -errno);
      raise(SIGKILL);
    }
    if (dup2(sub_fd, STDOUT_FILENO) != STDOUT_FILENO) {
      processx__write_int(error_fd, -errno);
      raise(SIGKILL);
    }
    if (dup2(sub_fd, STDERR_FILENO) != STDERR_FILENO) {
      processx__write_int(error_fd, -errno);
      raise(SIGKILL);
    }

    if (sub_fd > STDERR_FILENO) close(sub_fd);
  }

  /* We want to prevent use_fd < fd, because we will dup2() use_fd into
     fd later. If use_fd >= fd, then this is always possible,
     without mixing up stdin, stdout and stderr. Without this, we could
     have a case when we dup2() 2 into 1, and then 1 is lost. */

  for (fd = min_fd; fd < stdio_count; fd++) {
    use_fd = pipes[fd][1];
    /* If use_fd < 0 then there is no pipe for fd. */
    if (use_fd < 0 || use_fd >= fd) continue;
    /* If use_fd < fd, then we create a brand new fd for it,
       starting at stdio_count, which is bigger then fd, surely. */
    pipes[fd][1] = fcntl(use_fd, F_DUPFD, stdio_count);
    if (pipes[fd][1] == -1) {
      processx__write_int(error_fd, -errno);
      raise(SIGKILL);
    }
  }

  /* This loop initializes the stdin, stdout, stderr fds of the child
     process properly. */

  for (fd = min_fd; fd < stdio_count; fd++) {
    SEXP output = VECTOR_ELT(connections, fd);
    const char *stroutput =
      Rf_isString(output) ? CHAR(STRING_ELT(output, 0)) : NULL;

    /* close_fd is an fd that must be closed. Initially this is the
       parent's end of a pipe. (-1 if no pipe for this fd.) */
    close_fd = pipes[fd][0];
    /* use_fd is the fd that the child must use for stdin/out/err. */
    use_fd = pipes[fd][1];

    /* If no pipe, then we see if this is the 2>&1 case. */
    if (fd == 2 && use_fd < 0 && stroutput &&
	! strcmp(stroutput, "2>&1")) {
      use_fd = 1;

    } else if (use_fd < 0) {
      /* Otherwise we open a file. If the stdin/out/err is not
	 requested, then we open a file to /dev/null */
      /* For fd >= 3, the fd is just passed, and we just use it,
	 no need to open any file */
      if (fd >= 3) continue;

      if (stroutput) {
	/* A file was requested, open it */
	if (fd == 0) {
	  use_fd = open(stroutput, O_RDONLY);
	} else {
	  use_fd = open(stroutput, O_CREAT | O_TRUNC| O_RDWR, 0644);
	}
      } else {
	/* NULL, so stdin/out/err is ignored, using /dev/null */
	use_fd = open("/dev/null", fd == 0 ? O_RDONLY : O_RDWR);
      }
      /* In the output file case, we might need to close use_fd, after
	 we dup2()-d it into fd. */
      close_fd = use_fd;

      if (use_fd == -1) {
	processx__write_int(error_fd, -errno);
	raise(SIGKILL);
      }
    }

    /* We will use use_fd for fd. If they happen to be equal, make
       sure that fd is _not_ closed on exec. Otherwise dup2() use_fd
       into fd. dup2() clears the CLOEXEC flag, so no need for a fcntl
       call in this case. */
    if (fd == use_fd) {
      processx__cloexec_fcntl(use_fd, 0);
    } else {
      fd = dup2(use_fd, fd);
    }

    if (fd == -1) {
      processx__write_int(error_fd, -errno);
      raise(SIGKILL);
    }

    if (fd <= 2) processx__nonblock_fcntl(fd, 0);

    /* If we have an extra fd, that we already dup2()-d into fd,
       we can close it now. */
    if (close_fd >= stdio_count) close(close_fd);
  }

  for (fd = min_fd; fd < stdio_count; fd++) {
    use_fd = pipes[fd][1];
    if (use_fd >= stdio_count) close(use_fd);
  }

  for (i = stdio_count; i < error_fd; i++) {
    close(i);
  }
  for (i = error_fd + 1; ; i++) {
    if (-1 == close(i) && i > 200) break;
  }

  if (options->wd != NULL && chdir(options->wd)) {
    processx__write_int(error_fd, - errno);
    raise(SIGKILL);
  }

  if (env) environ = env;

  if (putenv(strdup(tree_id))) {
    processx__write_int(error_fd, - errno);
    raise(SIGKILL);
  }

  execvp(command, args);
  processx__write_int(error_fd, - errno);
  raise(SIGKILL);
}

/* LCOV_EXCL_STOP */

void processx__finalizer(SEXP status) {
  processx_handle_t *handle = (processx_handle_t*) R_ExternalPtrAddr(status);
  pid_t pid;
  int wp, wstat;

  processx__block_sigchld();

  /* Free child list nodes that are not needed any more. */
  processx__freelist_free();

  /* Already freed? */
  if (!handle) goto cleanup;

  pid = handle->pid;

  if (handle->cleanup) {
    /* Do a non-blocking waitpid() to see if it is running */
    do {
      wp = waitpid(pid, &wstat, WNOHANG);
    } while (wp == -1 && errno == EINTR);

    /* Maybe just waited on it? Then collect status */
    if (wp == pid) processx__collect_exit_status(status, wp, wstat);

    /* If it is running, we need to kill it, and wait for the exit status */
    if (wp == 0) {
      kill(-pid, SIGKILL);
      do {
	wp = waitpid(pid, &wstat, 0);
      } while (wp == -1 && errno == EINTR);
      processx__collect_exit_status(status, wp, wstat);
    }
  }

  /* Note: if no cleanup is requested, then we still have a sigchld
     handler, to read out the exit code via waitpid, but no handle
     any more. */

  /* Deallocate memory */
  R_ClearExternalPtr(status);
  processx__handle_destroy(handle);

 cleanup:
  processx__unblock_sigchld();
}

static SEXP processx__make_handle(SEXP private, int cleanup) {
  processx_handle_t * handle;
  SEXP result;

  handle = (processx_handle_t*) malloc(sizeof(processx_handle_t));
  if (!handle) { R_THROW_ERROR("Cannot make processx handle, out of memory"); }
  memset(handle, 0, sizeof(processx_handle_t));
  handle->waitpipe[0] = handle->waitpipe[1] = -1;

  result = PROTECT(R_MakeExternalPtr(handle, private, R_NilValue));
  R_RegisterCFinalizerEx(result, processx__finalizer, 1);
  handle->cleanup = cleanup;

  UNPROTECT(1);
  return result;
}

static void processx__handle_destroy(processx_handle_t *handle) {
  if (!handle) return;
  free(handle);
}

void processx__make_socketpair(int pipe[2], const char *exe) {
#if defined(__linux__)
  static int no_cloexec;
  if (no_cloexec)  goto skip;

  if (socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, pipe) == 0)
    return;

  /* Retry on EINVAL, it means SOCK_CLOEXEC is not supported.
   * Anything else is a genuine error.
   */
  if (errno != EINVAL) {
    R_THROW_SYSTEM_ERROR("processx socketpair");
  }

  no_cloexec = 1;

skip:
#endif

  if (socketpair(AF_UNIX, SOCK_STREAM, 0, pipe)) {
    if (exe) {
      R_THROW_SYSTEM_ERROR("cannot make processx socketpair while "
                           "running '%s'", exe);
    } else {
      R_THROW_SYSTEM_ERROR("cannot make processx socketpair");
    }
  }

  processx__cloexec_fcntl(pipe[0], 1);
  processx__cloexec_fcntl(pipe[1], 1);
}

SEXP processx_exec(SEXP command, SEXP args, SEXP pty, SEXP pty_options,
                   SEXP connections, SEXP env, SEXP windows_verbatim_args,
                   SEXP windows_hide_window, SEXP windows_detached_process,
                   SEXP private, SEXP cleanup, SEXP wd, SEXP encoding,
                   SEXP tree_id) {

  char *ccommand = processx__tmp_string(command, 0);
  char **cargs = processx__tmp_character(args);
  char **cenv = isNull(env) ? 0 : processx__tmp_character(env);
  int ccleanup = INTEGER(cleanup)[0];

  const int cpty = LOGICAL(pty)[0];
  const char *cencoding = CHAR(STRING_ELT(encoding, 0));
  const char *ctree_id = CHAR(STRING_ELT(tree_id, 0));
  processx_options_t options = { 0 };
  int num_connections = LENGTH(connections);

  pid_t pid;
  int err, exec_errorno = 0, status;
  ssize_t r;
  int signal_pipe[2] = { -1, -1 };
  int (*pipes)[2];
  int i;
  int pty_main_fd;
#define R_PROCESSX_PTY_NAME_LEN 2014
  char pty_namex[R_PROCESSX_PTY_NAME_LEN];
  char *pty_name = cpty ? pty_namex : 0;

  processx_handle_t *handle = NULL;
  SEXP result;

  pipes = (int(*)[2]) R_alloc(num_connections, sizeof(int) * 2);
  for (i = 0; i < num_connections; i++) pipes[i][0] = pipes[i][1] = -1;

  options.wd = isNull(wd) ? 0 : CHAR(STRING_ELT(wd, 0));

  if (pipe(signal_pipe)) {
    R_THROW_SYSTEM_ERROR("Cannot create pipe when running '%s'", ccommand);
  }
  processx__cloexec_fcntl(signal_pipe[0], 1);
  processx__cloexec_fcntl(signal_pipe[1], 1);

  processx__setup_sigchld();

  result = PROTECT(processx__make_handle(private, ccleanup));
  handle = R_ExternalPtrAddr(result);

  if (cpty) {
    pty_main_fd =
      processx__pty_main_open(pty_name, R_PROCESSX_PTY_NAME_LEN);
    if (pty_main_fd == -1) {
      R_THROW_SYSTEM_ERROR("Cannot open pty when running '%s'", ccommand);
    }
    options.pty_echo = LOGICAL(VECTOR_ELT(pty_options, 0))[0];
    options.pty_rows = INTEGER(VECTOR_ELT(pty_options, 1))[0];
    options.pty_cols = INTEGER(VECTOR_ELT(pty_options, 2))[0];
  }

  handle->fd0 = handle->fd1 = handle->fd2 = -1;
  for (i = 0; i < num_connections; i++) {
    SEXP output = VECTOR_ELT(connections, i);
    const char *stroutput =
      Rf_isString(output) ? CHAR(STRING_ELT(output, 0)) : NULL;

    if (isNull(output)) {
      /* Ignored output, nothing to do, handled in the child */

    } else if (stroutput && ! strcmp("|", stroutput)) {
      /* pipe, need to create */
      processx__make_socketpair(pipes[i], ccommand);
      if (i == 0) handle->fd0 = pipes[i][0];
      if (i == 1) handle->fd1 = pipes[i][0];
      if (i == 2) handle->fd2 = pipes[i][0];
      processx__nonblock_fcntl(pipes[i][0], 1);

    } else if (i == 2 && stroutput && ! strcmp("2>&1", stroutput)) {
      /* redirected stderr, handled in child */

    } else if (stroutput && ! strcmp("", stroutput)) {
      /* inherited std stream, assume usual numbers */
      pipes[i][1] = i;

    } else if (stroutput) {
      /* redirect to file, nothing to do the child will open it */

    } else {
      /* inherited processx connection, need to duplicate */
      processx_connection_t *ccon =
	R_ExternalPtrAddr(VECTOR_ELT(connections, i));
      int fd = processx_c_connection_fileno(ccon);
      pipes[i][1] = fd;
    }
  }

  processx__block_sigchld();

  pid = fork();

  /* TODO: how could we test a failure? */
  if (pid == -1) {		/* ERROR */
    err = -errno;
    if (signal_pipe[0] >= 0) close(signal_pipe[0]);
    if (signal_pipe[1] >= 0) close(signal_pipe[1]);
    if (cpty) close(pty_main_fd);
    processx__unblock_sigchld();
    R_THROW_SYSTEM_ERROR_CODE(err, "Cannot fork when running '%s'",
                              ccommand);
  }

  /* CHILD */
  if (pid == 0) {
    /* LCOV_EXCL_START */
    if (cpty) close(pty_main_fd);
    processx__unblock_sigchld();
    processx__child_init(handle, connections, pipes, num_connections,
			 ccommand, cargs, signal_pipe[1],
                         pty_name, cenv, &options, ctree_id);
    R_THROW_SYSTEM_ERROR("Cannot start child process when running '%s'",
                         ccommand);
    /* LCOV_EXCL_STOP */
  }

  /* Query creation time ASAP. We'll use (pid, create_time) as an ID,
     to avoid race conditions when sending signals */
  handle->create_time = processx__create_time(pid);

  handle->ptyfd = -1;
  if (cpty) handle->ptyfd = pty_main_fd;

  /* We need to know the processx children */
  if (processx__child_add(pid, result)) {
    err = -errno;
    if (signal_pipe[0] >= 0) close(signal_pipe[0]);
    if (signal_pipe[1] >= 0) close(signal_pipe[1]);
    processx__unblock_sigchld();
    R_THROW_ERROR("Cannot create child process '%s', out of memory",
                  ccommand);
  }

  /* SIGCHLD can arrive now */
  processx__unblock_sigchld();

  if (signal_pipe[1] >= 0) close(signal_pipe[1]);

  do {
    r = read(signal_pipe[0], &exec_errorno, sizeof(exec_errorno));
  } while (r == -1 && errno == EINTR);

  if (r == 0) {
    ; /* okay, EOF */
  } else if (r == sizeof(exec_errorno)) {
    do {
      err = waitpid(pid, &status, 0); /* okay, read errorno */
    } while (err == -1 && errno == EINTR);

  } else if (r == -1 && errno == EPIPE) {
    do {
      err = waitpid(pid, &status, 0); /* okay, got EPIPE */
    } while (err == -1 && errno == EINTR);

  } else {
    R_THROW_SYSTEM_ERROR_CODE(-exec_errorno,
                              "Child process '%s' failed to start",
                              ccommand);
  }

  if (signal_pipe[0] >= 0) close(signal_pipe[0]);

  /* Closed unused ends of std pipes. If there is no parent end, then
     this is an inherited std{in,out,err} fd, so we should not close it. */
  for (i = 0; i < 3; i++) {
    if (pipes[i][1] >= 0 && pipes[i][0] >= 0) close(pipes[i][1]);
  }

  /* Create proper connections */
  processx__create_connections(handle, private, cencoding);

  if (exec_errorno == 0) {
    handle->pid = pid;
    UNPROTECT(1);		/* result */
    return result;
  }

  R_THROW_SYSTEM_ERROR_CODE(-exec_errorno,
                            "cannot start processx process '%s'",
                            ccommand);
  return R_NilValue;
}

void processx__collect_exit_status(SEXP status, int retval, int wstat) {
  processx_handle_t *handle = R_ExternalPtrAddr(status);

  /* This must be called from a function that blocks SIGCHLD.
     So we are not blocking it here. */

  if (!handle) {
    R_THROW_ERROR("Invalid handle, already finalized");
  }

  if (handle->collected) { return; }

  /* If waitpid returned -1, then an error happened, e.g. ECHILD, because
     another SIGCHLD handler collected the exit status already. */
  if (retval == -1) {
    handle->exitcode = NA_INTEGER;
  } else if (WIFEXITED(wstat)) {
    handle->exitcode = WEXITSTATUS(wstat);
  } else {
    handle->exitcode = - WTERMSIG(wstat);
  }

  handle->collected = 1;
}

static void processx__wait_cleanup(void *ptr) {
  int *fds = (int*) ptr;
  if (!fds) return;
  if (fds[0] >= 0) close(fds[0]);
  if (fds[1] >= 0) close(fds[1]);
  free(fds);
}

/* In general we need to worry about three asynchronous processes here:
 * 1. The main code, i.e. the code in this function.
 * 2. The finalizer, that can be triggered by any R function.
 *    A good strategy is to avoid calling R functions here completely.
 *    Functions that return immediately, like `R_CheckUserInterrupt`, or
 *    a `ScalarLogical` that we return, are fine.
 * 3. The SIGCHLD handler that we just block at the beginning, but it can
 *    still be called between the R function doing the `.Call` to us, and
 *    the signal blocking call.
 *
 * Keeping these in mind, we do this:
 *
 * 1. If the exit status was copied over to R already, we return
 *    immediately from R. Otherwise this C function is called.
 * 2. We block SIGCHLD.
 * 3. If we already collected the exit status, then this process has
 *    finished, so we don't need to wait.
 * 4. We set up a self-pipe that we can poll. The pipe will be closed in
 *    the SIGCHLD signal handler, and that triggers the poll event.
 * 5. We unblock the SIGCHLD handler, so that it can trigger the pipe event.
 * 6. We start polling. We poll in small time chunks, to keep the wait still
 *    interruptible.
 * 7. We keep polling until the timeout expires or the process finishes.
 */

SEXP processx_wait(SEXP status, SEXP timeout, SEXP name) {
  processx_handle_t *handle = R_ExternalPtrAddr(status);
  const char *cname = isNull(name) ? "???" : CHAR(STRING_ELT(name, 0));
  int ctimeout = INTEGER(timeout)[0], timeleft = ctimeout;
  struct pollfd fd;
  int ret = 0;
  pid_t pid;

  int *fds = malloc(sizeof(int) * 2);
  if (!fds) R_THROW_SYSTEM_ERROR("Allocating memory when waiting");
  fds[0] = fds[1] = -1;
  r_call_on_exit(processx__wait_cleanup, fds);

  processx__block_sigchld();

  if (!handle) {
    processx__unblock_sigchld();
    return ScalarLogical(1);
  }

  pid = handle->pid;

  /* If we already have the status, then return now. */
  if (handle->collected) {
    processx__unblock_sigchld();
    return ScalarLogical(1);
  }

  /* Make sure this is active, in case another package replaced it... */
  processx__setup_sigchld();
  processx__block_sigchld();

  /* Setup the self-pipe that we can poll */
  if (pipe(handle->waitpipe)) {
    processx__unblock_sigchld();
    R_THROW_SYSTEM_ERROR("processx error when waiting for '%s'", cname);
  }
  fds[0] = handle->waitpipe[0];
  fds[1] = handle->waitpipe[1];
  processx__nonblock_fcntl(handle->waitpipe[0], 1);
  processx__nonblock_fcntl(handle->waitpipe[1], 1);

  /* Poll on the pipe, need to unblock sigchld before */
  fd.fd = handle->waitpipe[0];
  fd.events = POLLIN;
  fd.revents = 0;

  processx__unblock_sigchld();



  while (ctimeout < 0 || timeleft > PROCESSX_INTERRUPT_INTERVAL) {
    do {
      ret = poll(&fd, 1, PROCESSX_INTERRUPT_INTERVAL);
    } while (ret == -1 && errno == EINTR);

    /* If not a timeout, then we are done */
    if (ret != 0) break;

    R_CheckUserInterrupt();

    /* We also check if the process is alive, because the SIGCHLD is
       not delivered in valgrind :( This also works around the issue
       of SIGCHLD handler interference, i.e. if another package (like
       parallel) removes our signal handler. */
    ret = kill(pid, 0);
    if (ret != 0) {
      ret = 1;
      goto cleanup;
    }

    if (ctimeout >= 0) timeleft -= PROCESSX_INTERRUPT_INTERVAL;
  }

  /* Maybe we are not done, and there is a little left from the timeout */
  if (ret == 0 && timeleft >= 0) {
    do {
      ret = poll(&fd, 1, timeleft);
    } while (ret == -1 && errno == EINTR);
  }

  if (ret == -1) {
    R_THROW_SYSTEM_ERROR("processx wait with timeout error while "
                         "waiting for '%s'", cname);
  }

 cleanup:
  /* pipe is closed in the on_exit handler */
  handle->waitpipe[0] = -1;
  handle->waitpipe[1] = -1;

  return ScalarLogical(ret != 0);
}

/* This is similar to `processx_wait`, but a bit simpler, because we
 * don't need to wait and poll. The same restrictions listed there, also
 * apply here.
 *
 * 1. If the exit status was copied over to R already, we return
 *    immediately from R. Otherwise this C function is called.
 * 2. We block SIGCHLD.
 * 3. If we already collected the exit status, then this process has
 *    finished, and we return FALSE.
 * 4. Otherwise we do a non-blocking `waitpid`, because the process might
 *    have finished, we just haven't collected its exit status yet.
 * 5. If the process is still running, `waitpid` returns 0. We return TRUE.
 * 6. Otherwise we collect the exit status, and return FALSE.
 */

SEXP processx_is_alive(SEXP status, SEXP name) {
  processx_handle_t *handle = R_ExternalPtrAddr(status);
  const char *cname = isNull(name) ? "???" : CHAR(STRING_ELT(name, 0));
  pid_t pid;
  int wstat, wp;
  int ret = 0;

  processx__block_sigchld();

  if (!handle) goto cleanup;
  if (handle->collected) goto cleanup;

  /* Otherwise a non-blocking waitpid to collect zombies */
  pid = handle->pid;
  do {
    wp = waitpid(pid, &wstat, WNOHANG);
  } while (wp == -1 && errno == EINTR);

  /* Maybe another SIGCHLD handler collected the exit status?
     Then we just set it to NA (in the collect_exit_status call) */
  if (wp == -1 && errno == ECHILD) {
    processx__collect_exit_status(status, wp, wstat);
    goto cleanup;
  }

  /* Some other error? */
  if (wp == -1) {
    processx__unblock_sigchld();
    R_THROW_SYSTEM_ERROR("processx_is_alive, process '%s'", cname);
  }

  /* If running, return TRUE, otherwise collect exit status, return FALSE */
  if (wp == 0) {
    ret = 1;
  } else {
    processx__collect_exit_status(status, wp, wstat);
  }

 cleanup:
  processx__unblock_sigchld();
  return ScalarLogical(ret);
}

/* This is essentially the same as `processx_is_alive`, but we return an
 * exit status if the process has already finished. See above.
 */

SEXP processx_get_exit_status(SEXP status, SEXP name) {
  processx_handle_t *handle = R_ExternalPtrAddr(status);
  const char *cname = isNull(name) ? "???" : CHAR(STRING_ELT(name, 0));
  pid_t pid;
  int wstat, wp;
  SEXP result;

  processx__block_sigchld();

  if (!handle) {
    result = PROTECT(ScalarInteger(NA_INTEGER));
    goto cleanup;
  }

  /* If we already have the status, then just return */
  if (handle->collected) {
    result = PROTECT(ScalarInteger(handle->exitcode));
    goto cleanup;
  }

  /* Otherwise do a non-blocking waitpid to collect zombies */
  pid = handle->pid;
  do {
    wp = waitpid(pid, &wstat, WNOHANG);
  } while (wp == -1 && errno == EINTR);

  /* Another SIGCHLD handler already collected the exit code?
     Then we set it to NA (in the collect_exit_status call). */
  if (wp == -1 && errno == ECHILD) {
    processx__collect_exit_status(status, wp, wstat);
    result = PROTECT(ScalarInteger(handle->exitcode));
    goto cleanup;
  }

  /* Some other error? */
  if (wp == -1) {
    processx__unblock_sigchld();
    R_THROW_SYSTEM_ERROR("processx_get_exit_status error for '%s'", cname);
  }

  /* If running, do nothing otherwise collect */
  if (wp == 0) {
    result = PROTECT(R_NilValue);
  } else {
    processx__collect_exit_status(status, wp, wstat);
    result = PROTECT(ScalarInteger(handle->exitcode));
  }

 cleanup:
  processx__unblock_sigchld();
  UNPROTECT(1);
  return result;
}

/* See `processx_wait` above for the description of async processes and
 * possible race conditions.
 *
 * This is mostly along the lines of `processx_is_alive`. After we
 * successfully sent the signal, we try a `waitpid` just in case the
 * processx has aborted on it. This is a harmless race condition, because
 * the process might not have been cleaned up yet, when we call `waitpid`,
 * but that's OK, then its exit status will be collected later, e.g. in
 * the SIGCHLD handler.
 */

SEXP processx_signal(SEXP status, SEXP signal, SEXP name) {
  processx_handle_t *handle = R_ExternalPtrAddr(status);
  const char *cname = isNull(name) ? "???" : CHAR(STRING_ELT(name, 0));
  pid_t pid;
  int wstat, wp, ret, result;

  processx__block_sigchld();

  if (!handle) {
    result = 0;
    goto cleanup;
  }

  /* If we already have the status, then return `FALSE` */
  if (handle->collected) {
    result = 0;
    goto cleanup;
  }

  /* Otherwise try to send signal */
  pid = handle->pid;
  ret = kill(pid, INTEGER(signal)[0]);

  if (ret == 0) {
    result = 1;
  } else if (ret == -1 && errno == ESRCH) {
    result = 0;
  } else {
    processx__unblock_sigchld();
    R_THROW_SYSTEM_ERROR("processx_signal for '%s'", cname);
    return R_NilValue;
  }

  /* Possibly dead now, collect status */
  do {
    wp = waitpid(pid, &wstat, WNOHANG);
  } while (wp == -1 && errno == EINTR);

  /* Maybe another SIGCHLD handler collected it already? */
  if (wp == -1 && errno == ECHILD) {
    processx__collect_exit_status(status, wp, wstat);
    goto cleanup;
  }

  if (wp == -1) {
    processx__unblock_sigchld();
    R_THROW_SYSTEM_ERROR("processx_signal for '%s'", cname);
  }

 cleanup:
  processx__unblock_sigchld();
  return ScalarLogical(result);
}

SEXP processx_interrupt(SEXP status, SEXP name) {
  return processx_signal(status, ScalarInteger(2), name);
}

/* This is a special case of `processx_signal`, and we implement it almost
 * the same way. We make an effort to return a TRUE/FALSE value to indicate
 * if the process died as a response to our KILL signal. This is not 100%
 * accurate because of the unavoidable race conditions. (E.g. it might have
 * been killed by another process's KILL signal.)
 *
 * To do a better job for the return value, we call a `waitpid` before
 * delivering the signal, as a final check to see if the child process is
 * still alive or not.
 */

SEXP processx_kill(SEXP status, SEXP grace, SEXP name) {
  processx_handle_t *handle = R_ExternalPtrAddr(status);
  const char *cname = isNull(name) ? "???" : CHAR(STRING_ELT(name, 0));
  pid_t pid;
  int wstat, wp, result = 0;

  processx__block_sigchld();

  if (!handle) { goto cleanup; }

  /* Check if we have an exit status, it yes, just return (FALSE) */
  if (handle->collected) { goto cleanup; }

  /* Do a non-blocking waitpid to collect zombies */
  pid = handle->pid;
  do {
    wp = waitpid(pid, &wstat, WNOHANG);
  } while (wp == -1 && errno == EINTR);

  /* The child does not exist any more, set exit status to NA &
     return FALSE. */
  if (wp == -1 && errno == ECHILD) {
    processx__collect_exit_status(status, wp, wstat);
    goto cleanup;
  }

  /* Some other error? */
  if (wp == -1) {
    processx__unblock_sigchld();
    R_THROW_SYSTEM_ERROR("processx_kill for '%s'", cname);
  }

  /* If the process is not running, return (FALSE) */
  if (wp != 0) { goto cleanup; }

  /* It is still running, so a SIGKILL */
  int ret = kill(-pid, SIGKILL);
  if (ret == -1 && (errno == ESRCH || errno == EPERM)) { goto cleanup; }
  if (ret == -1) {
    processx__unblock_sigchld();
    R_THROW_SYSTEM_ERROR("process_kill for '%s'", cname);
  }

  /* Do a waitpid to collect the status and reap the zombie */
  do {
    wp = waitpid(pid, &wstat, 0);
  } while (wp == -1 && errno == EINTR);

  /* Collect exit status, and check if it was killed by a SIGKILL
     If yes, this was most probably us (although we cannot be sure in
     general...
     If the status was collected by another SIGCHLD, then the exit
     status will be set to NA */
  processx__collect_exit_status(status, wp, wstat);
  result = handle->exitcode == - SIGKILL;

 cleanup:
  processx__unblock_sigchld();
  return ScalarLogical(result);
}

SEXP processx_get_pid(SEXP status) {
  processx_handle_t *handle = R_ExternalPtrAddr(status);

  /* This might happen if it was finalized at the end of the session,
     even though there are some references to the R object. */
  if (!handle) return ScalarInteger(NA_INTEGER);

  return ScalarInteger(handle->pid);
}

/* We send a 0 signal to check if the process is alive. Note that a process
 * that is in a zombie state also counts as 'alive' with this method.
*/

SEXP processx__process_exists(SEXP pid) {
  pid_t cpid = INTEGER(pid)[0];
  int res = kill(cpid, 0);
  if (res == 0) {
    return ScalarLogical(1);
  } else if (errno == ESRCH) {
    return ScalarLogical(0);
  } else {
    R_THROW_SYSTEM_ERROR("kill syscall error for pid '%d'", cpid);
    return R_NilValue;
  }
}

#endif
