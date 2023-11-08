
#include "../processx.h"

#include <errno.h>
#include <pthread.h>

extern processx__child_list_t *child_list;

static struct sigaction old_sig_handler = {{ 0 }};
int processx__notify_old_sigchld_handler = 0;
pthread_t processx__main_thread = { 0 };

void processx__sigchld_callback(int sig, siginfo_t *info, void *ctx) {

  int saved_errno = errno;
  /* This might be called on another thread, if the main thread blocks
     the signal temporarily, so we forward it to the main thread.
     It is OK to call pthread_self() and pthread_kill() in the signal
     handler, according to POSIX.1-2008 TC1.
     https://man7.org/linux/man-pages/man7/signal-safety.7.html */
  if (pthread_self() != processx__main_thread) {
    pthread_kill(processx__main_thread, SIGCHLD);
    errno = saved_errno;
    return;
  }

  /* This should really not happem, but just in case. */
  if (sig != SIGCHLD) {
    errno = saved_errno;
    return;
  }

  /* While we get a pid in info, this is basically useless, as
     (on some platforms at least) a single signal might be delivered
     for multiple children exiting around the same time. For example this
     happens if multiple SIGCHLD signals arrive while SIGCHLD is blocked.
     So we need to iterate over all children to see which one has exited. */

  processx__child_list_t *ptr = child_list->next;
  processx__child_list_t *prev = child_list;

  while (ptr) {
    processx__child_list_t *next = ptr->next;
    int wp, wstat;

    /* Check if this child has exited */
    do {
      wp = waitpid(ptr->pid, &wstat, WNOHANG);
    } while (wp == -1 && errno == EINTR);

    if (wp == 0 || (wp < 0 && errno != ECHILD)) {
      /* If it is still running (or an error, other than ECHILD happened),
        we do nothing */
      prev = ptr;
      ptr = next;

    } else {
      /* Remove the child from the list */

      /* We deliberately do not call the finalizer here, because that
	 moves the exit code and pid to R, and we might have just checked
	 that these are not in R, before calling C. So finalizing here
	 would be a race condition.

	 OTOH, we need to check if the handle is null, because a finalizer
	 might actually run before the SIGCHLD handler. Or the finalizer
	 might even trigger the SIGCHLD handler...
      */

      SEXP status = R_WeakRefKey(ptr->weak_status);
      processx_handle_t *handle =
	isNull(status) ? 0 : R_ExternalPtrAddr(status);

      /* If waitpid errored with ECHILD, then the exit status is set to NA */
      if (handle) processx__collect_exit_status(status, wp, wstat);

      /* Defer freeing the memory, because malloc/free are typically not
	 reentrant, and if we free in the SIGCHLD handler, that can cause
	 crashes. The test case in test-run.R (see comments there)
	 typically brings this out. */
      processx__freelist_add(ptr);

      /* If there is an active wait() with a timeout, then stop it */
      if (handle && handle->waitpipe[1] >= 0) {
	close(handle->waitpipe[1]);
	handle->waitpipe[1] = -1;
      }

      /* If we remove the current list node, then prev stays the same,
	 we only need to update ptr. */
      prev->next = next;
      ptr = next;
    }
  }

  if (processx__notify_old_sigchld_handler) {
    if (old_sig_handler.sa_handler != SIG_DFL &&
        old_sig_handler.sa_handler != SIG_IGN &&
        old_sig_handler.sa_handler != NULL) {
      if (old_sig_handler.sa_flags | SA_SIGINFO) {
        old_sig_handler.sa_sigaction(sig, info, NULL);
      } else {
        old_sig_handler.sa_handler(sig);
      }
    }
  }

  errno = saved_errno;
}

void processx__setup_sigchld(void) {
  struct sigaction action;
  struct sigaction old;
  memset(&action, 0, sizeof(action));
  action.sa_sigaction = processx__sigchld_callback;
  action.sa_flags = SA_SIGINFO | SA_RESTART | SA_NOCLDSTOP;
  sigaction(SIGCHLD, &action, &old);
  if (old.sa_sigaction != processx__sigchld_callback) {
    memcpy(&old_sig_handler, &old, sizeof(old));
  }
}

void processx__remove_sigchld(void) {
  struct sigaction action;
  memset(&action, 0, sizeof(action));
  action.sa_handler = SIG_DFL;
  sigaction(SIGCHLD, &action, &old_sig_handler);
  memset(&old_sig_handler, 0, sizeof(old_sig_handler));
}

void processx__block_sigchld(void) {
  sigset_t blockMask;
  sigemptyset(&blockMask);
  sigaddset(&blockMask, SIGCHLD);
  if (sigprocmask(SIG_BLOCK, &blockMask, NULL) == -1) {
    R_THROW_ERROR("processx error setting up signal handlers");
  }
}

void processx__unblock_sigchld(void) {
  sigset_t unblockMask;
  sigemptyset(&unblockMask);
  sigaddset(&unblockMask, SIGCHLD);
  if (sigprocmask(SIG_UNBLOCK, &unblockMask, NULL) == -1) {
    R_THROW_ERROR("processx error setting up signal handlers");
  }
}
