
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <errno.h>
#include <stdlib.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <string.h>
#include <unistd.h>
#include <pwd.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include "common.h"

#ifdef PS__SUNOS10
#include "arch/solaris/v10/ifaddrs.h"
#elif PS__AIX
#include "arch/aix/ifaddrs.h"
#else
#include <ifaddrs.h>
#endif

#if defined(PS__LINUX)
#include <netdb.h>
#include <linux/types.h>
#include <linux/if_packet.h>
#elif defined(PS__BSD) || defined(PS__MACOS)
#include <netdb.h>
#include <netinet/in.h>
#include <net/if_dl.h>
#include <sys/sockio.h>
#include <net/if_media.h>
#include <net/if.h>
#elif defined(PS__SUNOS)
#include <netdb.h>
#include <sys/sockio.h>
#elif defined(PS__AIX)
#include <netdb.h>
#endif


/*
 * Check if PID exists. Return values:
 * 1: exists
 * 0: does not exist
 * -1: error (exception is set)
 */
int ps__pid_exists(long pid) {
  int ret;

  // No negative PID exists, plus -1 is an alias for sending signal
  // too all processes except system ones. Not what we want.
  if (pid < 0)
    return 0;

  // As per "man 2 kill" PID 0 is an alias for sending the signal to
  // every process in the process group of the calling process.
  // Not what we want. Some platforms have PID 0, some do not.
  // We decide that at runtime.
  if (pid == 0) {
#if defined(PS__LINUX) || defined(PS__FREEBSD)
    return 0;
#else
    return 1;
#endif
  }

#if defined(PS__MACOS)
  ret = kill((pid_t)pid , 0);
#else
  ret = kill(pid , 0);
#endif

  if (ret == 0)
    return 1;
  else {
    if (errno == ESRCH) {
      // ESRCH == No such process
      return 0;
    }
    else if (errno == EPERM) {
      // EPERM clearly indicates there's a process to deny
      // access to.
      return 1;
    }
    else {
      // According to "man 2 kill" possible error values are
      // (EINVAL, EPERM, ESRCH) therefore we should never get
      // here. If we do let's be explicit in considering this
      // an error.
      ps__set_error_from_errno();
      return -1;
    }
  }
}

SEXP psp__pid_exists(SEXP r_pid) {
  return ScalarLogical(ps__pid_exists(INTEGER(r_pid)[0]));
}

/*
 * Utility used for those syscalls which do not return a meaningful
 * error that we can translate into an exception which makes sense.
 * As such, we'll have to guess.
 * On UNIX, if errno is set, we return that one (OSError).
 * Else, if PID does not exist we assume the syscall failed because
 * of that so we raise NoSuchProcess.
 * If none of this is true we giveup and raise RuntimeError(msg).
 * This will always set an exception and return NULL.
 */
int ps__raise_for_pid(long pid, char *syscall_name) {
  // Set exception to AccessDenied if pid exists else NoSuchProcess.
  if (errno != 0) {
    // Unlikely we get here.
    ps__set_error_from_errno();
    return 0;
  }
  else if (ps__pid_exists(pid) == 0) {
    ps__debug("%s syscall failed and PID %i no longer exists; "
	      "assume NoSuchProcess", syscall_name, pid);
    ps__no_such_process(pid, 0);
  }
  else {
    ps__set_error("%s syscall failed", syscall_name);
  }
  return 0;
}

SEXP ps__get_pw_uid(SEXP r_uid) {
  struct passwd *pwd;
  errno = 0;
  pwd = getpwuid(INTEGER(r_uid)[0]);
  if (pwd == NULL) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  return ps__build_named_list(
    "ssiiss",
    "pw_name",   pwd->pw_name,
    "pw_passwd", pwd->pw_passwd,
    "pw_uid",    pwd->pw_uid,
    "pw_gid",    pwd->pw_gid,
    "pw_dir",    pwd->pw_dir,
    "pw_shell",  pwd->pw_shell);
}

SEXP psp__stat_st_rdev(SEXP files) {
  size_t i, len = LENGTH(files);
  struct stat buf;
  SEXP result;
  int ret;

  PROTECT(result = allocVector(INTSXP, len));

  for (i = 0; i < len; i++) {
    ret = stat(CHAR(STRING_ELT(files, i)), &buf);
    if (ret == -1) {
      if (errno == ENOENT) {
	INTEGER(result)[i] = 0;
      } else {
	ps__set_error_from_errno();
	ps__throw_error();
      }
    } else {
      INTEGER(result)[i] = (int) buf.st_rdev;
    }
  }

  UNPROTECT(1);
  return result;
}

pid_t ps__zombie(void) {
  pid_t child_pid;

  child_pid = fork();

  if (child_pid <= 0) raise(SIGKILL);

  return child_pid;
}

SEXP psp__zombie(void) {
  return ScalarInteger((int) ps__zombie());
}

int ps__waitpid(pid_t pid) {
  int wp, wstat;

  do {
    wp = waitpid(pid, &wstat, WNOHANG);
  } while (wp == -1 && errno == EINTR);

  if (wp == pid) {
    /* Get exit status */
    if (WIFEXITED(wstat)) {
      return WEXITSTATUS(wstat);
    } else {
      return - WTERMSIG(wstat);
    }

  } else if (wp == -1 && errno == ECHILD) {
    return NA_INTEGER;

  } else {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  return 0;
}

SEXP psp__waitpid(SEXP r_pid) {
  pid_t pid = INTEGER(r_pid)[0];
  return ScalarInteger(ps__waitpid(pid));
}
SEXP ps__define_signals(void) {

  SEXP signalenv = PROTECT(ps_new_env());

#define PS_ADD_SIGNAL(sig)						\
  defineVar(install(#sig), PROTECT(ScalarInteger(sig)), signalenv);	\
  UNPROTECT(1)

  /* macOS */

#ifdef SIGHUP
  PS_ADD_SIGNAL(SIGHUP);
#endif
#ifdef SIGINT
  PS_ADD_SIGNAL(SIGINT);
#endif
#ifdef SIGQUIT
  PS_ADD_SIGNAL(SIGQUIT);
#endif
#ifdef SIGILL
  PS_ADD_SIGNAL(SIGILL);
#endif
#ifdef SIGTRAP
  PS_ADD_SIGNAL(SIGTRAP);
#endif
#ifdef SIGABRT
  PS_ADD_SIGNAL(SIGABRT);
#endif
#ifdef SIGEMT
  PS_ADD_SIGNAL(SIGEMT);
#endif
#ifdef SIGFPE
  PS_ADD_SIGNAL(SIGFPE);
#endif
#ifdef SIGKILL
  PS_ADD_SIGNAL(SIGKILL);
#endif
#ifdef SIGBUS
  PS_ADD_SIGNAL(SIGBUS);
#endif
#ifdef SIGSEGV
  PS_ADD_SIGNAL(SIGSEGV);
#endif
#ifdef SIGSYS
  PS_ADD_SIGNAL(SIGSYS);
#endif
#ifdef SIGPIPE
  PS_ADD_SIGNAL(SIGPIPE);
#endif
#ifdef SIGALRM
  PS_ADD_SIGNAL(SIGALRM);
#endif
#ifdef SIGTERM
  PS_ADD_SIGNAL(SIGTERM);
#endif
#ifdef SIGURG
  PS_ADD_SIGNAL(SIGURG);
#endif
#ifdef SIGSTOP
  PS_ADD_SIGNAL(SIGSTOP);
#endif
#ifdef SIGTSTP
  PS_ADD_SIGNAL(SIGTSTP);
#endif
#ifdef SIGCONT
  PS_ADD_SIGNAL(SIGCONT);
#endif
#ifdef SIGCHLD
  PS_ADD_SIGNAL(SIGCHLD);
#endif
#ifdef SIGTTIN
  PS_ADD_SIGNAL(SIGTTIN);
#endif
#ifdef SIGTTOU
  PS_ADD_SIGNAL(SIGTTOU);
#endif
#ifdef SIGIO
  PS_ADD_SIGNAL(SIGIO);
#endif
#ifdef SIGXCPU
  PS_ADD_SIGNAL(SIGXCPU);
#endif
#ifdef SIGXFSZ
  PS_ADD_SIGNAL(SIGXFSZ);
#endif
#ifdef SIGVTALRM
  PS_ADD_SIGNAL(SIGVTALRM);
#endif
#ifdef SIGPROF
  PS_ADD_SIGNAL(SIGPROF);
#endif
#ifdef SIGWINCH
  PS_ADD_SIGNAL(SIGWINCH);
#endif
#ifdef SIGINFO
  PS_ADD_SIGNAL(SIGINFO);
#endif
#ifdef SIGUSR1
  PS_ADD_SIGNAL(SIGUSR1);
#endif
#ifdef SIGUSR2
  PS_ADD_SIGNAL(SIGUSR2);
#endif

  /* Linux */

#ifdef SIGPOLL
  PS_ADD_SIGNAL(SIGPOLL);
#endif
#ifdef SIGIOT
  PS_ADD_SIGNAL(SIGIOT);
#endif
#ifdef SIGSTKFLT
  PS_ADD_SIGNAL(SIGSTKFLT);
#endif
#ifdef SIGCLD
  PS_ADD_SIGNAL(SIGCLD);
#endif
#ifdef SIGPWR
  PS_ADD_SIGNAL(SIGPWR);
#endif
#ifdef SIGLOST
  PS_ADD_SIGNAL(SIGLOST);
#endif
#ifdef SIGUNUSED
  PS_ADD_SIGNAL(SIGUNUSED);
#endif

#undef PS_ADD_SIGNAL

  UNPROTECT(1);
  return signalenv;
}


SEXP ps__define_socket_address_families(void) {
  SEXP afenv = PROTECT(ps_new_env());

#define PS_ADD_AF(af)						\
  defineVar(install(#af), PROTECT(ScalarInteger(af)), afenv);	\
  UNPROTECT(1)

#ifdef AF_UNSPEC
  PS_ADD_AF(AF_UNSPEC);
#endif

#ifdef AF_INET
  PS_ADD_AF(AF_INET);
#endif

#if defined(AF_UNIX)
  PS_ADD_AF(AF_UNIX);
#endif

#ifdef AF_AX25
  /* Amateur Radio AX.25 */
  PS_ADD_AF(AF_AX25);
#endif
#ifdef AF_IPX
  PS_ADD_AF(AF_IPX); /* Novell IPX */
#endif
#ifdef AF_APPLETALK
  /* Appletalk DDP */
  PS_ADD_AF(AF_APPLETALK);
#endif
#ifdef AF_NETROM
  /* Amateur radio NetROM */
  PS_ADD_AF(AF_NETROM);
#endif
#ifdef AF_BRIDGE
  /* Multiprotocol bridge */
  PS_ADD_AF(AF_BRIDGE);
#endif
#ifdef AF_ATMPVC
  /* ATM PVCs */
  PS_ADD_AF(AF_ATMPVC);
#endif
#ifdef AF_AAL5
  /* Reserved for Werner's ATM */
  PS_ADD_AF(AF_AAL5);
#endif
#ifdef HAVE_SOCKADDR_ALG
  PS_ADD_AF(AF_ALG); /* Linux crypto */
#endif
#ifdef AF_X25
  /* Reserved for X.25 project */
  PS_ADD_AF(AF_X25);
#endif
#ifdef AF_INET6
  PS_ADD_AF(AF_INET6); /* IP version 6 */
#endif
#ifdef AF_ROSE
  /* Amateur Radio X.25 PLP */
  PS_ADD_AF(AF_ROSE);
#endif
#ifdef AF_DECnet
  /* Reserved for DECnet project */
  PS_ADD_AF(AF_DECnet);
#endif
#ifdef AF_NETBEUI
  /* Reserved for 802.2LLC project */
  PS_ADD_AF(AF_NETBEUI);
#endif
#ifdef AF_SECURITY
  /* Security callback pseudo AF */
  PS_ADD_AF(AF_SECURITY);
#endif
#ifdef AF_KEY
  /* PF_KEY key management API */
  PS_ADD_AF(AF_KEY);
#endif
#ifdef AF_NETLINK
  PS_ADD_AF(AF_NETLINK);
#endif /* AF_NETLINK */

#ifdef AF_VSOCK
  PS_ADD_AF(AF_VSOCK);
#endif

#ifdef AF_ROUTE
  /* Alias to emulate 4.4BSD */
  PS_ADD_AF(AF_ROUTE);
#endif
#ifdef AF_LINK
  PS_ADD_AF(AF_LINK);
#endif
#ifdef AF_ASH
  /* Ash */
  PS_ADD_AF(AF_ASH);
#endif
#ifdef AF_ECONET
  /* Acorn Econet */
  PS_ADD_AF(AF_ECONET);
#endif
#ifdef AF_ATMSVC
  /* ATM SVCs */
  PS_ADD_AF(AF_ATMSVC);
#endif
#ifdef AF_SNA
  /* Linux SNA Project (nutters!) */
  PS_ADD_AF(AF_SNA);
#endif
#ifdef AF_IRDA
  /* IRDA sockets */
  PS_ADD_AF(AF_IRDA);
#endif
#ifdef AF_PPPOX
  /* PPPoX sockets */
  PS_ADD_AF(AF_PPPOX);
#endif
#ifdef AF_WANPIPE
  /* Wanpipe API Sockets */
  PS_ADD_AF(AF_WANPIPE);
#endif
#ifdef AF_LLC
  /* Linux LLC */
  PS_ADD_AF(AF_LLC);
#endif

#ifdef USE_BLUETOOTH
  PS_ADD_AF(AF_BLUETOOTH);
#endif

#ifdef AF_CAN
  /* Controller Area Network */
  PS_ADD_AF(AF_CAN);
#endif

  /* Reliable Datagram Sockets */
#ifdef AF_RDS
  PS_ADD_AF(AF_RDS);
#endif

#ifdef AF_SYSTEM
  PS_ADD_AF(AF_SYSTEM);
#endif

#ifdef AF_PACKET
  PS_ADD_AF(AF_PACKET);
#endif

  UNPROTECT(1);
  return afenv;
}

SEXP ps__define_socket_types(void) {
  SEXP env = PROTECT(ps_new_env());

#define PS_ADD_SOCKET_TYPE(type)					\
  defineVar(install(#type), PROTECT(ScalarInteger(type)), env);		\
  UNPROTECT(1)

  PS_ADD_SOCKET_TYPE(SOCK_STREAM);
  PS_ADD_SOCKET_TYPE(SOCK_DGRAM);

#ifdef SOCK_RAW
    PS_ADD_SOCKET_TYPE(SOCK_RAW);
#endif
    PS_ADD_SOCKET_TYPE(SOCK_SEQPACKET);
#if defined(SOCK_RDM)
    PS_ADD_SOCKET_TYPE(SOCK_RDM);
#endif
#ifdef SOCK_CLOEXEC
    PS_ADD_SOCKET_TYPE(SOCK_CLOEXEC);
#endif
#ifdef SOCK_NONBLOCK
    PS_ADD_SOCKET_TYPE(SOCK_NONBLOCK);
#endif

  UNPROTECT(1);
  return env;
}
