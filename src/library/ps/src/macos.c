
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <utmpx.h>
#include <sys/sysctl.h>
#include <sys/vmmeter.h>
#include <libproc.h>
#include <sys/proc_info.h>
#include <netinet/tcp_fsm.h>
#include <arpa/inet.h>
#include <net/if_dl.h>
#include <pwd.h>

#include <mach/mach.h>
#include <mach/task.h>
#include <mach/mach_init.h>
#include <mach/host_info.h>
#include <mach/mach_host.h>
#include <mach/mach_traps.h>
#include <mach/mach_vm.h>
#include <mach/shared_region.h>

#include <mach-o/loader.h>

#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h>
#include <IOKit/storage/IOBlockStorageDriver.h>
#include <IOKit/storage/IOMedia.h>
#include <IOKit/IOBSD.h>
#include <IOKit/ps/IOPowerSources.h>
#include <IOKit/ps/IOPSKeys.h>

#include "common.h"
#include "posix.h"
#include "arch/macos/process_info.h"


#define PS__TV2DOUBLE(t) ((t).tv_sec + (t).tv_usec / 1000000.0)

/*
 * Return an integer vector of all the PIDs running on the system.
 */
SEXP ps__pids(void) {
  kinfo_proc *proclist = NULL;
  kinfo_proc *orig_address = NULL;
  size_t num_processes;
  size_t idx;
  SEXP retlist = R_NilValue;

  if (ps__get_proc_list(&proclist, &num_processes) != 0) {
    if (errno != 0) {
      ps__set_error_from_errno();
    } else {
      ps__set_error("failed to retrieve process list");
    }
    goto error;
  }

  retlist = PROTECT(allocVector(INTSXP, num_processes));

  if (num_processes > 0) {
    // save the address of proclist so we can free it later
    orig_address = proclist;
    for (idx = 0; idx < num_processes; idx++) {
      INTEGER(retlist)[idx] = proclist->kp_proc.p_pid;
      proclist++;
    }
    free(orig_address);
  }

  UNPROTECT(1);
  return retlist;

 error:
  if (orig_address != NULL) free(orig_address);
  ps__throw_error();
  return R_NilValue;
}

SEXP ps__define_tcp_statuses(void) {
  SEXP result, names;

  PROTECT(result = allocVector(INTSXP, 12));
  PROTECT(names = allocVector(STRSXP, 12));

  INTEGER(result)[0] = TCPS_CLOSED;
  SET_STRING_ELT(names, 0, mkChar("CONN_CLOSE"));
  INTEGER(result)[1] = TCPS_CLOSING;
  SET_STRING_ELT(names, 1, mkChar("CONN_CLOSING"));
  INTEGER(result)[2] = TCPS_CLOSE_WAIT;
  SET_STRING_ELT(names, 2, mkChar("CONN_CLOSE_WAIT"));
  INTEGER(result)[3] = TCPS_LISTEN;
  SET_STRING_ELT(names, 3, mkChar("CONN_LISTEN"));
  INTEGER(result)[4] = TCPS_ESTABLISHED;
  SET_STRING_ELT(names, 4, mkChar("CONN_ESTABLISHED"));
  INTEGER(result)[5] = TCPS_SYN_SENT;
  SET_STRING_ELT(names, 5, mkChar("CONN_SYN_SENT"));
  INTEGER(result)[6] = TCPS_SYN_RECEIVED;
  SET_STRING_ELT(names, 6, mkChar("CONN_SYN_RECV"));
  INTEGER(result)[7] = TCPS_FIN_WAIT_1;
  SET_STRING_ELT(names, 7, mkChar("CONN_FIN_WAIT_1"));
  INTEGER(result)[8] = TCPS_FIN_WAIT_2;
  SET_STRING_ELT(names, 8, mkChar("CONN_FIN_WAIT_2"));
  INTEGER(result)[9] = TCPS_LAST_ACK;
  SET_STRING_ELT(names, 9, mkChar("CONN_LAST_ACK"));
  INTEGER(result)[10] = TCPS_TIME_WAIT;
  SET_STRING_ELT(names, 10, mkChar("CONN_TIME_WAIT"));
  INTEGER(result)[11] = PS__CONN_NONE;
  SET_STRING_ELT(names, 11, mkChar("PS__CONN_NONE"));

  setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

SEXP ps__init(SEXP psenv, SEXP constenv) {

  /* Signals */
  defineVar(install("signals"), ps__define_signals(), constenv);

  /* errno values */
  defineVar(install("errno"), ps__define_errno(), constenv);

  /* Connection statuses */
  defineVar(install("tcp_statuses"), ps__define_tcp_statuses(), constenv);

  /* Socket address families */
  defineVar(install("address_families"),
	    ps__define_socket_address_families(), constenv);

  /* Socket address families */
  defineVar(install("socket_types"), ps__define_socket_types(), constenv);

  return R_NilValue;
}
