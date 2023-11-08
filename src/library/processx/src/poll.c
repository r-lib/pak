
#include "processx.h"

SEXP processx_poll(SEXP statuses, SEXP types, SEXP ms) {
  int cms = INTEGER(ms)[0];
  int i, j, num_total = LENGTH(statuses);
  processx_pollable_t *pollables;
  SEXP result;
  int num_proc = 0, num_poll;

  for (i = 0; i < num_total; i++) if (INTEGER(types)[i] == 1) num_proc++;
  num_poll = num_total + num_proc * 2;

  pollables = (processx_pollable_t*)
    R_alloc(num_poll, sizeof(processx_pollable_t));

  result = PROTECT(allocVector(VECSXP, num_total));
  for (i = 0, j = 0; i < num_total; i++) {
    SEXP status = VECTOR_ELT(statuses, i);
    if (INTEGER(types)[i] == 1) {
      SEXP process = VECTOR_ELT(status, 0);
      SEXP pollconn = VECTOR_ELT(status, 1);
      processx_handle_t *handle = R_ExternalPtrAddr(process);
      processx_connection_t *cpollconn = isNull(pollconn) ? 0 :
	R_ExternalPtrAddr(pollconn);

      processx_c_pollable_from_connection(&pollables[j], handle->pipes[1]);
      if (handle->pipes[1]) handle->pipes[1]->poll_idx = j;
      j++;
      processx_c_pollable_from_connection(&pollables[j], handle->pipes[2]);
      if (handle->pipes[2]) handle->pipes[2]->poll_idx = j;
      j++;

      processx_c_pollable_from_connection(&pollables[j], cpollconn);
      if (cpollconn) cpollconn->poll_idx = j;
      j++;

      SET_VECTOR_ELT(result, i, allocVector(INTSXP, 3));

    } else if (INTEGER(types)[i] == 2) {
      processx_connection_t *handle = R_ExternalPtrAddr(status);
      processx_c_pollable_from_connection(&pollables[j], handle);
      if (handle) handle->poll_idx = j;
      j++;
      SET_VECTOR_ELT(result, i, allocVector(INTSXP, 1));

    } else if (INTEGER(types)[i] == 3) {
      processx_c_pollable_from_curl(&pollables[j], status);
      j++;
      SET_VECTOR_ELT(result, i, allocVector(INTSXP, 1));
    }
  }

  processx_c_connection_poll(pollables, num_poll, cms);

  for (i = 0, j = 0; i < num_total; i++) {
    if (INTEGER(types)[i] == 1) {
      INTEGER(VECTOR_ELT(result, i))[0] = pollables[j++].event;
      INTEGER(VECTOR_ELT(result, i))[1] = pollables[j++].event;
      INTEGER(VECTOR_ELT(result, i))[2] = pollables[j++].event;
    } else if (INTEGER(types)[i] == 2) {
      INTEGER(VECTOR_ELT(result, i))[0] = pollables[j++].event;
    } else {
      INTEGER(VECTOR_ELT(result, i))[0] = pollables[j++].event;
    }
  }

  UNPROTECT(1);
  return result;
}
