#include "curl-common.h"
#include <time.h>

/* Notes:
 *  - First check for unhandled messages in curl_multi_info_read() before curl_multi_perform()
 *  - Use eval() to callback instead of R_tryEval() to propagate interrupt or error back to C
 */

#if LIBCURL_VERSION_MAJOR > 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 30)
#define HAS_CURLMOPT_MAX_TOTAL_CONNECTIONS 1
#endif

multiref *get_multiref(SEXP ptr){
  if(TYPEOF(ptr) != EXTPTRSXP || !Rf_inherits(ptr, "curl_multi"))
    Rf_error("pool ptr is not a curl_multi handle");
  multiref *mref = (multiref*) R_ExternalPtrAddr(ptr);
  if(!mref)
    Rf_error("multiref pointer is dead");
  return mref;
}

void multi_release(reference *ref){
  /* Release the easy-handle */
  CURL *handle = ref->handle;
  CURLM *multi = ref->async.mref->m;
  massert(curl_multi_remove_handle(multi, handle));
  curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, NULL);
  curl_easy_setopt(handle, CURLOPT_WRITEDATA, NULL);

  /* Remove the curl handle from the handles list */
  ref->async.mref->handles = reflist_remove(ref->async.mref->handles, ref->handleptr);
  R_SetExternalPtrProtected(ref->async.mref->multiptr, ref->async.mref->handles);
  SET_VECTOR_ELT(R_ExternalPtrProtected(ref->handleptr), 0, R_NilValue);

  /* Reset multi state struct */
  if(ref->async.content.buf){
    free(ref->async.content.buf);
    ref->async.content.buf = NULL;
    ref->async.content.size = 0;
  }
  ref->async.mref = NULL;
  ref->async.content.buf = NULL;
  ref->async.content.size = 0;
  ref->async.complete = NULL;
  ref->async.error = NULL;
  ref->async.data = NULL;
  ref->async.node = NULL;

  /* Unlock handle (and cleanup if needed) */
  ref->locked = 0;
  ref->refCount--;
  clean_handle(ref);
}

SEXP R_multi_cancel(SEXP handle_ptr){
  reference *ref = get_ref(handle_ptr);
  if(ref->async.mref)
    multi_release(ref);
  return handle_ptr;
}

SEXP R_multi_add(SEXP handle_ptr, SEXP cb_complete, SEXP cb_error, SEXP cb_data, SEXP pool_ptr){
  multiref *mref = get_multiref(pool_ptr);
  CURLM *multi = mref->m;

  reference *ref = get_ref(handle_ptr);
  if(ref->locked)
    Rf_error("Handle is locked. Probably in use in a connection or async request.");

  /* placeholder body */
  if(Rf_isFunction(cb_data)){
    curl_easy_setopt(ref->handle, CURLOPT_WRITEFUNCTION, (curl_write_callback) data_callback);
    curl_easy_setopt(ref->handle, CURLOPT_WRITEDATA, cb_data);
  } else {
    curl_easy_setopt(ref->handle, CURLOPT_WRITEFUNCTION, append_buffer);
    curl_easy_setopt(ref->handle, CURLOPT_WRITEDATA, &(ref->async.content));
  }

  /* add to scheduler */
  massert(curl_multi_add_handle(multi, ref->handle));

  /* create node in ref */
  ref->async.mref = mref;
  mref->handles = reflist_add(mref->handles, handle_ptr);
  R_SetExternalPtrProtected(pool_ptr, mref->handles);

  /* set multi callbacks */
  ref->async.complete = cb_complete;
  ref->async.error = cb_error;
  ref->async.data = cb_data;
  SET_VECTOR_ELT(R_ExternalPtrProtected(handle_ptr), 0,
                 Rf_list3(cb_error, cb_complete, cb_data));

  /* lock and protect handle */
  ref->refCount++;
  ref->locked = 1;
  return handle_ptr;
}

SEXP R_multi_run(SEXP pool_ptr, SEXP timeout, SEXP max){
  multiref *mref = get_multiref(pool_ptr);
  CURLM *multi = mref->m;

  int total_pending = -1;
  int total_success = 0;
  int total_fail = 0;
  int result_max = asInteger(max);
  double time_max = asReal(timeout);
  time_t time_start = time(NULL);

  double seconds_elapsed = 0;
  while(1) {
    /* check for completed requests */
    int dirty = 0;
    int msgq = 1;
    while (msgq > 0) {
      CURLMsg *m = curl_multi_info_read(multi, &msgq);
      if(m && (m->msg == CURLMSG_DONE)){
        dirty = 1;
        reference *ref = NULL;
        CURL *handle = m->easy_handle;
        CURLcode status = m->data.result;
        assert(curl_easy_getinfo(handle, CURLINFO_PRIVATE, (char**) &ref));

        // prepare for callback
        SEXP cb_complete = PROTECT(ref->async.complete);
        SEXP cb_error = PROTECT(ref->async.error);
        SEXP cb_data = PROTECT(ref->async.data);
        SEXP buf = PROTECT(allocVector(RAWSXP, ref->async.content.size));
        if(ref->async.content.buf && ref->async.content.size)
          memcpy(RAW(buf), ref->async.content.buf, ref->async.content.size);

        //release handle for use by callbacks
        multi_release(ref);

        // Trigger a final 'data' with second argument to TRUE
        // This also ensures that a file is consistently created, even for empty responses
        if(Rf_isFunction(cb_data)){
          SEXP buf = PROTECT(Rf_allocVector(RAWSXP, 0));
          SEXP call = PROTECT(Rf_lang3(cb_data, buf, ScalarInteger(1)));
          eval(call, R_GlobalEnv);
          UNPROTECT(2);
        }

        // callbacks must be trycatch! we should continue the loop
        if(status == CURLE_OK){
          total_success++;
          if(Rf_isFunction(cb_complete)){
            int arglen = Rf_length(FORMALS(cb_complete));
            SEXP out = PROTECT(make_handle_response(ref));
            SET_VECTOR_ELT(out, 6, buf);
            SEXP call = PROTECT(LCONS(cb_complete, arglen ? LCONS(out, R_NilValue) : R_NilValue));
            //R_tryEval(call, R_GlobalEnv, &cbfail);
            eval(call, R_GlobalEnv); //OK to error here
            UNPROTECT(2);
          }
        } else {
          total_fail++;
          if(Rf_isFunction(cb_error)){
            int arglen = Rf_length(FORMALS(cb_error));
            SEXP buf = PROTECT(mkString(strlen(ref->errbuf) ? ref->errbuf : curl_easy_strerror(status)));
            SEXP call = PROTECT(LCONS(cb_error, arglen ? LCONS(buf, R_NilValue) : R_NilValue));
            //R_tryEval(call, R_GlobalEnv, &cbfail);
            eval(call, R_GlobalEnv); //OK to error here
            UNPROTECT(2);
          }
        }
        UNPROTECT(4);
      }
      R_CheckUserInterrupt();
    }

    /* check for user interruptions */
    //if(pending_interrupt())  break;
    R_CheckUserInterrupt();

    /* check for timeout or max result*/
    if(result_max > 0 && total_success + total_fail >= result_max)
      break;
    if(time_max == 0 && total_pending != -1)
      break;
    if(time_max > 0){
      seconds_elapsed = (double) (time(NULL) - time_start);
      if(seconds_elapsed > time_max)
        break;
    }

    /* check if we are done */
    if(total_pending == 0 && !dirty)
      break;

#ifdef HAS_MULTI_WAIT
    /* wait for activity, timeout or "nothing" */
    int numfds;
    double waitforit = fmin(time_max - seconds_elapsed, 1); //at most 1 sec to support interrupts
    if(time_max > 0)
      massert(curl_multi_wait(multi, NULL, 0, (int) waitforit * 1000, &numfds));
#endif

    /* poll libcurl for new data - updates total_pending */
    CURLMcode res = CURLM_CALL_MULTI_PERFORM;
    while(res == CURLM_CALL_MULTI_PERFORM)
      res = curl_multi_perform(multi, &(total_pending));
    if(res != CURLM_OK)
      break;
  }

  SEXP res = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(res, 0, ScalarInteger(total_success));
  SET_VECTOR_ELT(res, 1, ScalarInteger(total_fail));
  SET_VECTOR_ELT(res, 2, ScalarInteger(total_pending));

  SEXP names = PROTECT(allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, mkChar("success"));
  SET_STRING_ELT(names, 1, mkChar("error"));
  SET_STRING_ELT(names, 2, mkChar("pending"));
  setAttrib(res, R_NamesSymbol, names);
  UNPROTECT(2);
  return res;
}

void fin_multi(SEXP ptr){
  multiref *mref = get_multiref(ptr);
  SEXP handles = mref->handles;
  while(handles != R_NilValue){
    multi_release(get_ref(CAR(handles)));
    handles = CDR(handles);
  }
  curl_multi_cleanup(mref->m);
  free(mref);
  R_ClearExternalPtr(ptr);
}

SEXP R_multi_new(void){
  multiref *ref = calloc(1, sizeof(multiref));
  ref->m = curl_multi_init();
  ref->handles = reflist_init();
  SEXP ptr = PROTECT(R_MakeExternalPtr(ref, R_NilValue, ref->handles));
  ref->multiptr = ptr;
  R_RegisterCFinalizerEx(ptr, fin_multi, 1);
  setAttrib(ptr, R_ClassSymbol, mkString("curl_multi"));
  UNPROTECT(1);
  return ptr;
}

SEXP R_multi_setopt(SEXP pool_ptr, SEXP total_con, SEXP host_con, SEXP multiplex){
  #ifdef HAS_CURLMOPT_MAX_TOTAL_CONNECTIONS
    CURLM *multi = get_multiref(pool_ptr)->m;
    massert(curl_multi_setopt(multi, CURLMOPT_MAX_TOTAL_CONNECTIONS, (long) asInteger(total_con)));
    massert(curl_multi_setopt(multi, CURLMOPT_MAX_HOST_CONNECTIONS, (long) asInteger(host_con)));
  #endif

  // NOTE: CURLPIPE_HTTP1 is unsafe for non idempotent requests
  #ifdef CURLPIPE_MULTIPLEX
    massert(curl_multi_setopt(multi, CURLMOPT_PIPELINING,
                              asLogical(multiplex) ? CURLPIPE_MULTIPLEX : CURLPIPE_NOTHING));
  #endif

  return pool_ptr;
}

SEXP R_multi_list(SEXP pool_ptr){
  return get_multiref(pool_ptr)->handles;
}

SEXP R_multi_fdset(SEXP pool_ptr){
  multiref *mref =  get_multiref(pool_ptr);
  CURLM *multi = mref->m;
  fd_set read_fd_set, write_fd_set, exc_fd_set;
  int max_fd, i, num_read = 0, num_write = 0, num_exc = 0;
  int *pread, *pwrite, *pexc;
  long timeout;
  SEXP result, names;

  FD_ZERO(&read_fd_set);
  FD_ZERO(&write_fd_set);
  FD_ZERO(&exc_fd_set);

  massert(curl_multi_fdset(multi, &read_fd_set, &write_fd_set,
			   &exc_fd_set, &max_fd));

  massert(curl_multi_timeout(multi, &timeout));

  for (i = 0; i <= max_fd; i++){
    if (FD_ISSET(i, &read_fd_set))  num_read++;
    if (FD_ISSET(i, &write_fd_set)) num_write++;
    if (FD_ISSET(i, &exc_fd_set))   num_exc++;
  }

  result = PROTECT(allocVector(VECSXP, 4));
  SET_VECTOR_ELT(result, 0, allocVector(INTSXP, num_read));
  SET_VECTOR_ELT(result, 1, allocVector(INTSXP, num_write));
  SET_VECTOR_ELT(result, 2, allocVector(INTSXP, num_exc));
  SET_VECTOR_ELT(result, 3, ScalarReal((double) timeout));

  names = PROTECT(allocVector(STRSXP, 4));
  SET_STRING_ELT(names, 0, mkChar("reads"));
  SET_STRING_ELT(names, 1, mkChar("writes"));
  SET_STRING_ELT(names, 2, mkChar("exceptions"));
  SET_STRING_ELT(names, 3, mkChar("timeout"));
  setAttrib(result, R_NamesSymbol, names);

  pread  = INTEGER(VECTOR_ELT(result, 0));
  pwrite = INTEGER(VECTOR_ELT(result, 1));
  pexc   = INTEGER(VECTOR_ELT(result, 2));

  for (i = 0; i <= max_fd; i++){
    if (FD_ISSET(i, &read_fd_set))  *(pread++)  = i;
    if (FD_ISSET(i, &write_fd_set)) *(pwrite++) = i;
    if (FD_ISSET(i, &exc_fd_set))   *(pexc++)   = i;
  }

  UNPROTECT(2);
  return result;
}
