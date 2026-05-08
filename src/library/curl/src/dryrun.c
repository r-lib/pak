/* *
 * Blocking easy interfaces to libcurl for R.
 * Example: https://curl.se/libcurl/c/getinmemory.html
 */

#include "curl-common.h"

static size_t write_nothing(void *contents, size_t sz, size_t nmemb, void *ctx) {
  return sz * nmemb;
}

static void run_httpuv(void *dummy) {
  SEXP expr = PROTECT(Rf_lang1(Rf_install("later_wrapper")));
  SEXP env = PROTECT(R_FindNamespace(Rf_mkString("curl")));
  Rf_eval(expr, env);
  UNPROTECT(2);
}

static int process_server(void) {
  return !(R_ToplevelExec(run_httpuv, NULL));
}

SEXP R_curl_dryrun(SEXP ptr){
  CURL *handle = get_handle(ptr);
  curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, write_nothing);
  CURLM * multi_handle = curl_multi_init();
  if(CURLM_OK != curl_multi_add_handle(multi_handle, handle))
    Rf_error("Failed to add handle");
  int still_running = 1;
  while(still_running) {
    if(process_server())
      break;
    if(curl_multi_perform(multi_handle, &(still_running)) != CURLM_OK)
      break;
  }
  int msgq = 0;
  CURLMsg *m = curl_multi_info_read(multi_handle, &msgq);
  CURLcode status = (m && (m->msg == CURLMSG_DONE)) ? m->data.result : CURLE_ABORTED_BY_CALLBACK;
  curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, NULL);
  curl_multi_remove_handle(multi_handle, handle);
  curl_multi_cleanup(multi_handle);
  assert_status(status, get_ref(ptr));
  return R_NilValue;
}
