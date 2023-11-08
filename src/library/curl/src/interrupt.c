/* Non-blocking drop-in replacement for curl_easy_perform with support for
 * R interruptions. Based on: https://curl.se/libcurl/c/multi-single.html
 */

#include <Rinternals.h>
#include "curl-common.h"

/* Check for interrupt without long jumping */
void check_interrupt_fn(void *dummy) {
  R_CheckUserInterrupt();
}

int pending_interrupt(void) {
  return !(R_ToplevelExec(check_interrupt_fn, NULL));
}

/* created in init.c */
extern CURLM * multi_handle;

/* Don't call Rf_error() until we remove the handle from the multi handle! */
CURLcode curl_perform_with_interrupt(CURL *handle){
  /* start settings */
  CURLcode status = CURLE_FAILED_INIT;
  int still_running = 1;

  if(CURLM_OK != curl_multi_add_handle(multi_handle, handle)){
    return CURLE_FAILED_INIT;
  }

  /* non blocking downloading */
  while(still_running) {
    if(pending_interrupt()){
      status = CURLE_ABORTED_BY_CALLBACK;
      break;
    }

#ifdef HAS_MULTI_WAIT
    /* wait for activity, timeout or "nothing" */
    int numfds;
    if(curl_multi_wait(multi_handle, NULL, 0, 1000, &numfds) != CURLM_OK)
      break;
#endif

    /* Required by old versions of libcurl */
    CURLMcode res = CURLM_CALL_MULTI_PERFORM;
    while(res == CURLM_CALL_MULTI_PERFORM)
      res = curl_multi_perform(multi_handle, &(still_running));

    /* check for multi errors */
    if(res != CURLM_OK)
      break;
  }

  /* set status if handle has completed. This might be overkill */
  if(!still_running){
    int msgq = 0;
    do {
      CURLMsg *m = curl_multi_info_read(multi_handle, &msgq);
      if(m && (m->msg == CURLMSG_DONE)){
        status = m->data.result;
        break;
      }
    } while (msgq > 0);
  }

  /* cleanup first */
  curl_multi_remove_handle(multi_handle, handle);
  return status;
}
