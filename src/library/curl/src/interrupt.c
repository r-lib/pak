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

static int str_starts_with(const char *a, const char *b) {
  if(strncmp(a, b, strlen(b)) == 0) return 1;
  return 0;
}

/* created in init.c */
extern CURLM * shared_multi_handle;

/* Don't call Rf_error() until we remove the handle from the multi handle! */
CURLcode curl_perform_with_interrupt(CURL *handle){
  /* start settings */
  CURLcode status = CURLE_FAILED_INIT;
  CURLM * temp_multi_handle = NULL;
  CURLM * multi_handle = NULL;
  int still_running = 1;

  /* Do not reuse FTP connections, because it is buggy */
  /* For example https://github.com/jeroen/curl/issues/348 */
  const char *effective_url = NULL;
  curl_easy_getinfo(handle, CURLINFO_EFFECTIVE_URL, &effective_url);
  if(effective_url && str_starts_with(effective_url, "ftp")){
    temp_multi_handle = curl_multi_init();
    multi_handle = temp_multi_handle;
  } else {
    multi_handle = shared_multi_handle;
  }

  if(CURLM_OK != curl_multi_add_handle(multi_handle, handle)){
    return CURLE_FAILED_INIT;
  }

  /* non blocking downloading */
  while(still_running) {
    if(pending_interrupt()){
      status = CURLE_ABORTED_BY_CALLBACK;
      break;
    }

    int numfds;
    if(curl_multi_wait(multi_handle, NULL, 0, 1000, &numfds) != CURLM_OK)
      break;

    /* check for multi errors */
    if(curl_multi_perform(multi_handle, &(still_running)) != CURLM_OK)
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
  if(temp_multi_handle)
    curl_multi_cleanup(temp_multi_handle);
  return status;
}
