/* *
 * Blocking easy interfaces to libcurl for R.
 * Example: https://curl.se/libcurl/c/getinmemory.html
 */

#include "curl-common.h"

SEXP R_curl_fetch_memory(SEXP url, SEXP ptr, SEXP nonblocking){
  if (!isString(url) || length(url) != 1)
    error("Argument 'url' must be string.");

  /* get the handle */
  CURL *handle = get_handle(ptr);

  /* update the url */
  curl_easy_setopt(handle, CURLOPT_URL, CHAR(STRING_ELT(url, 0)));

  /* reset the response header buffer */
  reset_resheaders(get_ref(ptr));
  reset_errbuf(get_ref(ptr));

  /* buffer body */
  memory body = {NULL, 0};
  curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, append_buffer);
  curl_easy_setopt(handle, CURLOPT_WRITEDATA, &body);

  /* perform blocking request */
  CURLcode status = asLogical(nonblocking) ?
    curl_perform_with_interrupt(handle) : curl_easy_perform(handle);

  /* Reset for reuse */
  curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, NULL);
  curl_easy_setopt(handle, CURLOPT_WRITEDATA, NULL);

  /* check for errors */
  if (status != CURLE_OK) {
    free(body.buf);
    assert_status(status, get_ref(ptr));
  }

  /* create output */
  SEXP out = PROTECT(allocVector(RAWSXP, body.size));

  /* copy only if there is actual content */
  if(body.size)
    memcpy(RAW(out), body.buf, body.size);

  /* cleanup and return */
  UNPROTECT(1);
  free(body.buf);
  return out;
}

SEXP R_curl_fetch_disk(SEXP url, SEXP ptr, SEXP path, SEXP mode, SEXP nonblocking){
  if (!isString(url) || length(url) != 1)
    error("Argument 'url' must be string.");
  if (!isString(path) || length(path) != 1)
    error("`path` must be string.");

  /* get the handle */
  CURL *handle = get_handle(ptr);

  /* update the url */
  curl_easy_setopt(handle, CURLOPT_URL, CHAR(STRING_ELT(url, 0)));

  /* reset the response header buffer */
  reset_resheaders(get_ref(ptr));
  reset_errbuf(get_ref(ptr));

  /* open file */
  FILE *dest = fopen(CHAR(asChar(path)), CHAR(asChar(mode)));
  if(!dest)
    error("Failed to open file %s.", CHAR(asChar(path)));
  curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, push_disk);
  curl_easy_setopt(handle, CURLOPT_WRITEDATA, dest);

  /* perform blocking request */
  CURLcode status = asLogical(nonblocking) ?
    curl_perform_with_interrupt(handle): curl_easy_perform(handle);

  /* cleanup */
  curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, NULL);
  curl_easy_setopt(handle, CURLOPT_WRITEDATA, NULL);
  fclose(dest);

  /* check for errors */
  assert_status(status, get_ref(ptr));

  /* return the file path */
  return path;
}
