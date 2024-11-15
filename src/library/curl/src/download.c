/* *
 * Reimplementation of C_download (the "internal" method for download.file).
 */
#include "curl-common.h"

SEXP R_download_curl(SEXP url, SEXP destfile, SEXP quiet, SEXP mode, SEXP ptr, SEXP nonblocking) {
  if(!Rf_isString(url))
    Rf_error("Argument 'url' must be string.");

  if(!Rf_isString(destfile))
    Rf_error("Argument 'destfile' must be string.");

  if(!Rf_isLogical(quiet))
    Rf_error("Argument 'quiet' must be TRUE/FALSE.");

  if(!Rf_isString(mode))
    Rf_error("Argument 'mode' must be string.");

  /* get the handle */
  CURL *handle = get_handle(ptr);
  reset_errbuf(get_ref(ptr));

  /* open file */
  FILE *dest = fopen(CHAR(Rf_asChar(destfile)), CHAR(Rf_asChar(mode)));
  if(!dest)
    Rf_error("Failed to open file %s.", CHAR(Rf_asChar(destfile)));

  /* set options */
  curl_easy_setopt(handle, CURLOPT_URL, Rf_translateCharUTF8(Rf_asChar(url)));
  curl_easy_setopt(handle, CURLOPT_NOPROGRESS, Rf_asLogical(quiet));
  curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, push_disk);
  curl_easy_setopt(handle, CURLOPT_WRITEDATA, dest);
  curl_easy_setopt(handle, CURLOPT_FAILONERROR, 1L);

  /* perform blocking request */
  CURLcode status = Rf_asLogical(nonblocking) ?
    curl_perform_with_interrupt(handle) : curl_easy_perform(handle);

  /* cleanup */
  curl_easy_setopt(handle, CURLOPT_NOPROGRESS, 1L);
  curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, NULL);
  curl_easy_setopt(handle, CURLOPT_WRITEDATA, NULL);
  curl_easy_setopt(handle, CURLOPT_FAILONERROR, 0L);
  fclose(dest);

  /* raise for curl errors */
  assert_status(status, get_ref(ptr));
  return Rf_ScalarInteger(0);
}
