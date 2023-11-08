int R_curl_callback_progress(SEXP fun, double dltotal, double dlnow,
  double ultotal, double ulnow);
size_t R_curl_callback_read(char *buffer, size_t size, size_t nitems, SEXP fun);
int R_curl_callback_seek(SEXP fun, curl_off_t offset, int origin);
int R_curl_callback_debug(CURL *handle, curl_infotype type_, char *data,
                          size_t size, SEXP fun);

int R_curl_callback_xferinfo(SEXP fun,
                             curl_off_t  dltotal, curl_off_t  dlnow,
                             curl_off_t  ultotal, curl_off_t  ulnow);

int R_curl_callback_ssl_ctx(CURL *handle, void *ssl_ctx, SEXP fun);
