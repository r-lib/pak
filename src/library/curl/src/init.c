#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <curl/curl.h>

/* .Call calls */
extern SEXP R_curl_connection(SEXP, SEXP, SEXP);
extern SEXP R_curl_escape(SEXP, SEXP);
extern SEXP R_curl_fetch_disk(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_curl_fetch_memory(SEXP, SEXP, SEXP);
extern SEXP R_curl_getdate(SEXP);
extern SEXP R_curl_version(void);
extern SEXP R_download_curl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_findport(SEXP candidates);
extern SEXP R_get_handle_clength(SEXP);
extern SEXP R_get_handle_cookies(SEXP);
extern SEXP R_get_handle_mtime(SEXP);
extern SEXP R_get_handle_speed(SEXP);
extern SEXP R_get_handle_received(SEXP);
extern SEXP R_get_handle_response(SEXP);
extern SEXP R_get_proxy_for_url(SEXP, SEXP, SEXP);
extern SEXP R_handle_getcustom(SEXP);
extern SEXP R_handle_getheaders(SEXP);
extern SEXP R_handle_reset(SEXP);
extern SEXP R_handle_setform(SEXP, SEXP);
extern SEXP R_handle_setheaders(SEXP, SEXP);
extern SEXP R_handle_setopt(SEXP, SEXP, SEXP);
extern SEXP R_option_types(void);
extern SEXP R_multi_add(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_multi_cancel(SEXP);
extern SEXP R_multi_fdset(SEXP);
extern SEXP R_multi_list(SEXP);
extern SEXP R_multi_new(void);
extern SEXP R_multi_run(SEXP, SEXP, SEXP);
extern SEXP R_multi_setopt(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_new_file_writer(SEXP);
extern SEXP R_new_handle(void);
extern SEXP R_nslookup(SEXP, SEXP);
extern SEXP R_proxy_info(void);
extern SEXP R_split_string(SEXP, SEXP);
extern SEXP R_total_handles(void);
extern SEXP R_total_writers(void);
extern SEXP R_windows_build(void);
extern SEXP R_write_file_writer(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"R_curl_connection",     (DL_FUNC) &R_curl_connection,     3},
    {"R_curl_escape",         (DL_FUNC) &R_curl_escape,         2},
    {"R_curl_fetch_disk",     (DL_FUNC) &R_curl_fetch_disk,     5},
    {"R_curl_fetch_memory",   (DL_FUNC) &R_curl_fetch_memory,   3},
    {"R_curl_getdate",        (DL_FUNC) &R_curl_getdate,        1},
    {"R_curl_version",        (DL_FUNC) &R_curl_version,        0},
    {"R_download_curl",       (DL_FUNC) &R_download_curl,       6},
    {"R_findport",            (DL_FUNC) &R_findport,            1},
    {"R_get_handle_clength",  (DL_FUNC) &R_get_handle_clength,  1},
    {"R_get_handle_cookies",  (DL_FUNC) &R_get_handle_cookies,  1},
    {"R_get_handle_mtime",    (DL_FUNC) &R_get_handle_mtime,    1},
    {"R_get_handle_speed",    (DL_FUNC) &R_get_handle_speed,    1},
    {"R_get_handle_received", (DL_FUNC) &R_get_handle_received, 1},
    {"R_get_handle_response", (DL_FUNC) &R_get_handle_response, 1},
    {"R_get_proxy_for_url",   (DL_FUNC) &R_get_proxy_for_url,   3},
    {"R_handle_getcustom",    (DL_FUNC) &R_handle_getcustom,    1},
    {"R_handle_getheaders",   (DL_FUNC) &R_handle_getheaders,   1},
    {"R_handle_reset",        (DL_FUNC) &R_handle_reset,        1},
    {"R_handle_setform",      (DL_FUNC) &R_handle_setform,      2},
    {"R_handle_setheaders",   (DL_FUNC) &R_handle_setheaders,   2},
    {"R_handle_setopt",       (DL_FUNC) &R_handle_setopt,       3},
    {"R_option_types",        (DL_FUNC) &R_option_types,        0},
    {"R_multi_add",           (DL_FUNC) &R_multi_add,           5},
    {"R_multi_cancel",        (DL_FUNC) &R_multi_cancel,        1},
    {"R_multi_fdset",         (DL_FUNC) &R_multi_fdset,         1},
    {"R_multi_list",          (DL_FUNC) &R_multi_list,          1},
    {"R_multi_new",           (DL_FUNC) &R_multi_new,           0},
    {"R_multi_run",           (DL_FUNC) &R_multi_run,           3},
    {"R_multi_setopt",        (DL_FUNC) &R_multi_setopt,        4},
    {"R_new_file_writer",     (DL_FUNC) &R_new_file_writer,     1},
    {"R_new_handle",          (DL_FUNC) &R_new_handle,          0},
    {"R_nslookup",            (DL_FUNC) &R_nslookup,            2},
    {"R_proxy_info",          (DL_FUNC) &R_proxy_info,          0},
    {"R_split_string",        (DL_FUNC) &R_split_string,        2},
    {"R_total_handles",       (DL_FUNC) &R_total_handles,       0},
    {"R_total_writers",       (DL_FUNC) &R_total_writers,       0},
    {"R_windows_build",       (DL_FUNC) &R_windows_build,       0},
    {"R_write_file_writer",   (DL_FUNC) &R_write_file_writer,   3},
    {NULL, NULL, 0}
};

void switch_to_openssl_on_vista(void);
CURLM *multi_handle = NULL;

attribute_visible void R_init_curl(DllInfo *info) {
  switch_to_openssl_on_vista();
  curl_global_init(CURL_GLOBAL_DEFAULT);
  multi_handle = curl_multi_init();
  R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

attribute_visible void R_unload_curl(DllInfo *info) {
  curl_multi_cleanup(multi_handle);
  //curl_global_cleanup();
}
