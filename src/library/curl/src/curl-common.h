#include <Rinternals.h>

/* for now we use the old form APIs to support older distros */
#define CURL_DISABLE_DEPRECATION 1

#include <curl/curl.h>
#include <curl/easy.h>
#include <string.h>
#include <stdlib.h>

#define make_string(x) x ? Rf_mkString(x) : Rf_ScalarString(NA_STRING)
#define get_string(x) CHAR(STRING_ELT(x, 0))
#define assert(x) assert_message(x, NULL)

//TODO: switch to CURL_AT_LEAST_VERSION
#define AT_LEAST_CURL(x,y) (LIBCURL_VERSION_MAJOR > x || (LIBCURL_VERSION_MAJOR == x && LIBCURL_VERSION_MINOR >= y))

#if AT_LEAST_CURL(7, 55)
#define USE_CURL_OFF_T 1
#endif

#if AT_LEAST_CURL(7, 62)
#define HAS_CURL_PARSER 1
#endif

#if AT_LEAST_CURL(7, 72)
#define HAS_CURLINFO_EFFECTIVE_METHOD 1
#endif

#if AT_LEAST_CURL(7, 73)
#define HAS_CURL_EASY_OPTION 1
#endif

#if AT_LEAST_CURL(7, 80)
#define HAS_CURL_PARSER_STRERROR 1
#endif

typedef struct {
  unsigned char *buf;
  size_t size;
} memory;

typedef struct {
  SEXP multiptr;
  SEXP handles;
  CURLM *m;
} multiref;

typedef struct {
  multiref *mref;
  struct refnode *node;
  memory content;
  SEXP complete;
  SEXP error;
  SEXP data;
} async;

typedef struct {
  SEXP handleptr;
  CURL *handle;
  struct curl_httppost *form;
  struct curl_slist *headers;
  struct curl_slist *custom;
  char errbuf[CURL_ERROR_SIZE];
  memory resheaders;
  async async;
  int refCount;
  int locked;
} reference;

CURL* get_handle(SEXP ptr);
reference* get_ref(SEXP ptr);
void assert_status(CURLcode res, reference *ref);
void assert_message(CURLcode res, const char *str);
void massert(CURLMcode res);
SEXP slist_to_vec(struct curl_slist *slist);
struct curl_slist* vec_to_slist(SEXP vec);
struct curl_httppost* make_form(SEXP form);
void set_form(reference *ref, struct curl_httppost* newform);
void reset_resheaders(reference *ref);
void reset_errbuf(reference *ref);
void clean_handle(reference *ref);
size_t push_disk(void* contents, size_t sz, size_t nmemb, FILE *ctx);
size_t append_buffer(void *contents, size_t sz, size_t nmemb, void *ctx);
size_t data_callback(void * data, size_t sz, size_t nmemb, SEXP fun);
CURLcode curl_perform_with_interrupt(CURL *handle);
int pending_interrupt(void);
SEXP make_handle_response(reference *ref);

/* reflist.c */
SEXP reflist_init(void);
SEXP reflist_add(SEXP x, SEXP target);
SEXP reflist_remove(SEXP x, SEXP target);

/* Workaround for CRAN using outdated MacOS11 SDK */
#if defined(__APPLE__) && !defined(HAS_CURL_EASY_OPTION) && defined(ENABLE_MACOS_POLYFILL)
#include "macos-polyfill.h"
#endif
