/* *
 * Streaming interface to libcurl for R. (c) 2015 Jeroen Ooms.
 * Source: https://github.com/jeroen/curl
 * Comments and contributions are welcome!
 * Helpful libcurl examples:
 *  - https://curl.se/libcurl/c/getinmemory.html
 *  - https://curl.se/libcurl/c/multi-single.html
 * Sparse documentation about Rconnection API:
 *  - https://github.com/wch/r-source/blob/trunk/src/include/R_ext/Connections.h
 *  - http://biostatmatt.com/R/R-conn-ints/C-Structures.html
 *
 * Notes: the close() function in R actually calls con->destroy. The con->close
 * function is only used when a connection is recycled after auto-open.
 */
#include "curl-common.h"
#include <Rconfig.h>

/* Define BSWAP_32 on Big Endian systems */
#ifdef WORDS_BIGENDIAN
#if (defined(__sun) && defined(__SVR4))
#include <sys/byteorder.h>
#elif (defined(__APPLE__) && defined(__ppc__) || defined(__ppc64__))
#include <libkern/OSByteOrder.h>
#define BSWAP_32 OSSwapInt32
#elif (defined(__OpenBSD__))
#define BSWAP_32(x) swap32(x)
#elif (defined(__NetBSD__))
#include <sys/types.h>
#include <machine/bswap.h>
#define BSWAP_32(x) bswap32(x)
#elif (defined(__GLIBC__))
#include <byteswap.h>
#define BSWAP_32(x) bswap_32(x)
#elif (defined(_AIX))
#define BSWAP_32(x) __builtin_bswap32(x)
#endif
#endif

/* the RConnection API is experimental and subject to change */
#include <R_ext/Connections.h>
#if ! defined(R_CONNECTIONS_VERSION) || R_CONNECTIONS_VERSION != 1
#error "Unsupported connections API version"
#endif

#define min(a, b) (((a) < (b)) ? (a) : (b))
#define R_EOF -1

typedef struct {
  char *url;
  char *buf;
  char *cur;
  int has_data;
  int has_more;
  int used;
  int partial;
  size_t size;
  size_t limit;
  CURLM *manager;
  CURL *handle;
  reference *ref;
} request;

/* callback function to store received data */
static size_t push(void *contents, size_t sz, size_t nmemb, void *ctx) {
  /* avoids compiler warning on windows */
  request* req = (request*) ctx;
  req->has_data = 1;

  /* move existing data to front of buffer (if any) */
  memmove(req->buf, req->cur, req->size);

  /* allocate more space if required */
  size_t realsize = sz * nmemb;
  size_t newsize = req->size + realsize;
  while(newsize > req->limit) {
    size_t newlimit = 2 * req->limit;
    //Rprintf("Resizing buffer to %d.\n", newlimit);
    void *newbuf = realloc(req->buf, newlimit);
    if(!newbuf)
      Rf_error("Failure in realloc. Out of memory?");
    req->buf = newbuf;
    req->limit = newlimit;
  }

  /* append new data */
  memcpy(req->buf + req->size, contents, realsize);
  req->size = newsize;
  req->cur = req->buf;
  return realsize;
}

static size_t pop(void *target, size_t max, request *req){
  size_t copy_size = min(req->size, max);
  memcpy(target, req->cur, copy_size);
  req->cur += copy_size;
  req->size -= copy_size;
  //Rprintf("Requested %d bytes, popped %d bytes, new size %d bytes.\n", max, copy_size, req->size);
  return copy_size;
}

void check_manager(CURLM *manager, reference *ref) {
  for(int msg = 1; msg > 0;){
    CURLMsg *out = curl_multi_info_read(manager, &msg);
    if(out)
      assert_status(out->data.result, ref);
  }
}

//NOTE: renamed because the name 'fetch' caused crash/conflict on Solaris.
void fetchdata(request *req) {
  R_CheckUserInterrupt();
  long timeout = 10*1000;
  massert(curl_multi_timeout(req->manager, &timeout));
  /* massert(curl_multi_perform(req->manager, &(req->has_more))); */

  /* On libcurl < 7.20 we need to check for CURLM_CALL_MULTI_PERFORM, see docs */
  CURLMcode res = CURLM_CALL_MULTI_PERFORM;
  while(res == CURLM_CALL_MULTI_PERFORM){
    res = curl_multi_perform(req->manager, &(req->has_more));
  }
  massert(res);
  /* End */
  check_manager(req->manager, req->ref);
}

/* Support for readBin() */
static size_t rcurl_read(void *target, size_t sz, size_t ni, Rconnection con) {
  request *req = (request*) con->private;
  size_t req_size = sz * ni;

  /* append data to the target buffer */
  size_t total_size = pop(target, req_size, req);

  if (total_size > 0 && (!con->blocking || req->partial)) {
      // If we can return data without waiting, and the connection is
      // non-blocking (or using curl_fetch_stream()), do so.
      // This ensures that bytes we already received get flushed
      // to the target buffer before a connection error.
      con->incomplete = req->has_more || req->size;
      return total_size;
  }

  while((req_size > total_size) && req->has_more) {
    /* wait for activity, timeout or "nothing" */
#ifdef HAS_MULTI_WAIT
    int numfds;
    if(con->blocking)
      massert(curl_multi_wait(req->manager, NULL, 0, 1000, &numfds));
#endif
    fetchdata(req);
    total_size += pop((char*)target + total_size, (req_size-total_size), req);

    //return less than requested data for non-blocking connections, or curl_fetch_stream()
    if(!con->blocking || req->partial)
      break;
  }
  con->incomplete = req->has_more || req->size;
  return total_size;
}

/* naive implementation of readLines */
static int rcurl_fgetc(Rconnection con) {
  int x = 0;
#ifdef WORDS_BIGENDIAN
  return rcurl_read(&x, 1, 1, con) ? BSWAP_32(x) : R_EOF;
#else
  return rcurl_read(&x, 1, 1, con) ? x : R_EOF;
#endif
}

void cleanup(Rconnection con) {
  //Rprintf("Destroying connection.\n");
  request *req = (request*) con->private;
  reference *ref = req->ref;

  /* free thee handle connection */
  curl_multi_remove_handle(req->manager, req->handle);
  ref->locked = 0;

  /* delayed finalizer cleanup */
  (ref->refCount)--;
  clean_handle(ref);

  /* clean up connection */
  curl_multi_cleanup(req->manager);
  free(req->buf);
  free(req->url);
  free(req);
}

/* reset to pre-opened state */
void reset(Rconnection con) {
  //Rprintf("Resetting connection object.\n");
  request *req = (request*) con->private;
  curl_multi_remove_handle(req->manager, req->handle);
  req->ref->locked = 0;
  con->isopen = FALSE;
  con->text = TRUE;
  con->incomplete = FALSE;
  strcpy(con->mode, "r");
}

static Rboolean rcurl_open(Rconnection con) {
  request *req = (request*) con->private;

  //same message as base::url()
  if (con->mode[0] != 'r' || strchr(con->mode, 'w'))
    Rf_error("can only open URLs for reading");

  if(req->ref->locked)
    Rf_error("Handle is already in use elsewhere.");

  /* init a multi stack with callback */
  CURL *handle = req->handle;
  assert(curl_easy_setopt(handle, CURLOPT_URL, req->url));
  assert(curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, push));
  assert(curl_easy_setopt(handle, CURLOPT_WRITEDATA, req));

  /* add the handle to the pool and lock it */
  massert(curl_multi_add_handle(req->manager, handle));
  req->ref->locked = 1;

  /* reset the state */
  req->handle = handle;
  req->cur = req->buf;
  req->size = 0;
  req->used = 1;
  req->has_data = 0;
  req->has_more = 1;

  /* fully non-blocking has 's' in open mode */
  int block_open = strchr(con->mode, 's') == NULL;
  int force_open = strchr(con->mode, 'f') != NULL;

 /* Wait for first data to arrive. Monitoring a change in status code does not
   suffice in case of http redirects */
  while(block_open && req->has_more && !req->has_data) {
#ifdef HAS_MULTI_WAIT
    int numfds;
    massert(curl_multi_wait(req->manager, NULL, 0, 1000, &numfds));
#endif
    fetchdata(req);
  }

  /* check http status code */
  /* Stream connections should be checked via handle_data() */
  /* Non-blocking open connections get checked during read */
  if(block_open && !force_open)
    stop_for_status(handle);

  /* set mode in case open() changed it */
  con->text = strchr(con->mode, 'b') ? FALSE : TRUE;
  con->isopen = TRUE;
  con->incomplete = TRUE;
  return TRUE;
}

SEXP R_curl_connection(SEXP url, SEXP ptr, SEXP partial) {
  if(!Rf_isString(url))
    Rf_error("Argument 'url' must be string.");

  /* create the R connection object, mimicking base::url() */
  Rconnection con;

  /* R wants description in native encoding, but we use UTF-8 URL below */
  SEXP rc = PROTECT(R_new_custom_connection(Rf_translateChar(STRING_ELT(url, 0)), "r", "curl", &con));

  /* setup curl. These are the parts that are recycable. */
  request *req = malloc(sizeof(request));
  req->handle = get_handle(ptr);
  req->ref = get_ref(ptr);
  req->limit = CURL_MAX_WRITE_SIZE;
  req->buf = malloc(req->limit);
  req->manager = curl_multi_init();
  req->partial = Rf_asLogical(partial); //only for curl_fetch_stream()
  req->used = 0;

  /* allocate url string */
  req->url = malloc(strlen(Rf_translateCharUTF8(Rf_asChar(url))) + 1);
  strcpy(req->url, Rf_translateCharUTF8(Rf_asChar(url)));

  /* set connection properties */
  con->incomplete = FALSE;
  con->private = req;
  con->canseek = FALSE;
  con->canwrite = FALSE;
  con->isopen = FALSE;
  con->blocking = TRUE;
  con->text = TRUE;
  con->UTF8out = TRUE;
  con->open = rcurl_open;
  con->close = reset;
  con->destroy = cleanup;
  con->read = rcurl_read;
  con->fgetc = rcurl_fgetc;
  con->fgetc_internal = rcurl_fgetc;

  /* protect the handle */
  (req->ref->refCount)++;

  UNPROTECT(1);
  return rc;
}
