#include "curl-common.h"
#include <stdint.h> /* SIZE_MAX */

CURL* get_handle(SEXP ptr){
  return get_ref(ptr)->handle;
}

reference* get_ref(SEXP ptr){
  if(TYPEOF(ptr) != EXTPTRSXP || !Rf_inherits(ptr, "curl_handle"))
    Rf_error("handle is not a curl_handle()");
  if(!R_ExternalPtrAddr(ptr))
    Rf_error("handle is dead");
  reference *ref = (reference*) R_ExternalPtrAddr(ptr);
  return ref;
}

void set_form(reference *ref, struct curl_httppost* newform){
  if(ref->form)
    curl_formfree(ref->form);
  ref->form = newform;
  if(newform){
    assert(curl_easy_setopt(ref->handle, CURLOPT_HTTPPOST, ref->form));
  } else {
    //CURLOPT_HTTPPOST has bug for empty forms. We probably want this:
    assert(curl_easy_setopt(ref->handle, CURLOPT_POSTFIELDS, ""));
  }
}

void reset_resheaders(reference *ref){
  if(ref->resheaders.buf)
    free(ref->resheaders.buf);
  ref->resheaders.buf = NULL;
  ref->resheaders.size = 0;
}

void reset_errbuf(reference *ref){
  memset(ref->errbuf, 0, CURL_ERROR_SIZE);
}

void assert_message(CURLcode res, const char *str){
  if(res == CURLE_OK)
    return;
  if(str == NULL)
    str = curl_easy_strerror(res);
  SEXP code = PROTECT(Rf_ScalarInteger(res));
  SEXP message = PROTECT(make_string(str));
  SEXP expr = PROTECT(Rf_install("raise_libcurl_error"));
  SEXP call = PROTECT(Rf_lang3(expr, code, message));
  Rf_eval(call, R_FindNamespace(Rf_mkString("curl")));
  UNPROTECT(4);
}

void assert_status(CURLcode res, reference *ref){
  if(res == CURLE_OK)
    return;
  const char *source_url = NULL;
  curl_easy_getinfo(ref->handle, CURLINFO_EFFECTIVE_URL, &source_url);
  SEXP url = PROTECT(make_string(source_url));
  SEXP code = PROTECT(Rf_ScalarInteger(res));
  SEXP message = PROTECT(make_string(curl_easy_strerror(res)));
  SEXP errbuf = PROTECT(make_string(ref->errbuf));
  SEXP expr = PROTECT(Rf_install("raise_libcurl_error"));
  SEXP call = PROTECT(Rf_lang5(expr, code, message, errbuf, url));
  Rf_eval(call, R_FindNamespace(Rf_mkString("curl")));
  UNPROTECT(6);
}

void massert(CURLMcode res){
  if(res != CURLM_OK)
    Rf_error("%s", curl_multi_strerror(res));
}

/* make sure to call curl_slist_free_all on this object */
struct curl_slist* vec_to_slist(SEXP vec){
  if(!Rf_isString(vec))
    Rf_error("vec is not a character vector");
  struct curl_slist *slist = NULL;
  for(int i = 0; i < Rf_length(vec); i++){
    slist = curl_slist_append(slist, CHAR(STRING_ELT(vec, i)));
  }
  return slist;
}

SEXP slist_to_vec(struct curl_slist *slist){
  /* linked list of strings */
  struct curl_slist *cursor = slist;

  /* count slist */
  int n = 0;
  while (cursor) {
    n++;
    cursor = cursor->next;
  }

  SEXP out = PROTECT(Rf_allocVector(STRSXP, n));
  cursor = slist;
  for(int i = 0; i < n; i++){
    SET_STRING_ELT(out, i, Rf_mkChar(cursor->data));
    cursor = cursor->next;
  }
  UNPROTECT(1);
  return out;
}

size_t push_disk(void* contents, size_t sz, size_t nmemb, FILE *ctx) {
  //if (pending_interrupt())
  //  return 0;
  return fwrite(contents, sz, nmemb, ctx);
}

static size_t round_up(size_t v){
  if(v == 0)
    return 0;
  v--;
  v |= v >> 1;
  v |= v >> 2;
  v |= v >> 4;
  v |= v >> 8;
  v |= v >> 16;
/* 64 bit only */
#if SIZE_MAX > 4294967296
    v |= v >> 32;
#endif
  return ++v;
}

size_t append_buffer(void *contents, size_t sz, size_t nmemb, void *ctx) {
//if (pending_interrupt())
  //  return 0;

  /* avoids compiler warning on windows */
  size_t realsize = sz * nmemb;
  memory *mem = (memory*) ctx;

  /* realloc can be slow, therefore increase buffer to nearest 2^n */
  mem->buf = realloc(mem->buf, round_up(mem->size + realsize));
  if (!mem->buf)
    return 0;

  /* append data and increment size */
  memcpy(&(mem->buf[mem->size]), contents, realsize);
  mem->size += realsize;
  return realsize;
}

size_t data_callback(void * data, size_t sz, size_t nmemb, SEXP fun) {
  size_t size = sz * nmemb;
  SEXP buf = PROTECT(Rf_allocVector(RAWSXP, size));
  memcpy(RAW(buf), data, size);

  /* call the R function */
  int err;
  SEXP call = PROTECT(Rf_lang3(fun, buf, Rf_ScalarInteger(0)));
  R_tryEval(call, R_GlobalEnv, &err);
  UNPROTECT(2);
  return err ? 0 : size;
}
