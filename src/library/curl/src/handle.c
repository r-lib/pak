#include "curl-common.h"
#include "callbacks.h"

/* These are defined in typechecking.c */
extern int r_curl_is_slist_option(CURLoption x);
extern int r_curl_is_long_option(CURLoption x);
extern int r_curl_is_off_t_option(CURLoption x);
extern int r_curl_is_string_option(CURLoption x);
extern int r_curl_is_postfields_option(CURLoption x);

#define make_string(x) x ? Rf_mkString(x) : Rf_ScalarString(NA_STRING)

#ifndef MAX_PATH
#define MAX_PATH 1024
#endif

#if LIBCURL_VERSION_MAJOR > 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 47)
#define HAS_HTTP_VERSION_2TLS 1
#endif

#if LIBCURL_VERSION_MAJOR > 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 32)
#define HAS_XFERINFOFUNCTION 1
#endif

#if LIBCURL_VERSION_MAJOR > 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 36)
#define HAS_CURLOPT_EXPECT_100_TIMEOUT_MS 1
#endif

int total_handles = 0;

void clean_handle(reference *ref){
  if(ref->refCount == 0){
    if(ref->headers)
      curl_slist_free_all(ref->headers);
    if(ref->custom)
      curl_slist_free_all(ref->custom);
    if(ref->form)
      curl_formfree(ref->form);
    if(ref->handle)
      curl_easy_cleanup(ref->handle);
    if(ref->resheaders.buf)
      free(ref->resheaders.buf);
    free(ref);
    total_handles--;
  }
}

void fin_handle(SEXP ptr){
  reference *ref = (reference*) R_ExternalPtrAddr(ptr);

  //this kind of strange but the multi finalizer needs the ptr value
  //if it is still pending
  ref->refCount--;
  if(ref->refCount == 0)
    R_ClearExternalPtr(ptr);

  //free stuff
  clean_handle(ref);
}

/* the default readfunc os fread which can cause R to freeze */
size_t dummy_read(char *buffer, size_t size, size_t nitems, void *instream){
  return 0;
}

#ifdef HAS_XFERINFOFUNCTION
#define xftype curl_off_t
#else
#define xftype double
#endif

static int xferinfo_callback(void *clientp, xftype dltotal, xftype dlnow, xftype ultotal, xftype ulnow){
  static xftype dlprev = 0;
  static xftype ulprev = 0;
  if(dlnow && dlnow != dlprev){
    dlprev = dlnow;
    if(dltotal){
      int pct_dn = (100 * dlnow)/dltotal;
      REprintf("\r [%d%%] Downloaded %.0lf bytes...", pct_dn, (double) dlnow);
      if(dlnow == dltotal)
        REprintf("\n");
    } else {
      REprintf("\r Downloaded %.0lf bytes...", (double) dlnow);
    }
  } else if(ulnow && ulnow != ulprev){
    ulprev = ulnow;
    int pct_up = (100 * ulnow)/ultotal;
    REprintf("\r [%d%%] Uploaded %.0lf bytes...", pct_up, (double) ulnow);
    if(ulnow == ultotal)
      REprintf("\n");
  }
  return 0;
}

/* Set default headers here, these are only allocated once */
static struct curl_slist * default_headers(void){
  static struct curl_slist * headers = NULL;
  if(headers == NULL){
    headers = curl_slist_append(headers, "Expect:");
  }
  return headers;
}

static void set_headers(reference *ref, struct curl_slist *newheaders){
  if(ref->headers)
    curl_slist_free_all(ref->headers);
  ref->headers = newheaders;
  assert(curl_easy_setopt(ref->handle, CURLOPT_HTTPHEADER,
                          newheaders ? newheaders : default_headers()));
}

static int default_verbose_cb(CURL *handle, curl_infotype type, char *data, size_t size, void *userptr){
  if(type < 3){
    char prefix = type == CURLINFO_TEXT ? '*' : (type == CURLINFO_HEADER_IN ? '<' : '>');
    REprintf("%c %.*s", prefix, (int) size, data);
  }
  return 0;
}


/* These are defaulst that we always want to set */
static void set_handle_defaults(reference *ref){

  /* the actual curl handle */
  CURL *handle = ref->handle;
  assert(curl_easy_setopt(handle, CURLOPT_PRIVATE, ref));

  /* set the response header collector */
  reset_resheaders(ref);
  curl_easy_setopt(handle, CURLOPT_HEADERFUNCTION, append_buffer);
  curl_easy_setopt(handle, CURLOPT_HEADERDATA, &(ref->resheaders));

  /* Respect CURL_CA_BUNDLE like base R, except for Schannel on Windows */
  const char *ca_bundle = getenv("CURL_CA_BUNDLE");
  #ifdef _WIN32
  curl_easy_setopt(handle, CURLOPT_SSL_OPTIONS, CURLSSLOPT_NO_REVOKE);
  struct curl_tlssessioninfo *tlsinfo = NULL;
  if(curl_easy_getinfo(handle, CURLINFO_TLS_SSL_PTR, &tlsinfo) == CURLE_OK){
    if(tlsinfo->backend == CURLSSLBACKEND_OPENSSL && ca_bundle == NULL) {
      curl_easy_setopt(handle, CURLOPT_SSL_OPTIONS, CURLSSLOPT_NO_REVOKE | CURLSSLOPT_NATIVE_CA);
    } else if(tlsinfo->backend == CURLSSLBACKEND_SCHANNEL){
      ca_bundle = NULL;
    }
  }
  #endif

  if(ca_bundle != NULL) {
    curl_easy_setopt(handle, CURLOPT_CAINFO, ca_bundle);
  }

  /* needed to support compressed responses */
  assert(curl_easy_setopt(handle, CURLOPT_ENCODING, ""));

  /* follow redirect */
  assert(curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, 1L));
  assert(curl_easy_setopt(handle, CURLOPT_MAXREDIRS, 10L));

  /* a sensible timeout (10s) */
  assert(curl_easy_setopt(handle, CURLOPT_CONNECTTIMEOUT, 10L));

  /* needed to start the cookie engine */
  assert(curl_easy_setopt(handle, CURLOPT_COOKIEFILE, ""));
  assert(curl_easy_setopt(handle, CURLOPT_FILETIME, 1L));

  /* set the default user agent */
  SEXP agent = Rf_GetOption1(Rf_install("HTTPUserAgent"));
  if(Rf_isString(agent) && Rf_length(agent)){
    assert(curl_easy_setopt(handle, CURLOPT_USERAGENT, CHAR(STRING_ELT(agent, 0))));
  } else {
    assert(curl_easy_setopt(handle, CURLOPT_USERAGENT, "r/curl/jeroen"));
  }

  /* allow all authentication methods */
  assert(curl_easy_setopt(handle, CURLOPT_HTTPAUTH, CURLAUTH_ANY));
  assert(curl_easy_setopt(handle, CURLOPT_PROXYAUTH, CURLAUTH_ANY));

  /* enables HTTP2 on HTTPS (match behavior of curl cmd util) */
//#if defined(CURL_VERSION_HTTP2) && defined(HAS_HTTP_VERSION_2TLS)
//  if(curl_version_info(CURLVERSION_NOW)->features & CURL_VERSION_HTTP2)
//    assert(curl_easy_setopt(handle, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_2TLS));
//#endif

  /* set an error buffer */
  assert(curl_easy_setopt(handle, CURLOPT_ERRORBUFFER, ref->errbuf));

  /* dummy readfunction because default can freeze R */
  assert(curl_easy_setopt(handle, CURLOPT_READFUNCTION, dummy_read));

  /* set default progress printer (disabled by default) */
#ifdef HAS_XFERINFOFUNCTION
  assert(curl_easy_setopt(handle, CURLOPT_XFERINFOFUNCTION, xferinfo_callback));
#else
  assert(curl_easy_setopt(handle, CURLOPT_PROGRESSFUNCTION, xferinfo_callback));
#endif

  /* Disable the 'Expect: 100' header (deprecated in recent libcurl) */
  set_headers(ref, NULL);
#ifdef HAS_CURLOPT_EXPECT_100_TIMEOUT_MS
  assert(curl_easy_setopt(handle, CURLOPT_EXPECT_100_TIMEOUT_MS, 0L));
#endif

  /* Send verbose outout to R front-end virtual stderr */
  assert(curl_easy_setopt(handle, CURLOPT_DEBUGFUNCTION, default_verbose_cb));
}

SEXP R_new_handle(void){
  reference *ref = calloc(1, sizeof(reference));
  ref->refCount = 1;
  ref->handle = curl_easy_init();
  total_handles++;
  set_handle_defaults(ref);
  SEXP prot = PROTECT(Rf_allocVector(VECSXP, 7)); //for protecting callback functions
  SEXP ptr = PROTECT(R_MakeExternalPtr(ref, R_NilValue, prot));
  R_RegisterCFinalizerEx(ptr, fin_handle, TRUE);
  Rf_setAttrib(ptr, R_ClassSymbol, Rf_mkString("curl_handle"));
  UNPROTECT(2);
  ref->handleptr = ptr;
  return ptr;
}

SEXP R_handle_reset(SEXP ptr){
  //reset all fields
  reference *ref = get_ref(ptr);
  set_form(ref, NULL);
  reset_errbuf(ref);
  curl_easy_reset(ref->handle);

  //clear custom vector field
  if(ref->custom){
    curl_slist_free_all(ref->custom);
    ref->custom = NULL;
  }

  //restore default settings
  set_handle_defaults(ref);
  return Rf_ScalarLogical(1);
}

SEXP R_handle_getheaders(SEXP ptr){
  reference *ref = get_ref(ptr);
  return slist_to_vec(ref->headers);
}

SEXP R_handle_getcustom(SEXP ptr){
  reference *ref = get_ref(ptr);
  return slist_to_vec(ref->custom);
}

SEXP R_handle_setopt(SEXP ptr, SEXP keys, SEXP values){
  reference *ref = get_ref(ptr);
  CURL *handle = get_handle(ptr);
  SEXP prot = R_ExternalPtrProtected(ptr);
  SEXP optnames = PROTECT(Rf_getAttrib(values, R_NamesSymbol));

  if(!Rf_isInteger(keys))
    Rf_error("keys` must be an integer");

  if(!Rf_isVector(values))
    Rf_error("`values` must be a list");

  for(int i = 0; i < Rf_length(keys); i++){
    int key = INTEGER(keys)[i];
    const char* optname = CHAR(STRING_ELT(optnames, i));
    SEXP val = VECTOR_ELT(values, i);
    if(val == R_NilValue){
      assert(curl_easy_setopt(handle, key, NULL));
#ifdef HAS_XFERINFOFUNCTION
    } else if (key == CURLOPT_XFERINFOFUNCTION) {
      if (TYPEOF(val) != CLOSXP)
        Rf_error("Value for option %s (%d) must be a function.", optname, key);

      assert(curl_easy_setopt(handle, CURLOPT_XFERINFOFUNCTION,
                              (curl_progress_callback) R_curl_callback_xferinfo));
      assert(curl_easy_setopt(handle, CURLOPT_XFERINFODATA, val));
      assert(curl_easy_setopt(handle, CURLOPT_NOPROGRESS, 0));
      SET_VECTOR_ELT(prot, 1, val); //protect gc
#endif
    } else if (key == CURLOPT_PROGRESSFUNCTION) {
      if (TYPEOF(val) != CLOSXP)
        Rf_error("Value for option %s (%d) must be a function.", optname, key);

      assert(curl_easy_setopt(handle, CURLOPT_PROGRESSFUNCTION,
        (curl_progress_callback) R_curl_callback_progress));
      assert(curl_easy_setopt(handle, CURLOPT_PROGRESSDATA, val));
      assert(curl_easy_setopt(handle, CURLOPT_NOPROGRESS, 0));
      SET_VECTOR_ELT(prot, 2, val); //protect gc
    } else if (key == CURLOPT_READFUNCTION) {
      if (TYPEOF(val) != CLOSXP)
        Rf_error("Value for option %s (%d) must be a function.", optname, key);

      assert(curl_easy_setopt(handle, CURLOPT_READFUNCTION,
        (curl_read_callback) R_curl_callback_read));
      assert(curl_easy_setopt(handle, CURLOPT_READDATA, val));
      SET_VECTOR_ELT(prot, 3, val); //protect gc
    } else if (key == CURLOPT_DEBUGFUNCTION) {
      if (TYPEOF(val) != CLOSXP)
        Rf_error("Value for option %s (%d) must be a function.", optname, key);

      assert(curl_easy_setopt(handle, CURLOPT_DEBUGFUNCTION,
        (curl_debug_callback) R_curl_callback_debug));
      assert(curl_easy_setopt(handle, CURLOPT_DEBUGDATA, val));
      SET_VECTOR_ELT(prot, 4, val); //protect gc
    } else if (key == CURLOPT_SSL_CTX_FUNCTION){
      if (TYPEOF(val) != CLOSXP)
        Rf_error("Value for option %s (%d) must be a function.", optname, key);

      assert(curl_easy_setopt(handle, CURLOPT_SSL_CTX_FUNCTION,
                              (curl_ssl_ctx_callback) R_curl_callback_ssl_ctx));
      assert(curl_easy_setopt(handle, CURLOPT_SSL_CTX_DATA, val));
      SET_VECTOR_ELT(prot, 5, val); //protect gc
    } else if (key == CURLOPT_SEEKFUNCTION) {
      if (TYPEOF(val) != CLOSXP)
        Rf_error("Value for option %s (%d) must be a function.", optname, key);
      assert(curl_easy_setopt(handle, CURLOPT_SEEKFUNCTION,
                              (curl_seek_callback) R_curl_callback_seek));
      assert(curl_easy_setopt(handle, CURLOPT_SEEKDATA, val));
      SET_VECTOR_ELT(prot, 6, val); //protect gc
    } else if (key == CURLOPT_URL) {
      /* always use utf-8 for urls */
      const char * url_utf8 = Rf_translateCharUTF8(STRING_ELT(val, 0));
      assert(curl_easy_setopt(handle, CURLOPT_URL, url_utf8));
    } else if(key == CURLOPT_HTTPHEADER){
      if(!Rf_isString(val))
        Rf_error("Value for option %s (%d) must be a string vector", optname, key);
      set_headers(get_ref(ptr), vec_to_slist(val));
    } else if (r_curl_is_slist_option(key)) {
      if(!Rf_isString(val))
        Rf_error("Value for option %s (%d) must be a string vector", optname, key);
      ref->custom = vec_to_slist(val);
      assert(curl_easy_setopt(handle, key, ref->custom));
    } else if(r_curl_is_long_option(key)){
      if(!Rf_isNumeric(val) || Rf_length(val) != 1) {
        Rf_error("Value for option %s (%d) must be a number.", optname, key);
      }
      assert(curl_easy_setopt(handle, key, (long) Rf_asInteger(val)));
    } else if(r_curl_is_off_t_option(key)){
      if(!Rf_isNumeric(val) || Rf_length(val) != 1) {
        Rf_error("Value for option %s (%d) must be a number.", optname, key);
      }
      assert(curl_easy_setopt(handle, key, (curl_off_t) Rf_asReal(val)));
    } else if(r_curl_is_postfields_option(key) || r_curl_is_string_option(key)){
      if(key == CURLOPT_POSTFIELDS){
        key = CURLOPT_COPYPOSTFIELDS;
      }
      switch (TYPEOF(val)) {
      case RAWSXP:
        if(key == CURLOPT_COPYPOSTFIELDS)
          assert(curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE_LARGE, (curl_off_t) Rf_length(val)));
        assert(curl_easy_setopt(handle, key, RAW(val)));
        break;
      case STRSXP:
        if (Rf_length(val) != 1)
          Rf_error("Value for option %s (%d) must be length-1 string", optname, key);
        assert(curl_easy_setopt(handle, key, CHAR(STRING_ELT(val, 0))));
        break;
      default:
        Rf_error("Value for option %s (%d) must be a string or raw vector.", optname, key);
      }
    } else {
      Rf_error("Option %s (%d) has unknown or unsupported type.", optname, key);
    }
  }
  UNPROTECT(1);
  return Rf_ScalarLogical(1);
}

SEXP R_handle_setform(SEXP ptr, SEXP form){
  if(!Rf_isVector(form))
    Rf_error("Form must be a list.");
  set_form(get_ref(ptr), make_form(form));
  return Rf_ScalarLogical(1);
}

SEXP make_timevec(CURL *handle){
  double time_redirect, time_lookup, time_connect, time_pre, time_start, time_total;
  assert(curl_easy_getinfo(handle, CURLINFO_REDIRECT_TIME, &time_redirect));
  assert(curl_easy_getinfo(handle, CURLINFO_NAMELOOKUP_TIME, &time_lookup));
  assert(curl_easy_getinfo(handle, CURLINFO_CONNECT_TIME, &time_connect));
  assert(curl_easy_getinfo(handle, CURLINFO_PRETRANSFER_TIME, &time_pre));
  assert(curl_easy_getinfo(handle, CURLINFO_STARTTRANSFER_TIME, &time_start));
  assert(curl_easy_getinfo(handle, CURLINFO_TOTAL_TIME, &time_total));

  SEXP result = PROTECT(Rf_allocVector(REALSXP, 6));
  REAL(result)[0] = time_redirect;
  REAL(result)[1] = time_lookup;
  REAL(result)[2] = time_connect;
  REAL(result)[3] = time_pre;
  REAL(result)[4] = time_start;
  REAL(result)[5] = time_total;

  SEXP names = PROTECT(Rf_allocVector(STRSXP, 6));
  SET_STRING_ELT(names, 0, Rf_mkChar("redirect"));
  SET_STRING_ELT(names, 1, Rf_mkChar("namelookup"));
  SET_STRING_ELT(names, 2, Rf_mkChar("connect"));
  SET_STRING_ELT(names, 3, Rf_mkChar("pretransfer"));
  SET_STRING_ELT(names, 4, Rf_mkChar("starttransfer"));
  SET_STRING_ELT(names, 5, Rf_mkChar("total"));
  Rf_setAttrib(result, R_NamesSymbol, names);
  UNPROTECT(2);
  return result;
}

/* Extract current cookies (state) from handle */
SEXP make_cookievec(CURL *handle){
  /* linked list of strings */
  struct curl_slist *cookies;
  assert(curl_easy_getinfo(handle, CURLINFO_COOKIELIST, &cookies));
  SEXP out = slist_to_vec(cookies);
  curl_slist_free_all(cookies);
  return out;
}

SEXP make_status(CURL *handle){
  long res_status;
  assert(curl_easy_getinfo(handle, CURLINFO_RESPONSE_CODE, &res_status));
  return Rf_ScalarInteger(res_status);
}

SEXP make_ctype(CURL *handle){
  char * ct;
  assert(curl_easy_getinfo(handle, CURLINFO_CONTENT_TYPE, &ct));
  return make_string(ct);
}

SEXP make_url(CURL *handle){
  char *res_url;
  assert(curl_easy_getinfo(handle, CURLINFO_EFFECTIVE_URL, &res_url));
  return Rf_ScalarString(Rf_mkCharCE(res_url, CE_UTF8));
}

SEXP make_filetime(CURL *handle){
  long filetime;
  assert(curl_easy_getinfo(handle, CURLINFO_FILETIME, &filetime));
  if(filetime < 0){
    filetime = NA_INTEGER;
  }

  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(classes, 0, Rf_mkChar("POSIXct"));
  SET_STRING_ELT(classes, 1, Rf_mkChar("POSIXt"));

  SEXP out = PROTECT(Rf_ScalarInteger(filetime));
  Rf_setAttrib(out, R_ClassSymbol, classes);
  UNPROTECT(2);
  return out;
}

SEXP make_rawvec(unsigned char *ptr, size_t size){
  SEXP out = PROTECT(Rf_allocVector(RAWSXP, size));
  if(size > 0)
    memcpy(RAW(out), ptr, size);
  UNPROTECT(1);
  return out;
}

SEXP make_namesvec(void){
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 7));
  SET_STRING_ELT(names, 0, Rf_mkChar("url"));
  SET_STRING_ELT(names, 1, Rf_mkChar("status_code"));
  SET_STRING_ELT(names, 2, Rf_mkChar("type"));
  SET_STRING_ELT(names, 3, Rf_mkChar("headers"));
  SET_STRING_ELT(names, 4, Rf_mkChar("modified"));
  SET_STRING_ELT(names, 5, Rf_mkChar("times"));
  SET_STRING_ELT(names, 6, Rf_mkChar("content"));
  UNPROTECT(1);
  return names;
}

SEXP R_get_handle_cookies(SEXP ptr){
  return make_cookievec(get_handle(ptr));
}

SEXP make_handle_response(reference *ref){
  CURL *handle = ref->handle;
  SEXP res = PROTECT(Rf_allocVector(VECSXP, 7));
  SET_VECTOR_ELT(res, 0, make_url(handle));
  SET_VECTOR_ELT(res, 1, make_status(handle));
  SET_VECTOR_ELT(res, 2, make_ctype(handle));
  SET_VECTOR_ELT(res, 3, make_rawvec(ref->resheaders.buf, ref->resheaders.size));
  SET_VECTOR_ELT(res, 4, make_filetime(handle));
  SET_VECTOR_ELT(res, 5, make_timevec(handle));
  SET_VECTOR_ELT(res, 6, R_NilValue);
  Rf_setAttrib(res, R_NamesSymbol, make_namesvec());
  UNPROTECT(1);
  return res;
}

SEXP R_get_handle_response(SEXP ptr){
  /* get the handle */
  reference *ref = get_ref(ptr);
  return make_handle_response(ref);
}

SEXP R_get_handle_speed(SEXP ptr){
  CURL *handle = get_handle(ptr);
#ifdef USE_CURL_OFF_T
  curl_off_t dl = 0;
  curl_off_t ul = 0;
  curl_easy_getinfo(handle, CURLINFO_SPEED_DOWNLOAD_T, &dl);
  curl_easy_getinfo(handle, CURLINFO_SPEED_UPLOAD_T, &ul);
#else
  double dl = 0;
  double ul = 0;
  curl_easy_getinfo(handle, CURLINFO_SPEED_DOWNLOAD, &dl);
  curl_easy_getinfo(handle, CURLINFO_SPEED_UPLOAD, &ul);
#endif
  SEXP out = Rf_allocVector(REALSXP, 2);
  REAL(out)[0] = (double) dl;
  REAL(out)[1] = (double) ul;
  return out;
}

SEXP R_get_handle_clength(SEXP ptr){
  CURL *handle = get_handle(ptr);
#ifdef USE_CURL_OFF_T
  curl_off_t cl = 0;
  curl_easy_getinfo(handle, CURLINFO_CONTENT_LENGTH_DOWNLOAD_T, &cl);
#else
  double cl = 0;
  curl_easy_getinfo(handle, CURLINFO_CONTENT_LENGTH_DOWNLOAD, &cl);
#endif
  return Rf_ScalarReal((double) cl < 0 ? NA_REAL : cl);
}

SEXP R_get_handle_received(SEXP ptr){
  CURL *handle = get_handle(ptr);
#ifdef USE_CURL_OFF_T
  curl_off_t dl = 0;
  curl_easy_getinfo(handle, CURLINFO_SIZE_DOWNLOAD_T, &dl);
#else
  double dl = 0;
  curl_easy_getinfo(handle, CURLINFO_SIZE_DOWNLOAD, &dl);
#endif
  return Rf_ScalarReal((double) dl);
}

SEXP R_get_handle_mtime(SEXP ptr){
  return make_filetime(get_handle(ptr));
}

SEXP R_total_handles(void){
  return(Rf_ScalarInteger(total_handles));
}
