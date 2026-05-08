#include <curl/curl.h>
#include <Rinternals.h>

#if LIBCURL_VERSION_MAJOR > 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 56)
#define HAS_MULTI_SSL 1
#endif

/* Force OpenSSL on Legacy Windows (Vista/2008) which do not support TLS 1.2 natively.
 * On other systems we let libcurl choose so you can set the 'CURL_SSL_BACKEND' variable.
 */
void switch_to_openssl_on_vista(void){
#if defined(_WIN32) && defined(HAS_MULTI_SSL)
  /* If a CURL_SSL_BACKEND is set, do not override */
  char *envvar = getenv("CURL_SSL_BACKEND");
  if(envvar != NULL && *envvar != 0){
    REprintf("Initiating curl with CURL_SSL_BACKEND: %s\n", envvar);
    return;
  }

  /* Lookup Windows version */
  DWORD dwBuild = 0;
  DWORD dwVersion = GetVersion();
  if (dwVersion < 0x80000000)
    dwBuild = (DWORD)(HIWORD(dwVersion));

  /* TLS 1.2 requires at least Windows 7 or 2008-R2 */
  if(dwBuild < 7600){
    switch(curl_global_sslset(CURLSSLBACKEND_OPENSSL, NULL, NULL)){
    case CURLSSLSET_OK :
      break;
    case CURLSSLSET_TOO_LATE:
      Rf_warning("Failed to set libcurl SSL: already initiated");
      break;
    case CURLSSLSET_UNKNOWN_BACKEND:
      Rf_warning("Failed to set libcurl SSL: unsupported backend");
      break;
    default:
      Rf_warning("Failed to set libcurl SSL: unknown error");
      break;
    }
  }

#endif
}
