#include <Rinternals.h>

#ifdef _WIN32
#include <Windows.h>
#include <Winhttp.h>
#include <stdlib.h>

#define WINHTTP_AUTO_DETECT_TYPE_DHCP           0x00000001
#define WINHTTP_AUTO_DETECT_TYPE_DNS_A          0x00000002
#define WINHTTP_AUTOPROXY_AUTO_DETECT           0x00000001
#define WINHTTP_AUTOPROXY_CONFIG_URL            0x00000002
#define WINHTTP_AUTOPROXY_RUN_INPROCESS         0x00010000
#define WINHTTP_AUTOPROXY_RUN_OUTPROCESS_ONLY   0x00020000

static SEXP proxy_namesvec(void){
  SEXP names = PROTECT(allocVector(STRSXP, 4));
  SET_STRING_ELT(names, 0, mkChar("AutoDetect"));
  SET_STRING_ELT(names, 1, mkChar("AutoConfigUrl"));
  SET_STRING_ELT(names, 2, mkChar("Proxy"));
  SET_STRING_ELT(names, 3, mkChar("ProxyBypass"));
  UNPROTECT(1);
  return names;
}

static SEXP auto_namesvec(void){
  SEXP names = PROTECT(allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, mkChar("HasProxy"));
  SET_STRING_ELT(names, 1, mkChar("Proxy"));
  SET_STRING_ELT(names, 2, mkChar("ProxyBypass"));
  UNPROTECT(1);
  return names;
}

SEXP R_proxy_info(void){
  WINHTTP_CURRENT_USER_IE_PROXY_CONFIG MyProxyConfig;
  if(!WinHttpGetIEProxyConfigForCurrentUser(&MyProxyConfig)){
    return R_NilValue;
  }
  char buffer[500];
  SEXP vec = PROTECT(allocVector(VECSXP, 4));
  SET_VECTOR_ELT(vec, 0, ScalarLogical(MyProxyConfig.fAutoDetect));

  if(MyProxyConfig.lpszAutoConfigUrl != NULL) {
    wcstombs(buffer, MyProxyConfig.lpszAutoConfigUrl, 500);
    SET_VECTOR_ELT(vec, 1, mkString(buffer));
  }

  if(MyProxyConfig.lpszProxy != NULL) {
    wcstombs(buffer, MyProxyConfig.lpszProxy, 500);
    SET_VECTOR_ELT(vec, 2, mkString(buffer));
  }

  if(MyProxyConfig.lpszProxyBypass != NULL) {
    wcstombs(buffer, MyProxyConfig.lpszProxyBypass, 500);
    SET_VECTOR_ELT(vec, 3, mkString(buffer));
  }
  setAttrib(vec, R_NamesSymbol, proxy_namesvec());
  UNPROTECT(1);
  return vec;
}

SEXP R_get_proxy_for_url(SEXP target_url, SEXP auto_detect, SEXP autoproxy_url){
  // Convert char to windows strings
  wchar_t *longurl = (wchar_t *) calloc(10000, sizeof(int));
  mbstowcs(longurl, CHAR(STRING_ELT(target_url, 0)), LENGTH(STRING_ELT(target_url, 0)));

  // Some settings
  WINHTTP_AUTOPROXY_OPTIONS AutoProxyOptions;
  WINHTTP_PROXY_INFO ProxyInfo;

  // Clear memory
  ZeroMemory( &AutoProxyOptions, sizeof(AutoProxyOptions) );
  ZeroMemory( &ProxyInfo, sizeof(ProxyInfo) );

  // Create the WinHTTP session.
  HINTERNET hHttpSession = WinHttpOpen( L"WinHTTP AutoProxy Sample/1.0",
      WINHTTP_ACCESS_TYPE_NO_PROXY, WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);

  // Exit if WinHttpOpen failed.
  if( !hHttpSession )
    error("Call to WinHttpOpen failed.");

  // Auto-detection doesn't work very well
  if(asLogical(auto_detect)){
    AutoProxyOptions.dwFlags = WINHTTP_AUTOPROXY_AUTO_DETECT;
    AutoProxyOptions.dwAutoDetectFlags = WINHTTP_AUTO_DETECT_TYPE_DHCP | WINHTTP_AUTO_DETECT_TYPE_DNS_A;
  }

  // Use manual URL instead
  if(isString(autoproxy_url) && LENGTH(autoproxy_url)){
    wchar_t *autourl = (wchar_t *) calloc(10000, sizeof(int));
    mbstowcs(autourl, CHAR(STRING_ELT(autoproxy_url, 0)), LENGTH(STRING_ELT(autoproxy_url, 0)));
    AutoProxyOptions.dwFlags = WINHTTP_AUTOPROXY_CONFIG_URL;
    AutoProxyOptions.lpszAutoConfigUrl = autourl;
  }

  // Use DHCP and DNS-based auto-detection.
  AutoProxyOptions.fAutoLogonIfChallenged = TRUE;

  // This downloads and runs the JavaScript to map the url to a proxy
  if(!WinHttpGetProxyForUrl( hHttpSession, longurl, &AutoProxyOptions, &ProxyInfo)){
    DWORD err = GetLastError();
    switch(err){
      case ERROR_WINHTTP_AUTO_PROXY_SERVICE_ERROR:
        error("ERROR_WINHTTP_AUTO_PROXY_SERVICE_ERROR");
      case ERROR_WINHTTP_BAD_AUTO_PROXY_SCRIPT:
        error("ERROR_WINHTTP_BAD_AUTO_PROXY_SCRIPT");
      case ERROR_WINHTTP_INCORRECT_HANDLE_TYPE:
        error("ERROR_WINHTTP_INCORRECT_HANDLE_TYPE");
      case ERROR_WINHTTP_INTERNAL_ERROR:
        error("ERROR_WINHTTP_INTERNAL_ERROR");
      case ERROR_WINHTTP_INVALID_URL:
        error("ERROR_WINHTTP_INVALID_URL");
      case ERROR_WINHTTP_LOGIN_FAILURE:
        error("ERROR_WINHTTP_LOGIN_FAILURE");
      case ERROR_WINHTTP_OPERATION_CANCELLED:
        error("ERROR_WINHTTP_OPERATION_CANCELLED");
      case ERROR_WINHTTP_UNABLE_TO_DOWNLOAD_SCRIPT:
        error("ERROR_WINHTTP_UNABLE_TO_DOWNLOAD_SCRIPT");
      case ERROR_WINHTTP_UNRECOGNIZED_SCHEME:
        error("ERROR_WINHTTP_UNRECOGNIZED_SCHEME");
      case ERROR_NOT_ENOUGH_MEMORY:
        error("ERROR_NOT_ENOUGH_MEMORY");
    }
  }

  //store output data
  char buffer[500];
  SEXP vec = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(vec, 0, ScalarLogical(
      ProxyInfo.dwAccessType == WINHTTP_ACCESS_TYPE_NAMED_PROXY ||
      ProxyInfo.dwAccessType == WINHTTP_ACCESS_TYPE_DEFAULT_PROXY));

  if(ProxyInfo.lpszProxy != NULL) {
    wcstombs(buffer, ProxyInfo.lpszProxy, 500);
    SET_VECTOR_ELT(vec, 1, mkString(buffer));
    GlobalFree((void*) ProxyInfo.lpszProxy);
  }

  if(ProxyInfo.lpszProxyBypass != NULL) {
    wcstombs(buffer, ProxyInfo.lpszProxyBypass, 500);
    SET_VECTOR_ELT(vec, 2, mkString(buffer));
    GlobalFree((void*) ProxyInfo.lpszProxyBypass );
  }

  //clean up
  WinHttpCloseHandle( hHttpSession );

  //return
  setAttrib(vec, R_NamesSymbol, auto_namesvec());
  UNPROTECT(1);
  return vec;
}

SEXP R_windows_build(void){
  DWORD dwBuild = 0;
  DWORD dwVersion = GetVersion();
  if (dwVersion < 0x80000000)
    dwBuild = (DWORD)(HIWORD(dwVersion));
  return ScalarInteger(dwBuild);
}

#else //_WIN32

SEXP R_proxy_info(void){
  return R_NilValue;
}

SEXP R_get_proxy_for_url(SEXP target_url, SEXP auto_detect, SEXP autoproxy_url){
  return R_NilValue;
}

SEXP R_windows_build(void){
  return R_NilValue;
}

#endif //_WIN32
