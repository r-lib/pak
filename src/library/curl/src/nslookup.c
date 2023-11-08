//libcurl internal punycode converter
#ifdef _WIN32
int jeroen_win32_idn_to_ascii(const char *in, char **out);
#endif

//needed to expose inet_ntop
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif

//getaddrinfo is an extension (not C99)
#if !defined(_WIN32) && !defined(__sun) && !defined(_POSIX_C_SOURCE)
#define _POSIX_C_SOURCE 200112L
#endif

#include <Rinternals.h>
#include <string.h>

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#endif

SEXP R_nslookup(SEXP hostname, SEXP ipv4_only) {
  /* Because gethostbyname() is deprecated */
  struct addrinfo hints = {0};
  if(asLogical(ipv4_only))
    hints.ai_family = AF_INET; //only allow ipv4
  struct addrinfo *addr;
  const char * hoststr = CHAR(STRING_ELT(hostname, 0));
#ifdef _WIN32
  if(Rf_getCharCE(STRING_ELT(hostname, 0)) == CE_UTF8){
    char * punycode;
    if(jeroen_win32_idn_to_ascii(hoststr, &punycode))
      hoststr = punycode;
  }
#endif
  if(getaddrinfo(hoststr, NULL, &hints, &addr))
    return R_NilValue;

  // count number of hits
  int len = 0;
  struct addrinfo * cur = addr;
  while(cur != NULL){
    len++;
    cur = cur->ai_next;
  }

  //allocate output
  SEXP out = PROTECT(allocVector(STRSXP, len));

  //extract the values
  cur = addr;
  for(size_t i = 0; i < len; i++) {
    struct sockaddr *sa = cur->ai_addr;

    /* IPv4 vs v6 */
    char ip[INET6_ADDRSTRLEN];
    if (sa->sa_family == AF_INET) {
      struct sockaddr_in *sa_in = (struct sockaddr_in*) sa;
      inet_ntop(AF_INET, &(sa_in->sin_addr), ip, INET_ADDRSTRLEN);
    } else {
      struct sockaddr_in6 *sa_in = (struct sockaddr_in6*) sa;
      inet_ntop(AF_INET6, &(sa_in->sin6_addr), ip, INET6_ADDRSTRLEN);
    }
    SET_STRING_ELT(out, i, mkChar(ip));
    cur = cur->ai_next;
  }
  UNPROTECT(1);
  freeaddrinfo(addr);
  return out;
}
