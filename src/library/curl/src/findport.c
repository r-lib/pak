#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <sys/select.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#endif

#ifdef _WIN32
static const char *formatError(DWORD res){
  static char buf[1000], *p;
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL, res,
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                buf, 1000, NULL);
  p = buf+strlen(buf) -1;
  if(*p == '\n') *p = '\0';
  p = buf+strlen(buf) -1;
  if(*p == '\r') *p = '\0';
  p = buf+strlen(buf) -1;
  if(*p == '.') *p = '\0';
  return buf;
}
#define getsyserror() formatError(GetLastError())
#else
#define getsyserror() strerror(errno)
#endif

/* check for system errors */
static void syserror_if(int err, const char * what){
  if(err)
    Rf_errorcall(R_NilValue, "System failure for: %s (%s)", what, getsyserror());
}

static int port_is_available(int port){

  // define server socket
  struct sockaddr_in serv_addr;
  memset(&serv_addr, '0', sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  serv_addr.sin_port = htons(port);

  //creates the listening socket
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  syserror_if(sockfd < 0, "socket()");

  //Allows immediate reuse of a port in TIME_WAIT state.
  //for Windows see TcpTimedWaitDelay (doesn't work)
#ifndef _WIN32
  int enable = 1;
  syserror_if(setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &enable, sizeof(int)) < 0, "SO_REUSEADDR");
#endif

  int res = bind(sockfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr));
  #ifdef _WIN32
  closesocket(sockfd);
  #else
  close(sockfd);
  #endif
  return res == 0;
}

SEXP R_findport(SEXP candidates){
  for(int i = 0; i < Rf_length(candidates); i++){
    int port = INTEGER(candidates)[i];
    if(port_is_available(port)){
      return ScalarInteger(port);
    }
  }
  return R_NilValue;
}
