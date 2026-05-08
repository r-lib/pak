#include <curl/curl.h>
#if LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR < 73
#error Local libcurl version is too old
#endif
