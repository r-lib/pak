
#ifdef _WIN32

#include "date.h"

struct tm *gmtime_r(const time_t *timep, struct tm *result)
{
	/* gmtime() in MSVCRT.DLL is thread-safe, but not reentrant */
	memcpy(result, gmtime(timep), sizeof(struct tm));
	return result;
}

struct tm *localtime_r(const time_t *timep, struct tm *result)
{
	/* localtime() in MSVCRT.DLL is thread-safe, but not reentrant */
	memcpy(result, localtime(timep), sizeof(struct tm));
	return result;
}

#endif

#define UNUSED(x) (void)(x)

void R_parsedate_shut_up_about_empty_translation_unit(void) {
  const char *xx = "Here! Better?";
  UNUSED(xx);
}
