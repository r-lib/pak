
#ifndef GIT_DATE_H
#define GIT_DATE_H

#include <time.h>
#include <string.h>
#include "mingw.h"

enum date_mode {
	DATE_NORMAL = 0,
	DATE_RELATIVE,
	DATE_SHORT,
	DATE_LOCAL,
	DATE_ISO8601,
	DATE_RFC2822,
	DATE_RAW
};

unsigned long approxidate_careful(const char *date, int *error_ret);
enum date_mode parse_date_format(const char *format);
int parse_date(const char *date, char *result, int maxlen);
int parse_date_basic(const char *date, unsigned long *timestamp, int *offset);

#endif
