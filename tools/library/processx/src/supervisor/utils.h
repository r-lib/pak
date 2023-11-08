
#ifndef R_PROCESSX_SUPERVISOR_UTILS_H
#define R_PROCESSX_SUPERVISOR_UTILS_H

#include <stdbool.h>


extern bool verbose_mode;


void verbose_printf(const char *format, ...);

int remove_element(int* ar, int len, int idx) ;
bool array_contains(int* ar, int len, int value);

#endif
