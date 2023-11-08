
#ifdef __INTEL_COMPILER
#define _BSD_SOURCE 1
#define _POSIX_C_SOURCE  200809L
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "utils.h"


bool verbose_mode = false;


void verbose_printf(const char *format, ...) {
    va_list args;
    va_start(args, format);

    if (verbose_mode) {
        vprintf(format, args);
        fflush(stdout);
    }

    va_end(args);
}


// Remove an element from an array by replacing it with the last element. Note
// that this can alter the order of elements in the array. Returns new length
// of array.
int remove_element(int* ar, int len, int idx) {
    ar[idx] = ar[len-1];
    return len-1;
}


bool array_contains(int* ar, int len, int value) {
    for (int i=0; i<len; i++) {
        if (ar[i] == value)
            return true;
    }

    return false;
}
