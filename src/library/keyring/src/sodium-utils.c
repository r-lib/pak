#include <stdlib.h>

void
sodium_memzero(void * const pnt, const size_t len)
{
      volatile unsigned char *volatile pnt_ =
        (volatile unsigned char *volatile) pnt;
    size_t i = (size_t) 0U;

    while (i < len) {
        pnt_[i++] = 0U;
    }
}
