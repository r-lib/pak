
#include "../processx.h"
#include <io.h>
#include <fcntl.h>

SEXP processx_write_raw_stdout(SEXP raw_data) {
  const unsigned char *data = RAW(raw_data);
  size_t nbytes = LENGTH(raw_data);

  _setmode(1, _O_BINARY);

  while (nbytes > 0) {
    int written = _write(1, data, (unsigned int) nbytes);
    if (written < 0) R_THROW_SYSTEM_ERROR("Cannot write to stdout");
    data += written;
    nbytes -= written;
  }

  return R_NilValue;
}

SEXP processx_disable_crash_dialog(void) {
  /* TODO */
  return R_NilValue;
}

SEXP processx__echo_on(void) {
  R_THROW_ERROR("Only implemented on Unix");
  return R_NilValue;
}

SEXP processx__echo_off(void) {
  R_THROW_ERROR("Only implemented on Unix");
  return R_NilValue;
}

SEXP processx_make_fifo(SEXP name) {
  /* TODO */
  return R_NilValue;
}
