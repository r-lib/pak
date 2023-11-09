
#include "cli.h"
#include "errors.h"
#include "cleancall.h"

#include <Rversion.h>

#ifdef WIN32
extern Rboolean EmitEmbeddedUTF8;
#endif

SEXP clic_get_embedded_utf8(void) {
#ifdef WIN32
#if R_VERSION < R_Version(4, 0, 0)
  error("get_embedded_utf8() needs at least R 4.0.0");
#else
  return ScalarLogical(EmitEmbeddedUTF8);
#endif
#else
  error("get_embedded_utf8() only works on Windows");
  return R_NilValue;
#endif
}

SEXP clic_set_embedded_utf8(SEXP value) {
#ifdef WIN32
#if R_VERSION < R_Version(4, 0, 0)
  error("set_embedded_utf8() needs at least R 4.0.0");
#else
  Rboolean prev = EmitEmbeddedUTF8;
  EmitEmbeddedUTF8 = LOGICAL(value)[0];
  return ScalarLogical(prev);
#endif
#else
  error("set_embedded_utf8() only works on Windows");
  return R_NilValue;
#endif
}
