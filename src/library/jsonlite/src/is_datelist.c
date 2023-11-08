#include <Rinternals.h>
#include <string.h>

//tests if all elements are either NULL or POSIXct, and at least one POSIXct
SEXP C_is_datelist(SEXP x) {
  size_t len = Rf_length(x);
  if(!Rf_isVectorList(x) || len == 0)
    return ScalarLogical(FALSE);

  // Need
  int status = FALSE;
  for (size_t i = 0; i < len; i++) {
    SEXP el = VECTOR_ELT(x, i);
    if(Rf_isNull(el))
      continue;
    if(Rf_isString(el) && Rf_length(el) > 0 && !strcmp(CHAR(STRING_ELT(el, 0)), "NA"))
      continue;
    if(Rf_isNumeric(el) && Rf_inherits(el, "POSIXct")){
      status = TRUE; //at least one date
    } else {
      return ScalarLogical(FALSE); // quit immediately
    }
  }

  return ScalarLogical(status);
}
