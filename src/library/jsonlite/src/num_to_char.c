#include <Rdefines.h>
#include <Rinternals.h>
#include <stdlib.h>
#include "modp_numtoa.h"

SEXP R_num_to_char(SEXP x, SEXP digits, SEXP na_as_string, SEXP use_signif, SEXP always_decimal) {
  int len = length(x);
  int na_string = asLogical(na_as_string);
  int signif = asLogical(use_signif);
  int always_dec = asLogical(always_decimal);
  char buf[32];
  SEXP out = PROTECT(allocVector(STRSXP, len));
  if(isInteger(x)){
    for (int i=0; i<len; i++) {
      if(INTEGER(x)[i] == NA_INTEGER){
        if(na_string == NA_LOGICAL){
          SET_STRING_ELT(out, i, NA_STRING);
        } else if(na_string){
          SET_STRING_ELT(out, i, mkChar("\"NA\""));
        } else {
          SET_STRING_ELT(out, i, mkChar("null"));
        }
      } else {
        modp_itoa10(INTEGER(x)[i], buf);
        SET_STRING_ELT(out, i, mkChar(buf));
      }
    }
  } else if(isReal(x)) {
    int precision = asInteger(digits);
    int sig_digits = signif ? ceil(fmin(17, precision)) : 0;
    double * xreal = REAL(x);
    for (int i=0; i<len; i++) {
      double val = xreal[i];
      if(!R_FINITE(val)){
        if(na_string == NA_LOGICAL){
          SET_STRING_ELT(out, i, NA_STRING);
        } else if(na_string){
          if(ISNA(val)){
            SET_STRING_ELT(out, i, mkChar("\"NA\""));
          } else if(ISNAN(val)){
            SET_STRING_ELT(out, i, mkChar("\"NaN\""));
          } else if(val == R_PosInf){
            SET_STRING_ELT(out, i, mkChar("\"Inf\""));
          } else if(val == R_NegInf){
            SET_STRING_ELT(out, i, mkChar("\"-Inf\""));
          } else {
            error("Unrecognized non finite value.");
          }
        } else {
          SET_STRING_ELT(out, i, mkChar("null"));
        }
      } else {
        if(precision == NA_INTEGER){
          snprintf(buf, 32, "%.15g", val);
        } else if(signif){
          //use signifant digits rather than decimal digits
          snprintf(buf, 32, "%.*g", sig_digits, val);
        } else if(precision > -1 && precision < 10 && fabs(val) < 2147483647 && fabs(val) > 1e-5) {
          //preferred method: fast with fixed decimal digits
          //does not support large numbers or scientific notation
          modp_dtoa2(val, buf, precision);
        } else {
          //fall back on sprintf (includes scientific notation)
          //funky formula is mostly to convert decimal digits into significant digits
          int decimals = ceil(fmin(17, fmax(1, log10(fabs(val))) + precision));
          snprintf(buf, 32, "%.*g", decimals, val);
        }
        //if always_decimal = TRUE, then append .0 to whole numbers
        if(always_dec && strspn(buf, "0123456789-") == strlen(buf)){
          strcat(buf, ".0");
        }
        SET_STRING_ELT(out, i, mkChar(buf));
      }
    }
  } else {
    error("num_to_char called with invalid object type.");
  }

  UNPROTECT(1);
  return out;
}
