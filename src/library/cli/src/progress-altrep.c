
#include "cli.h"

#include <Rversion.h>

#if R_VERSION < R_Version(3, 5, 0)

SEXP clic_progress_along(SEXP seq, SEXP bar) {
  error("internal cli error");
  return R_NilValue;
}

SEXP clic_make_timer() { return R_NilValue; }

SEXP clic_update_due() {
  return ScalarLogical(*cli_timer_flag);
}

SEXP clic_dataptr(SEXP x) {
  int i, n = LENGTH(x);
  SEXP ret = PROTECT(allocVector(INTSXP, n));
  for (i = 0; i < n; i++) {
    INTEGER(ret)[i] = INTEGER(x)[i] * 2;
  }

  UNPROTECT(1);
  return ret;
}

#else

#include <R_ext/Altrep.h>

R_altrep_class_t progress_along_t;
R_altrep_class_t disable_gc_t;
R_altrep_class_t cli_timer_t;

static SEXP cli__current_progress_bar = 0;
static SEXP cli__disable_gc = 0;

void *disable_gc_DataPtr(SEXP x, Rboolean writeable) {
  /* We can't throw a condition from C, unfortunately... */
  SEXP call = PROTECT(Rf_lang2(install("progress_altrep_update"),
                               cli__current_progress_bar));
  Rf_eval(call, cli_pkgenv);
  UNPROTECT(1);
  return NULL;
}

/* --------------------------------------------------------------------- */

static SEXP cli__timer = 0;

SEXP clic_make_timer(void) {
  return cli__timer;
}

SEXP clic_update_due(void) {
  /* This will make a copy, which leads to better semantics. */
  return ScalarLogical(*cli_timer_flag);
}

R_xlen_t cli_timer_Length(SEXP x) {
  return 1;
}

void* cli_timer_DataPtr(SEXP x, Rboolean writeable) {
  return (void*) cli_timer_flag;
}

int cli_timer_Elt(SEXP x, R_xlen_t i) {
  return *cli_timer_flag;
}

/* --------------------------------------------------------------------- */

SEXP clic_progress_along(SEXP seq, SEXP bar) {
  SEXP val = R_new_altrep(progress_along_t, seq, bar);
  return val;
}

R_xlen_t progress_along_Length(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  return XLENGTH(data1);
}

SEXP progress_along_Duplicate(SEXP x, Rboolean deep) {
  SEXP data1 = R_altrep_data1(x);
  SEXP bar = R_altrep_data2(x);
  return clic_progress_along(data1, bar);
}

Rboolean progress_along_Inspect(SEXP x,
                                int pre,
                                int deep,
                                int pvec,
                                void (*inspect_subtree)(SEXP, int, int, int)) {

  Rprintf(" progress_along %s\n", type2char(TYPEOF(x)));
  return FALSE;
}

void* progress_along_Dataptr(SEXP x, Rboolean writeable) {
  SEXP data1 = R_altrep_data1(x);
  if (writeable) {
    return DATAPTR(data1);
  } else {
    return (void*) DATAPTR_RO(data1);
  }
}

const void* progress_along_Dataptr_or_null(SEXP x) {
  SEXP data1 = R_altrep_data1(x);
  return DATAPTR_OR_NULL(data1);
}

// TODO: long vector support?
int progress_along_Elt(SEXP x, R_xlen_t i) {
  if (*cli_timer_flag) {
    if (cli__reset) *cli_timer_flag = 0;
    SEXP bar = R_altrep_data2(x);
    double now = clic__get_time();
    Rf_defineVar(PROTECT(Rf_install("current")), PROTECT(ScalarReal((int) i)), bar);
    cli__current_progress_bar = bar;
    SEXP show_after = clic__find_var(bar, Rf_install("show_after"));
    if (now > REAL(show_after)[0]) DATAPTR(cli__disable_gc);
    UNPROTECT(2);
  }
  return (int) (i + 1);
}

R_xlen_t progress_along_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf) {
  SEXP data1 = R_altrep_data1(x);
  return INTEGER_GET_REGION(data1, i, n, buf);
}

SEXP progress_along_Sum(SEXP x, Rboolean narm) {
  SEXP data1 = R_altrep_data1(x);
  R_xlen_t n = XLENGTH(data1);
  return ScalarReal(n * ( n + 1.0) / 2.0);
}

SEXP progress_along_Max(SEXP x, Rboolean narm) {
  SEXP data1 = R_altrep_data1(x);
  R_xlen_t n = XLENGTH(data1);
  return ScalarReal(n);
}

SEXP progress_along_Min(SEXP x, Rboolean narm) {
  SEXP data1 = R_altrep_data1(x);
  R_xlen_t n = XLENGTH(data1);
  if (n > 0) {
    return ScalarInteger(1);
  } else {
    return ScalarReal(R_PosInf);
  }
}

int progress_along_Is_sorted(SEXP x) {
  return SORTED_INCR;
}

void cli_init_altrep(DllInfo *dll) {

  // -- progress_along_t --------------------------------------------------

  progress_along_t = R_make_altinteger_class("progress_along_t", "cli", dll);

  // override ALTREP methods
  R_set_altrep_Duplicate_method(progress_along_t, progress_along_Duplicate);
  R_set_altrep_Inspect_method(progress_along_t, progress_along_Inspect);
  R_set_altrep_Length_method(progress_along_t, progress_along_Length);

  // override ALTVEC methods
  R_set_altvec_Dataptr_method(progress_along_t, progress_along_Dataptr);
  R_set_altvec_Dataptr_or_null_method(progress_along_t, progress_along_Dataptr_or_null);

  // override ALTINTEGER methods
  R_set_altinteger_Elt_method(progress_along_t, progress_along_Elt);
  R_set_altinteger_Get_region_method(progress_along_t, progress_along_Get_region);
  R_set_altinteger_Sum_method(progress_along_t, progress_along_Sum);
  R_set_altinteger_Max_method(progress_along_t, progress_along_Max);
  R_set_altinteger_Min_method(progress_along_t, progress_along_Min);
  // R_set_altinteger_No_NA_method(progress_along_t, progress_along_No_NA);
  R_set_altinteger_Is_sorted_method(progress_along_t, progress_along_Is_sorted);

  // -- disable_gc_t --------------------------------------------------

  disable_gc_t = R_make_altinteger_class("disable_gc_t", "cli", dll);
  R_set_altvec_Dataptr_method(disable_gc_t, disable_gc_DataPtr);

  cli__disable_gc = R_new_altrep(disable_gc_t, R_NilValue, R_NilValue);
  R_PreserveObject(cli__disable_gc);

  // -- cli_timer_t ---------------------------------------------------

#if R_VERSION < R_Version(3, 6, 0)
  cli_timer_t = R_make_altinteger_class("cli_timer_t", "cli", dll);
  R_set_altrep_Length_method(cli_timer_t, cli_timer_Length);
  R_set_altvec_Dataptr_method(cli_timer_t, cli_timer_DataPtr);
  R_set_altinteger_Elt_method(cli_timer_t, cli_timer_Elt);
#else
  cli_timer_t = R_make_altlogical_class("cli_timer_t", "cli", dll);
  R_set_altrep_Length_method(cli_timer_t, cli_timer_Length);
  R_set_altvec_Dataptr_method(cli_timer_t, cli_timer_DataPtr);
  R_set_altlogical_Elt_method(cli_timer_t, cli_timer_Elt);
#endif

  cli__timer = R_new_altrep(cli_timer_t, R_NilValue, R_NilValue);
  MARK_NOT_MUTABLE(cli__timer);
  R_PreserveObject(cli__timer);
}

SEXP clic_dataptr(SEXP x) {
  int i, n = LENGTH(x);
  SEXP ret = PROTECT(allocVector(INTSXP, n));
  for (i = 0; i < n; i++) {
    INTEGER(ret)[i] = INTEGER(x)[i] + INTEGER_RO(x)[i];
  }

  UNPROTECT(1);
  return ret;
}

#endif
