
#include "cli.h"
#include "errors.h"
#include "cleancall.h"

#include <errno.h>
#include <time.h>
#include <unistd.h>

/* ---------------------------------------------------------------------*/
/* Internals                                                            */
/* ---------------------------------------------------------------------*/

/* for older macOS versions */

#if defined(__APPLE__) && defined(__MACH__)
#include <mach/clock.h>
#include <mach/mach.h>
#include <mach/mach_time.h>
#include <sys/time.h>
/* It doesn't really matter what these are defined to, as long as they
   are defined */
#ifndef CLOCK_REALTIME
#define CLOCK_REALTIME 0
#endif
#ifndef CLOCK_MONOTONIC
#define CLOCK_MONOTONIC 1
#endif
static int cli_clock_gettime(int clk_id, struct timespec *t) {
  memset(t, 0, sizeof(*t));
  if (clk_id == CLOCK_REALTIME) {
    struct timeval now;
    int rv = gettimeofday(&now, NULL); // __NO_COVERAGE__
    if (rv) {                          // __NO_COVERAGE__
      return rv;                       // __NO_COVERAGE__
    }                                  // __NO_COVERAGE__
    t->tv_sec = now.tv_sec;            // __NO_COVERAGE__
    t->tv_nsec = now.tv_usec * 1000;   // __NO_COVERAGE__
    return 0;                          // __NO_COVERAGE__

  } else if (clk_id == CLOCK_MONOTONIC) {
    static uint64_t clock_start_time = 0;
    static mach_timebase_info_data_t timebase_ifo = {0, 0};
    uint64_t now = mach_absolute_time();

    if (clock_start_time == 0) {
      kern_return_t mach_status = mach_timebase_info(&timebase_ifo);

      /* appease "unused variable" warning for release builds */
      (void)mach_status;

      clock_start_time = now;
    }

    now = (uint64_t)((double)(now - clock_start_time)
                     * (double)timebase_ifo.numer
                     / (double)timebase_ifo.denom);

    t->tv_sec = now / 1000000000;
    t->tv_nsec = now % 1000000000;
    return 0;
  }
  return EINVAL; /* EINVAL - Clock ID is unknown */ // __NO_COVERAGE__
}
#else
#define cli_clock_gettime(a,b) clock_gettime(a,b)
#endif

static R_INLINE SEXP new_env(void) {
  SEXP env;
  PROTECT(env = allocSExp(ENVSXP));
  SET_FRAME(env, R_NilValue);
  SET_ENCLOS(env, R_EmptyEnv);
  SET_HASHTAB(env, R_NilValue);
  SET_ATTRIB(env, R_NilValue);
  UNPROTECT(1);
  return env;
}

double clic__get_time(void) {
  struct timespec t;
  int ret = cli_clock_gettime(CLOCK_MONOTONIC, &t);
  if (ret) R_THROW_POSIX_ERROR("Cannot query monotonic clock");
  return (double) t.tv_sec + 1e-9 * (double) t.tv_nsec;
}

SEXP clic_get_time(void) {
  struct timespec t;
  int ret = cli_clock_gettime(CLOCK_MONOTONIC, &t);
  if (ret) R_THROW_POSIX_ERROR("Cannot query monotonic clock");
  double ts = (double) t.tv_sec + 1e-9 * (double) t.tv_nsec;
  return Rf_ScalarReal(ts);
}

SEXP clic__find_var(SEXP rho, SEXP symbol) {
  SEXP ret = Rf_findVarInFrame3(rho, symbol, TRUE);
  if (ret == R_UnboundValue) {
    error("Cannot find variable `%s`.", CHAR(PRINTNAME(symbol)));

  } else if (TYPEOF(ret) == PROMSXP) {
    PROTECT(ret);
    SEXP ret2 = Rf_eval(ret, rho);
    UNPROTECT(1);
    return(ret2);

  } else {
    return ret;
  }
}

static int cli__counter = 0;

#define P(x) PROTECT(x)

SEXP cli__progress_update(SEXP bar) {
  /* We can't throw a condition from C, unfortunately... */
  SEXP call = PROTECT(Rf_lang2(install("progress_c_update"), bar));
  SEXP retv = PROTECT(Rf_eval(call, cli_pkgenv));
  UNPROTECT(2);
  return retv;
}

/* ---------------------------------------------------------------------*/
/* Public API                                                           */
/* ---------------------------------------------------------------------*/

void cli_progress_init_timer(vint **ptr) {
  *ptr = cli_timer_flag;
}

SEXP cli_progress_bar(vint **ptr, double total, SEXP config) {
  *ptr = cli_timer_flag;

  /* FALSE means no progress bar */
  if (config && Rf_isLogical(config) && LENGTH(config) == 1 &&
      !LOGICAL(config)[0]) {
    return R_NilValue;
  }

  /* If changes, synchronize with R API in progress-client.R */
  double now = clic__get_time();
  SEXP bar = PROTECT(new_env());
  SEXP show_after = PROTECT(Rf_GetOption1(Rf_install("cli.progress_show_after")));
  double sa = 2;
  if (!isNull(show_after)) sa = REAL(show_after)[0];
  SEXP clear = PROTECT(Rf_GetOption1(Rf_install("cli.progress_clear")));
  int cl = 1;
  if (!isNull(clear)) cl = LOGICAL(clear)[0];

  char idstr[64];
  static pid_t pid = 0;
  if (pid == 0) pid = getpid();
  snprintf(idstr, sizeof(idstr) - 1, "cli-%d-%d", (int) pid, cli__counter++);

  Rf_defineVar(P(Rf_install("id")),             P(Rf_mkString(idstr)),      bar);
  Rf_defineVar(P(Rf_install("name")),           P(Rf_mkString("")),         bar);
  Rf_defineVar(P(Rf_install("status")),         P(Rf_mkString("")),         bar);
  Rf_defineVar(P(Rf_install("type")),           P(Rf_mkString("iterator")), bar);
  Rf_defineVar(P(Rf_install("total")),          P(Rf_ScalarReal(total)),    bar);
  Rf_defineVar(P(Rf_install("show_after")),     P(Rf_ScalarReal(now + sa)), bar);
  Rf_defineVar(P(Rf_install("show_50")),        P(Rf_ScalarReal(now + sa/2)), bar);
  Rf_defineVar(P(Rf_install("format")),         R_NilValue,                 bar);
  Rf_defineVar(P(Rf_install("format_done")),    R_NilValue,                 bar);
  Rf_defineVar(P(Rf_install("format_failed")),  R_NilValue,                 bar);
  Rf_defineVar(P(Rf_install("clear")),          P(Rf_ScalarLogical(cl)),    bar);
  Rf_defineVar(P(Rf_install("auto_terminate")), P(Rf_ScalarLogical(1)),     bar);
  Rf_defineVar(P(Rf_install("envkey")),         R_NilValue,                 bar);
  Rf_defineVar(P(Rf_install("current")),        P(Rf_ScalarReal(0)),        bar);
  Rf_defineVar(P(Rf_install("start")),          P(Rf_ScalarReal(now)),      bar);
  Rf_defineVar(P(Rf_install("statusbar")),      R_NilValue,                 bar);
  Rf_defineVar(P(Rf_install("tick")),           P(Rf_ScalarReal(0)),        bar);
  Rf_defineVar(P(Rf_install("extra")),          R_NilValue,                 bar);

  UNPROTECT(30);

  if (!config || Rf_isNull(config)) {
    /* NULL pointer or R NULL, use defaults */

  } else if (Rf_isLogical(config) && LENGTH(config) == 1) {
    /* TRUE, use defaults */

  } else if (TYPEOF(config) == VECSXP) {
    /* Proper config */
    int i, n = LENGTH(config);
    SEXP nms = getAttrib(config, R_NamesSymbol);
    if (isNull(nms)) {
      error("Invalid cli progress bar configuration, list elements must "
            "be named.");
    }
    for (i = 0; i < n; i++) {
      Rf_defineVar(
        Rf_install(CHAR(STRING_ELT(nms, i))),
        VECTOR_ELT(config, i),
        bar
     );
    }

  } else if (TYPEOF(config) == STRSXP) {
    /* String, use as name */
    Rf_defineVar(Rf_install("name"), config, bar);

  } else {
    error("Unknown cli progress bar configuation, see manual.");
  }

  UNPROTECT(3);
  return bar;
}

void cli_progress_set_name(SEXP bar, const char *name) {
  PROTECT(bar);
  if (isNull(bar)) {
    UNPROTECT(1);
    return;
  }
  Rf_defineVar(PROTECT(Rf_install("name")), PROTECT(Rf_mkString(name)), bar);
  UNPROTECT(3);
}

void cli_progress_set_status(SEXP bar, const char *status) {
  PROTECT(bar);
  if (isNull(bar)) {
    UNPROTECT(1);
    return;
  }
  Rf_defineVar(PROTECT(Rf_install("status")), PROTECT(Rf_mkString(status)), bar);
  UNPROTECT(3);
}

void cli_progress_set_type(SEXP bar, const char *type) {
  PROTECT(bar);
  if (isNull(bar)) {
    UNPROTECT(1);
    return;
  }
  Rf_defineVar(PROTECT(Rf_install("type")), PROTECT(Rf_mkString(type)), bar);
  UNPROTECT(3);
}

void cli_progress_set_format(SEXP bar, const char *format) {
  PROTECT(bar);
  if (isNull(bar)) {
    UNPROTECT(1);
    return;
  }
  Rf_defineVar(PROTECT(Rf_install("format")), PROTECT(Rf_mkString(format)), bar);
  UNPROTECT(3);
}

void cli_progress_set_clear(SEXP bar, int clear) {
  PROTECT(bar);
  if (isNull(bar)) {
    UNPROTECT(1);
    return;
  }
  Rf_defineVar(PROTECT(Rf_install("clear")), PROTECT(Rf_ScalarLogical(clear)), bar);
  UNPROTECT(3);
}

void cli_progress_set(SEXP bar, double set) {
  PROTECT(bar);
  if (isNull(bar)) {
    UNPROTECT(1);
    return;
  }
  Rf_defineVar(PROTECT(Rf_install("current")), PROTECT(ScalarReal(set)), bar);
  if (*cli_timer_flag) {
    if (cli__reset) *cli_timer_flag = 0;
    double now = clic__get_time();
    SEXP show_after = PROTECT(clic__find_var(bar, PROTECT(Rf_install("show_after"))));
    if (now > REAL(show_after)[0]) {
      cli__progress_update(bar);
    } else {
      SEXP show_50 = PROTECT(clic__find_var(bar, PROTECT(Rf_install("show_50"))));
      SEXP total = PROTECT(clic__find_var(bar, PROTECT(Rf_install("total"))));
      if (now > REAL(show_50)[0] &&
          REAL(total)[0] != NA_REAL &&
          set <= REAL(total)[0] / 2) {
        cli__progress_update(bar);
      }
      UNPROTECT(4);
    }
    UNPROTECT(2);
  }
  UNPROTECT(3);
}

void cli_progress_add(SEXP bar, double inc) {
  PROTECT(bar);
  if (isNull(bar)) {
    UNPROTECT(1);
    return;
  }
  SEXP current = PROTECT(Rf_install("current"));
  double crnt = REAL(PROTECT(clic__find_var(bar, current)))[0];
  Rf_defineVar(current, PROTECT(ScalarReal(crnt + inc)), bar);
  if (*cli_timer_flag) {
    if (cli__reset) *cli_timer_flag = 0;
    double now = clic__get_time();
    SEXP show_after = PROTECT(clic__find_var(bar, PROTECT(Rf_install("show_after"))));
    if (now > REAL(show_after)[0]) {
      cli__progress_update(bar);
    } else {
      SEXP show_50 = PROTECT(clic__find_var(bar, PROTECT(Rf_install("show_50"))));
      SEXP total = PROTECT(clic__find_var(bar, PROTECT(Rf_install("total"))));
      if (now > REAL(show_50)[0] &&
          REAL(total)[0] != NA_REAL &&
          crnt + inc <= REAL(total)[0] / 2) {
        cli__progress_update(bar);
      }
      UNPROTECT(4);
    }
    UNPROTECT(2);
  }
  UNPROTECT(4);
}

void cli_progress_done(SEXP bar) {
  PROTECT(bar);
  if (isNull(bar)) {
    UNPROTECT(1);
    return;
  }
  SEXP call = PROTECT(Rf_lang2(PROTECT(install("progress_c_done")), bar));
  PROTECT(Rf_eval(call, cli_pkgenv));
  UNPROTECT(4);
}

int cli_progress_num(void) {
  SEXP clienv = PROTECT(clic__find_var(cli_pkgenv, Rf_install("clienv")));
  if (clienv == R_UnboundValue) error("Cannot find 'clienv'");
  SEXP bars = PROTECT(clic__find_var(clienv, Rf_install("progress")));
  if (bars == R_UnboundValue) error("Cannot find 'clienv$progress'");
  UNPROTECT(2);
  return LENGTH(bars);
}

extern double cli_speed_time;

void cli_progress_sleep(int s, long ns) {
  struct timespec ts;
  int s2 = s;
  long ns2 = ns;
  if (cli_speed_time != 1.0) {
    s2 = s / cli_speed_time;                            // __NO_COVERAGE__
    ns2 =                                               // __NO_COVERAGE__
      (s / cli_speed_time - s2) * 1000 * 1000 * 1000 +  // __NO_COVERAGE__
      ns / cli_speed_time;                              // __NO_COVERAGE__
  }                                                     // __NO_COVERAGE__
  ts.tv_sec = s2;
  ts.tv_nsec = ns2;
  nanosleep(&ts, NULL);
}

void cli_progress_update(SEXP bar, double set, double inc, int force) {
  double crnt = 0;
  PROTECT(bar);
  if (isNull(bar)) {
    UNPROTECT(1);
    return;
  }
  SEXP current = PROTECT(Rf_install("current"));
  if (set >= 0) {
    crnt = set;
    Rf_defineVar(current, PROTECT(ScalarReal(set)), bar);
    UNPROTECT(1);
  } else {
    crnt = REAL(PROTECT(clic__find_var(bar, current)))[0];
    if (inc != 0) {
      crnt = crnt + inc;
      Rf_defineVar(current, PROTECT(ScalarReal(crnt)), bar);
      UNPROTECT(1);
    }
    UNPROTECT(1);
  }
  if (force) {
    cli__progress_update(bar);
  } else if (*cli_timer_flag) {
    if (cli__reset) *cli_timer_flag = 0;
    double now = clic__get_time();
    SEXP show_after = PROTECT(clic__find_var(bar, PROTECT(Rf_install("show_after"))));
    if (now > REAL(show_after)[0]) {
      cli__progress_update(bar);
    } else {
      SEXP show_50 = PROTECT(clic__find_var(bar, PROTECT(Rf_install("show_50"))));
      SEXP total = PROTECT(clic__find_var(bar, PROTECT(Rf_install("total"))));
      if (now > REAL(show_50)[0] &&
          REAL(total)[0] != NA_REAL &&
          crnt <= REAL(total)[0] / 2) {
        cli__progress_update(bar);
      }
      UNPROTECT(4);

    }
    UNPROTECT(2);
  }

  UNPROTECT(2);
}
