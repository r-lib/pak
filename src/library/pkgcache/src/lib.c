
#include "pkgcache.h"
#include "errors.h"
#include "winfiles.h"

#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <ctype.h>

#include <R_ext/GraphicsEngine.h>

#define STR1(x) STRING_ELT(x, 0)
#define HASH_SIZE 256
#define MAX_COLL 10

#define ERROR_TOO_MANY_COLUMNS 1
#define ERROR_HASH_TABLE_FULL  2

static R_INLINE int hash_string(char *str, int strlen) {
  int backup = str[strlen];
  str[strlen] = '\0';
  unsigned long hash = 5381;
  int c;
  while ((c = *str++)) {
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
  }

  str--;
  *str = backup;
  return hash % HASH_SIZE;
}

struct hash_table {
  SEXP nms;
  const SEXP *nmsptr;
  SEXP cols;
  int *tab;
  int tablen;
  int nfld;
  int max_cols;
  int npkgs;
};

static void hash_create(struct hash_table *table, SEXP nms, SEXP cols,
                        SEXP tab, int max_cols, int npkgs) {
  table->nms = nms;
  table->nmsptr = STRING_PTR_RO(nms);
  table->cols = cols;
  table->tab = INTEGER(tab);
  table->tablen = LENGTH(tab);
  table->nfld = 0;
  table->max_cols = max_cols;
  table->npkgs = npkgs;
  memset(table->tab, 0, sizeof(int) * table->tablen);
}

static inline int hash_update(struct hash_table *table, char *key, int keylen,
                              int npkg, SEXP val, int err) {
  int len = table->tablen;
  int *t = table->tab;
  int hash = hash_string(key, keylen);
  int start = hash * MAX_COLL;

  for (; start < len; start++) {
    int p = t[start];
    if (p == 0) {

      if (table->nfld == table->max_cols) {
        if (err) {                                                   // __NO_COVERAGE__
          R_THROW_ERROR(                                             // __NO_COVERAGE__
            "Internal pkgcache error, too many different fields in " // __NO_COVERAGE__
            "PACKAGES or DESCRIPTION data, please report a bug"      // __NO_COVERAGE__
          );                                                         // __NO_COVERAGE__
        }                                                            // __NO_COVERAGE__
        return ERROR_TOO_MANY_COLUMNS;                               // __NO_COVERAGE__
      }

      SET_STRING_ELT(table->nms, table->nfld, Rf_mkCharLenCE(key, keylen, CE_NATIVE));
      SET_VECTOR_ELT(table->cols, table->nfld, allocVector(STRSXP, table->npkgs));
      SET_STRING_ELT(VECTOR_ELT(table->cols, table->nfld), npkg, val);
      table->nfld += 1;
      t[start] = table->nfld;
      return 0;

    } else {
      p--;
      if (!strncmp(key, CHAR(table->nmsptr[p]), keylen)) {
        SET_STRING_ELT(VECTOR_ELT(table->cols, p), npkg, val);
        return 0;
      }
    }
  }                                                                      // __NO_COVERAGE__

  if (err) {                                                             // __NO_COVERAGE__
    R_THROW_ERROR(                                                       // __NO_COVERAGE__
      "Internal pkgcache error, hash table is full, please report a bug" // __NO_COVERAGE__
    );                                                                   // __NO_COVERAGE__
  }                                                                      // __NO_COVERAGE__
  return ERROR_HASH_TABLE_FULL;                                          // __NO_COVERAGE__
}

/* --------------------------------------------------------------------- */

SEXP pkgcache__read_file_raw(const char *cpath) {
  SEXP result = R_NilValue;
  int err;
  int fd = open_file(cpath, O_RDONLY);

  if (fd == -1) {
    return(R_FORMAT_SYSTEM_ERROR("Cannot open file `%s`", cpath));
  }

  off_t len = lseek(fd, 0, SEEK_END);
  if (len == -1) {
    err = errno;                                                       // __NO_COVERAGE__
    close(fd);                                                         // __NO_COVERAGE__
    return R_FORMAT_SYSTEM_ERROR_CODE(err, "Cannot seek `%s`", cpath); // __NO_COVERAGE__
  }
  off_t len2 = lseek(fd, 0, SEEK_SET);
  if (len2 == -1) {
    err = errno;                                                       // __NO_COVERAGE__
    close(fd);                                                         // __NO_COVERAGE__
    return R_FORMAT_SYSTEM_ERROR_CODE(err, "Cannot seek `%s`", cpath); // __NO_COVERAGE__
  }

  /* TODO: should use cleancall to close the file if allocVector fails */

  result = PROTECT(allocVector(RAWSXP, len));

  ssize_t ret = read(fd, RAW(result), len);
  if (ret == -1) {
    err = errno;                                                       // __NO_COVERAGE__
    close(fd);                                                         // __NO_COVERAGE__
    UNPROTECT(1);                                                      // __NO_COVERAGE__
    return R_FORMAT_SYSTEM_ERROR_CODE(err, "Cannot read `%s`", cpath); // __NO_COVERAGE__
  }

  close(fd);

  UNPROTECT(1);
  return result;
}

/* --------------------------------------------------------------------- */

SEXP pkgcache_read_raw(SEXP paths) {
  R_xlen_t i, len = XLENGTH(paths);
  SEXP result = PROTECT(allocVector(VECSXP, len));

  for (i = 0; i < len; i++) {
    SET_VECTOR_ELT(
      result,
      i,
      pkgcache__read_file_raw(CHAR(STRING_ELT(paths, i)))
    );
  }

  UNPROTECT(1);
  return result;
}

/* --------------------------------------------------------------------- */

#define S_BG 0                  /* beginning of the file */
#define S_KW 1                  /* inside a keyword */
#define S_VL 2                  /* inside a value */
#define S_NL 3                  /* right after a newline */
#define S_WS 4                  /* after newline + space */

SEXP pkgcache_parse_description_raw(SEXP raw) {
  char *p = NULL, *start = (char*) RAW(raw);
  char *end = start + XLENGTH(raw);
  int state = S_BG;
  char *kw = NULL, *vl = NULL;
  int kwsize = 0, vlsize = 0;
  int linum = 1;

  SEXP result = PROTECT(allocVector(STRSXP, 200));
  SEXP names = PROTECT(allocVector(STRSXP, 200));
  int ridx = 0;

  for (p = start; p < end; ) {
    switch (state) {

    /* -- at the begining ---------------------------------------------- */
    case S_BG:
      if (*p == ':' || *p == '\r' || *p == '\n' || *p == ' ' || *p == '\t') {
        R_THROW_ERROR(
          "Invalid DESCRIPTION file, must start with an "
          "alphanumeric character"
        );
      }
      /* Otherwise it must be the start of a keyword */
      kw = p++;
      state = S_KW;

      break;

    /* -- within a keyword --------------------------------------------- */
    case S_KW:
      /* Maybe the keyword ends here, and a value starts */
      if (*p == ':') {
        kwsize = p - kw;
        p++;
        vl = p;
        if (*vl == ' ') vl++;
        state = S_VL;

      /* A newline within a keyword is an error */
      } else if (*p == '\n') {
        R_THROW_ERROR(
          "Line %d invalid in DESCRIPTION: must be of form `key: value`",
          linum
        );

      /* Otherwise we are inside the keyword */
      } else {
        p++;
      }

      break;

    /* --- within a value ---------------------------------------------- */
    case S_VL:
      /* newline might be the end of the value, if no continuation. */
      if (*p == '\n') {
        state = S_NL;
        vlsize = p - vl;
        p++;
        linum++;

      } else {
        p++;
      }
      break;

    /* -- right after a newline ---------------------------------------- */
    case S_NL:
      /* maybe a continuation line */
      if (*p == ' ' || *p == '\t') {
        state = S_WS;
        p++;

      /* othewise we can save the field, and start parsing the next one */
      } else {
        SET_STRING_ELT(result, ridx, Rf_mkCharLenCE(vl, vlsize, CE_BYTES));
        SET_STRING_ELT(names, ridx, Rf_mkCharLenCE(kw, kwsize, CE_NATIVE));
        ridx++;
        kw = p;
        state = S_KW;
        p++;
      }

      break;

    /* -- after continuation space ------------------------------------- */
    case S_WS:
      /* more whitespace? */
      if (*p == ' ' || *p == '\t') {
        p++;

      /* otherwise continuation line, so this is still the value */
      } else {
        state = S_VL;
        p++;
      }
      break;

    /* ----------------------------------------------------------------- */
    default:
      R_THROW_ERROR("Internal DESCRIPTION parser error"); // __NO_COVERAGE__
      break;                                              // __NO_COVERAGE__
    }
  }

  if (state == S_KW) {
    R_THROW_ERROR("DESCRIPTION file ended while parsing a key");
  } else if (state != S_BG) {
    /* Strip the trailing newline(s) */
    while (p - 1 > start && *(p-1) == '\n') p--;
    vlsize = p - vl;
    SET_STRING_ELT(result, ridx, Rf_mkCharLenCE(vl, vlsize, CE_BYTES));
    SET_STRING_ELT(names, ridx, Rf_mkCharLenCE(kw, kwsize, CE_NATIVE));
    ridx++;
  }

  Rf_setAttrib(result, R_NamesSymbol, names);
  SEXP final = PROTECT(Rf_lengthgets(result, ridx));

  UNPROTECT(3);
  return final;
}

/* --------------------------------------------------------------------- */

SEXP pkgcache_parse_description(SEXP path) {
  SEXP raw = PROTECT(pkgcache__read_file_raw(CHAR(STRING_ELT(path, 0))));
  if (TYPEOF(raw) != RAWSXP) {
    R_THROW_ERROR(CHAR(STRING_ELT(raw, 0)));
  }

  SEXP desc = PROTECT(pkgcache_parse_description_raw(raw));

  UNPROTECT(2);
  return desc;
}

/* --------------------------------------------------------------------- */

SEXP pkgcache_parse_packages_raw(SEXP raw) {
  int len = LENGTH(raw);
  char *p = NULL;
  int npkgs = 1;

  if (len == 0) return R_NilValue;

  /* ------------------------------------------------------------------- */
  /* Count number of empty lines, to guess the number of packages */
  p = (char*) RAW(raw);
  char tail = p[len - 1];
  p[len - 1] = '\0';

  /* Skip whitespace first, check for empty file */

  while (*p == '\n' || *p == '\r') p++;
  if (*p == '\0') return R_NilValue;

  /* This is faster than manual search, because strchr is optimized.
     It is also faster than strstr, for this special case of a two
     character pattern. */

  for (;;) {
    p = strchr(p, '\n');
    if (p == NULL) break;
    p++;
    if (*p == '\n' || *p == '\r') {
      p++;
      npkgs++;
      while (*p == '\n' || *p == '\r') p++;
      if (*p == '\0') npkgs--;
    }
  }

  /* ------------------------------------------------------------------- */

  int state = S_BG;
  char *kw = NULL, *vl = NULL;
  int kwsize = 0, vlsize = 0;
  int linum = 1;
  int max_cols = 1000;

  SEXP nms = PROTECT(allocVector(STRSXP, max_cols));
  SEXP cols = PROTECT(allocVector(VECSXP, max_cols));
  SEXP tab = PROTECT(allocVector(INTSXP, HASH_SIZE * MAX_COLL));
  struct hash_table table;
  hash_create(&table, nms, cols, tab, max_cols, npkgs);
  int npkg = 0;

  p = (char*) RAW(raw);
  while (*p != '\0') {
    switch (state) {

    /* -- at the begining of a package --------------------------------- */
    case S_BG:
      if (*p == '\r') {
        p++;
      } else if (*p == '\n') {
        linum++;
        p++;
      } else if (*p == ':' || *p == ' ' || *p == '\t') {
        R_THROW_ERROR(
          "Invalid PACKAGES file in line %d: expected key",
          linum
        );
      } else {
        kw = p++;
        state = S_KW;
      }
      break;

    /* -- within a keyword --------------------------------------------- */
    case S_KW:
      if (*p == ':') {
        kwsize = p - kw;
        p++;
        vl = p;
        if (*vl == ' ') vl++;   /* skip leading space */
        state = S_VL;

      } else if (*p == '\n') {
        R_THROW_ERROR(
          "Invalid line %d in PACKAGES file: must contain `:`",
          linum
        );

      } else {
        p++;
      }

      break;

    /* --- within a value ---------------------------------------------- */
    case S_VL:
      /* newline might be the end of the value, if no continuation. */
      if (*p == '\n') {
        state = S_NL;
        vlsize = p - vl;
        p++;
        linum++;

      } else {
        p++;
      }
      break;

    /* -- right after a newline ---------------------------------------- */
    case S_NL:

      /* maybe a continuation line */
      if (*p == ' ' || *p == '\t') {
        state = S_WS;
        p++;

      /* end of field */
      } else {
        /* Save field */
        SEXP val = PROTECT(mkCharLenCE(vl, vlsize, CE_BYTES));
        hash_update(&table, kw, kwsize, npkg, val, /* err */ 1);
        UNPROTECT(1);

        /* end of package? */
	/* maybe it ends with \r\n but we put a \0 over the \n */
        if (*p == '\n' || *p == '\r') {
          p++;
          npkg++;
          linum++;
          state = S_BG;

        } else if (*p == '\r' && *(p+1) == '\n') {
          p++;
          p++;
          npkg++;
          linum++;
          state = S_BG;

        /* or just a new key */
        } else {
          kw = p;
          p++;
          state = S_KW;
        }
      }

      break;

    /* -- after continuation space ------------------------------------- */
    case S_WS:
      /* more whitespace? */
      if (*p == ' ' || *p == '\t') {
        p++;

      /* otherwise continuation line, so this is still the value */
      } else {
        state = S_VL;
        p++;
      }

      break;

    /* ----------------------------------------------------------------- */
    default:
      R_THROW_ERROR("Internal PACKAGES parser error");  // __NO_COVERAGE__
      break;                                            // __NO_COVERAGE__
    }
  }

  vlsize = p - vl;
  p = (char*) RAW(raw);
  p[len - 1] = tail;
  if (state == S_VL && tail != '\n') vlsize++;

  if (state == S_KW) {
    R_THROW_ERROR("PACKAGES file ended while parsing a key");
  } else if (state != S_BG) {
    /* Save field */
    SEXP val = PROTECT(mkCharLenCE(vl, vlsize, CE_BYTES));
    hash_update(&table, kw, kwsize, npkg, val, /* err= */ 1);
    UNPROTECT(1);
  }

  /* ------------------------------------------------------------------- */

  Rf_setAttrib(cols, R_NamesSymbol, nms);
  SEXP final = PROTECT(Rf_lengthgets(cols, table.nfld));
  UNPROTECT(4);
  return final;
}

/* --------------------------------------------------------------------- */

SEXP pkgcache_parse_descriptions(SEXP paths, SEXP lowercase) {
  int npkg, npkgs = LENGTH(paths);
  int clowercase = LOGICAL(lowercase)[0];

  int state = S_BG;
  char *kw = NULL, *vl = NULL;
  int kwsize = 0, vlsize = 0;
  int linum = 1;
  int haserrors = 0;

  int max_cols = 1000;

  SEXP errors = PROTECT(allocVector(STRSXP, npkgs));
  SEXP nms = PROTECT(allocVector(STRSXP, max_cols));
  SEXP cols = PROTECT(allocVector(VECSXP, max_cols));
  SEXP tab = PROTECT(allocVector(INTSXP, HASH_SIZE * MAX_COLL));
  struct hash_table table;
  hash_create(&table, nms, cols, tab, max_cols, npkgs);

  for (npkg = 0; npkg < npkgs; npkg++) {

    const char *cpath = CHAR(STRING_ELT(paths, npkg));
    SEXP raw = PROTECT(pkgcache__read_file_raw(cpath));
    if (TYPEOF(raw) != RAWSXP) {
      SET_STRING_ELT(errors, npkg, STR1(raw));
      UNPROTECT(1);
      goto failedpkg;
    }

    state = S_BG;
    kw = NULL;
    vl = NULL;
    kwsize = 0;
    vlsize = 0;
    linum = 1;

    int len = LENGTH(raw);
    char *p = (char*) RAW(raw);
    char tail = p[len - 1];
    p[len - 1] = '\0';

    while (*p != '\0') {
      switch(state) {
      /* -- at the begining -------------------------------------------- */
      case S_BG:
        if (*p == ':' || *p == '\r' || *p == '\n' || *p == ' ' || *p == '\t') {
          SET_STRING_ELT(
            errors,
            npkg,
            STR1(R_FORMAT_ERROR(
              "`%s` is invalid, must start with an alphanumeric character",
              cpath
            ))
          );
          UNPROTECT(1);
          goto failedpkg;
        }
        /* Otherwise it must be the start of a keyword */
        if (clowercase) *p = tolower(*p);
        kw = p++;
        state = S_KW;

        break;

      /* -- within a keyword ------------------------------------------- */
      case S_KW:
        /* Maybe the keyword ends here, and a value starts */
        if (*p == ':') {
          kwsize = p - kw;
          p++;
          vl = p;
          if (*vl == ' ') vl++;
          state = S_VL;

        /* A newline within a keyword is an error */
        } else if (*p == '\n') {
          SET_STRING_ELT(
            errors,
            npkg,
            STR1(R_FORMAT_ERROR(
              "Line %d is invalid in `%s`: must contain `:`",
              linum,
              cpath
            ))
          );
          UNPROTECT(1);
          goto failedpkg;

        /* Otherwise we are inside the keyword */
        } else {
          if (clowercase) *p = tolower(*p);
          p++;
        }

        break;

      /* --- within a value -------------------------------------------- */
      case S_VL:
        if (*p == '\n') {
          state = S_NL;
          vlsize = p - vl;
          p++;
          linum++;

        } else {
          p++;
        }

        break;

      /* -- right after a newline -------------------------------------- */
      case S_NL:
        /* maybe a continuation line */
        if (*p == ' ' || *p == '\t') {
          state = S_WS;
          p++;

        /* othewise we can save the field, and start parsing the next one */
        } else {
          SEXP val = PROTECT(mkCharLenCE(vl, vlsize, CE_BYTES));
          hash_update(&table, kw, kwsize, npkg, val, 1);
          UNPROTECT(1);

          kw = p;
          state = S_KW;
          if (clowercase) *p = tolower(*p);
          p++;
        }

        break;

      /* -- after continuation space ----------------------------------- */
      case S_WS:
        /* more whitespace? */
        if (*p == ' ' || *p == '\t') {
          p++;

        /* otherwise continuation line, so this is still the value */
        } else {
          state = S_VL;
          p++;
        }

        break;

      /* --------------------------------------------------------------- */
      default:
        R_THROW_ERROR("Internal DESCRIPTION parser error");  // __NO_COVERAGE__
        break;                                               // __NO_COVERAGE__
      }
    }

    vlsize = p - vl;
    p = (char*) RAW(raw);
    p[len - 1] = tail;
    if (state == S_VL && tail != '\n') vlsize++;

    if (state == S_KW) {
      SET_STRING_ELT(
        errors,
        npkg,
        STR1(R_FORMAT_ERROR(
          "`%s` ended while parsing a key",
          cpath
        ))
      );
      UNPROTECT(1);
      goto failedpkg;

    } else {
      /* Save field */
      SEXP val = PROTECT(mkCharLenCE(vl, vlsize, CE_BYTES));
      hash_update(&table, kw, kwsize, npkg, val, /* err = */ 1);
      UNPROTECT(1);
    }

    UNPROTECT(1);
    continue;

  failedpkg:
    haserrors = 1;
  }

  Rf_setAttrib(cols, R_NamesSymbol, nms);
  SEXP final = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(final, 0, Rf_lengthgets(cols, table.nfld));
  SET_VECTOR_ELT(final, 1, errors);
  SET_VECTOR_ELT(final, 2, ScalarLogical(haserrors));

  UNPROTECT(5);
  return final;
}

SEXP pkgcache_graphics_api_version(void) {
    return ScalarInteger(R_GE_getVersion());
}
