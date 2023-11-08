
#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#ifdef _WIN32
#include <windows.h>
#endif

SEXP filelock_lock(SEXP path, SEXP exclusive, SEXP timeout);
SEXP filelock_unlock(SEXP path);
SEXP filelock_is_unlocked(SEXP lock);

/* ------------------------------------------------------------------ */
/* INTERNALS */

typedef struct filelock__list_s {
  char *path;
  int refcount;
  int exclusive;
#ifdef _WIN32
  HANDLE file;
#else
  int file;
#endif
  struct filelock__list_s *next;
} filelock__list_t;


#ifdef _WIN32
SEXP filelock__list_add(const char *path, HANDLE file, int exclusive);
#else
SEXP filelock__list_add(const char *path, int file, int exclusive);
#endif

SEXP filelock__make_lock_handle(filelock__list_t *node);
void filelock__list_remove(const char *path);
filelock__list_t *filelock__list_find(const char *path);
void filelock__finalizer(SEXP x);
