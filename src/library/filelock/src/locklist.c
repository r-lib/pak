
#include <stdlib.h>
#include <string.h>

#include "filelock.h"

filelock__list_t lock_list_head = { 0, 0 };
filelock__list_t *lock_list = &lock_list_head;

SEXP filelock__make_lock_handle(filelock__list_t *node) {
  SEXP ptr, result, path;
  ptr = PROTECT(R_MakeExternalPtr(node, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(ptr, filelock__finalizer, 0);

  path = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(path, 0, mkCharCE(node->path, CE_UTF8));
  result = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, ptr);
  SET_VECTOR_ELT(result, 1, path);

  UNPROTECT(3);
  node->refcount += 1;

  return result;
}


#ifdef _WIN32
SEXP filelock__list_add(const char *path, HANDLE file, int exclusive) {
#else
SEXP filelock__list_add(const char *path, int file, int exclusive) {
#endif
  filelock__list_t *node;
  node = calloc(1, sizeof(filelock__list_t));
  if (!node) error("Out of memory");
  node->path = strdup(path);
  node->file = file;
  node->exclusive = exclusive;
  node->refcount = 0;
  if (!node->path) { free(node); error("Out of memory"); }
  node->next = lock_list->next;
  lock_list->next = node;

  return filelock__make_lock_handle(node);
}

void filelock__list_remove(const char *path) {
  filelock__list_t *prev = lock_list, *ptr = lock_list->next;
  while (ptr) {
    if (!strcmp(ptr->path, path)) {
      prev->next = ptr->next;
      free(ptr->path);
      free(ptr);
      return;
    }
    prev = ptr;
    ptr = ptr->next;
  }
}

filelock__list_t *filelock__list_find(const char *path) {
  filelock__list_t *ptr = lock_list->next;
  while (ptr) {
    if (!strcmp(ptr->path, path)) {
      /* TODO: check that the lock is valid, because it might have been
	 unlocked through another lock object. */
      return ptr;
    }
    ptr = ptr->next;
  }
  return 0;
}
