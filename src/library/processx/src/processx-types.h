
#ifndef PROCESSX_TYPES_H
#define PROCESSX_TYPES_H

#ifdef _WIN32
#include <windows.h>
typedef DWORD pid_t;
#else
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#endif

typedef struct {
  pid_t *stor_begin;
  pid_t *stor_end;
  pid_t *end;
} processx_vector_t;

#define VECTOR(v) ((v).stor_begin)

void processx_vector_init(processx_vector_t *v, size_t size, size_t alloc_size);
size_t processx_vector_size(const processx_vector_t *v);
void processx_vector_reserve(processx_vector_t *v, size_t size);
void processx_vector_clear(processx_vector_t *v);
void processx_vector_push_back(processx_vector_t *v, pid_t e);
int processx_vector_find(const processx_vector_t *v, pid_t e, size_t from, size_t *idx);
void processx_vector_rooted_tree(pid_t root, const processx_vector_t *nodes,
				 const processx_vector_t *parents,
				 processx_vector_t *result);

#endif
