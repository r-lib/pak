
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <R.h>
#include "processx-types.h"
#include "errors.h"

void processx_vector_init(processx_vector_t *v, size_t size, size_t alloc_size) {
  if (alloc_size < size) alloc_size = size;
  if (alloc_size == 0) alloc_size = 1;
  v->stor_begin = (pid_t*) R_alloc(alloc_size, sizeof(pid_t));
  if (v->stor_begin == 0) {
    R_THROW_ERROR("cannot allocate processx vector, out of memory");
  }
  v->stor_end = v->stor_begin + alloc_size;
  v->end      = v->stor_begin + size;
}

size_t processx_vector_size(const processx_vector_t *v) {
  return v->end - v->stor_begin;
}

void processx_vector_reserve(processx_vector_t *v, size_t size) {
  size_t actual_size = processx_vector_size(v);
  size_t alloc_size = v->stor_end - v->stor_begin;
  pid_t *tmp;
  if (size <= actual_size) return;

  tmp = (pid_t*) S_realloc( (char*) v->stor_begin, size, alloc_size, sizeof(pid_t));
  v->stor_begin = tmp;
  v->stor_end   = v->stor_begin + size;
  v->end        = v->stor_begin + actual_size;
}

void processx_vector_clear(processx_vector_t *v) {
  v->end = v->stor_begin;
}

void processx_vector_push_back(processx_vector_t *v, pid_t e) {
  /* full, allocate more storage */
  if (v->stor_end == v->end) {
    long int new_size = processx_vector_size(v) * 2;
    if (new_size == 0) { new_size = 1; }
    processx_vector_reserve(v, new_size);
  }

  *(v->end) = e;
  v->end += 1;
}

/**
 * Find an element in a vector
 *
 * @param v The vector.
 * @param e The element to find.
 * @param from Start the search from this position.
 * @param idx If not a NULL pointer, then it is set to the index of the first
     occurence of `e`, if found. Otherwise not touched.
 * @return Non-zero if `e` is found, zero otherwise.
 */

int processx_vector_find(const processx_vector_t *v, pid_t e, size_t from, size_t *idx) {
  size_t size = processx_vector_size(v);

  while (from < size) {
    if (VECTOR(*v)[from] == e) {
      if (idx) *idx = from;
      return 1;
    }
    from++;
  }

  return 0;
}

/**
 * Find a rooted tree within forest
 *
 * @param root The id of the root node.
 * @param nodes The ids of all nodes.
 * @param parents The ids of the parent nodes for each node. The length must
 *   match `nodes`.
 * @param result The result is stored here. `root` is included here as well, as the first
 *   (zeroth) element.
 */

void processx_vector_rooted_tree(pid_t root, const processx_vector_t *nodes,
				 const processx_vector_t *parents,
				 processx_vector_t *result) {

  size_t len = processx_vector_size(nodes);
  size_t done = 0, next_done = 1;

  processx_vector_clear(result);
  processx_vector_push_back(result, root);

  while (done < next_done) {
    size_t i;
    for (i = 0; i < len; i++) {
      pid_t parent = VECTOR(*parents)[i];
      if (processx_vector_find(result, parent, done, 0)) {
	processx_vector_push_back(result, VECTOR(*nodes)[i]);
      }
    }
    done = next_done;
    next_done = processx_vector_size(result);
  }
}
