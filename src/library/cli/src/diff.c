/* diff - compute a shortest edit script (SES) given two sequences
 * Copyright (c) 2004 Michael B. Allen <mba2000 ioplex.com>
 *
 * The MIT License
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

/* This algorithm is basically Myers' solution to SES/LCS with
 * the Hirschberg linear space refinement as described in the
 * following publication:
 *
 *   E. Myers, ``An O(ND) Difference Algorithm and Its Variations,''
 *   Algorithmica 1, 2 (1986), 251-266.
 *   http://www.cs.arizona.edu/people/gene/PAPERS/diff.ps
 *
 * This is the same algorithm used by GNU diff(1).
 */

#include <stdlib.h>
#include <limits.h>
#include <errno.h>
#include <stddef.h>
#include <R_ext/Utils.h>

#include "cli.h"
#include "errors.h"

typedef enum {
  DIFF_MATCH = 1,
  DIFF_DELETE,
  DIFF_INSERT
} diff_op;

struct diff_edit {
  short op;
  int off; /* off into s1 if MATCH or DELETE but s2 if INSERT */
  int len;
};

typedef int (*cmp_fn)(int a, int b, void *context);

static int _diff(const void *a, int aoff, int n,
                 const void *b, int boff, int m,
                 cmp_fn cmp, void *context, int dmax,
                 struct diff_edit *ses, int *sn,
                 int *buf, int bufsize);

struct _chr_data {
  const SEXP* aptr;
  const SEXP* bptr;
};

static int _cmp_chr(int a, int b, void *context) {
  struct _chr_data *data = (struct _chr_data*) context;
  return data->aptr[a] != data->bptr[b];
}

SEXP clic_diff_chr(SEXP old, SEXP new, SEXP max) {
  int l_old = Rf_length(old);
  int l_new = Rf_length(new);
  int dmax = INTEGER(max)[0];
  int snmax = l_old + l_new + 1; // upper bound is sum of lengths
  int bufsize = snmax;

  struct diff_edit *ses =
    (struct diff_edit*) S_alloc(snmax, sizeof(struct diff_edit));
  int *buf = (int*) S_alloc(bufsize, sizeof(int));
  int sn;

  struct _chr_data data;
  data.aptr = STRING_PTR_RO(old);
  data.bptr = STRING_PTR_RO(new);

  int out = _diff(old, 0, l_old, new, 0, l_new, _cmp_chr, &data,
                  dmax, ses, &sn, buf, bufsize);

  /* AFAICT we'll never error like this, but it does not hurt... */
  if (out < 0) {                // __NO_COVERAGE__
    R_THROW_ERROR(                                        // __NO_COVERAGE__
      "Could not calculate diff, internal error: %d, %d", // __NO_COVERAGE__
      out,                                                // __NO_COVERAGE__
      errno                                               // __NO_COVERAGE__
    );                                                    // __NO_COVERAGE__
  }                                                       // __NO_COVERAGE__

  SEXP result = PROTECT(allocVector(VECSXP, 4));
  SET_VECTOR_ELT(result, 0, allocVector(INTSXP, sn));
  SET_VECTOR_ELT(result, 1, allocVector(INTSXP, sn));
  SET_VECTOR_ELT(result, 2, allocVector(INTSXP, sn));
  SET_VECTOR_ELT(result, 3, ScalarInteger(out));

  int i;
  int *op_ptr  = INTEGER(VECTOR_ELT(result, 0));
  int *off_ptr = INTEGER(VECTOR_ELT(result, 1));
  int *len_ptr = INTEGER(VECTOR_ELT(result, 2));

  for (i = 0; i < sn; i++, op_ptr++, off_ptr++, len_ptr++) {
    struct diff_edit *e = ses + i;
    *op_ptr = e->op;
    *off_ptr = e->off;
    *len_ptr = e->len;
  }

  UNPROTECT(1);
  return result;
}

#define FV(k) _v(ctx, (k), 0)
#define RV(k) _v(ctx, (k), 1)

struct _ctx {
  cmp_fn cmp;
  void *context;
  int *buf;
  int bufsize;
  struct diff_edit *ses;
  int si;
  int dmax;
};

struct middle_snake {
  int x, y, u, v;
};

static void _setv(struct _ctx *ctx, int k, int r, int val) {
  /* Pack -N to N into 0 to N * 2 */
  int j = k <= 0 ? -k * 4 + r : k * 4 + (r - 2);

  if (j >= ctx->bufsize) {
    int newsize = j + 1;
    ctx->buf = (int*) S_realloc((char*) ctx->buf, newsize, ctx->bufsize,
                                sizeof(int));
    ctx->bufsize = newsize;
  }

  ctx->buf[j] = val;
}

static int _v(struct _ctx *ctx, int k, int r) {
  int j = k <= 0 ? -k * 4 + r : k * 4 + (r - 2);

  if (j > ctx->bufsize) {
    int newsize = j + 1;
    ctx->buf = (int*) S_realloc((char*) ctx->buf, newsize, ctx->bufsize,
                                sizeof(int));
    ctx->bufsize = newsize;
  }

  return ctx->buf[j];
}

static int _find_middle_snake(const void *a, int aoff, int n,
                              const void *b, int boff, int m,
                              struct _ctx *ctx,
                              struct middle_snake *ms) {
  int delta, odd, mid, d;

  delta = n - m;
  odd = delta & 1;
  mid = (n + m) / 2;
  mid += odd;

  _setv(ctx, 1, 0, 0);
  _setv(ctx, delta - 1, 1, n);

  for (d = 0; d <= mid; d++) {
    int k, x, y;

    R_CheckUserInterrupt();

    if ((2 * d - 1) >= ctx->dmax) {
      return ctx->dmax;
    }

    for (k = d; k >= -d; k -= 2) {
      if (k == -d || (k != d && FV(k - 1) < FV(k + 1))) {
        x = FV(k + 1);
      } else {
        x = FV(k - 1) + 1;
      }
      y = x - k;

      ms->x = x;
      ms->y = y;
      while (x < n && y < m && ctx->cmp(aoff + x, boff + y, ctx->context) == 0) {
        x++; y++;
      }
      _setv(ctx, k, 0, x);

      if (odd && k >= (delta - (d - 1)) && k <= (delta + (d - 1))) {
        if (x >= RV(k)) {
          ms->u = x;
          ms->v = y;
          return 2 * d - 1;
        }
      }
    }
    for (k = d; k >= -d; k -= 2) {
      int kr = (n - m) + k;

      if (k == d || (k != -d && RV(kr - 1) < RV(kr + 1))) {
        x = RV(kr - 1);
      } else {
        x = RV(kr + 1) - 1;
      }
      y = x - kr;

      ms->u = x;
      ms->v = y;
      while (x > 0 && y > 0 && ctx->cmp(aoff + (x - 1), boff + (y - 1), ctx->context) == 0) {
        x--; y--;
      }
      _setv(ctx, kr, 1, x);

      if (!odd && kr >= -d && kr <= d) {
        if (x <= FV(kr)) {
          ms->x = x;
          ms->y = y;
          return 2 * d;
        }
      }
    }
  }

  errno = EFAULT; // __NO_COVERAGE__

  return -1;      // __NO_COVERAGE__
}

static void _edit(struct _ctx *ctx, diff_op op, int off, int len) {
  struct diff_edit *e;

  if (len == 0 || ctx->ses == NULL) {
    return;
  }               /* Add an edit to the SES (or
                   * coalesce if the op is the same)
                   */
  e = &ctx->ses[ctx->si];
  if (e->op != op) {
    if (e->op) {
      ctx->si++;
      e = &ctx->ses[ctx->si];
    }
    e->op = op;
    e->off = off;
    e->len = len;
  } else {
    e->len += len;
  }
}

static int _ses(const void *a, int aoff, int n,
                const void *b, int boff, int m,
                struct _ctx *ctx) {
  struct middle_snake ms;
  int d;

  if (n == 0) {
    _edit(ctx, DIFF_INSERT, boff, m);
    d = m;
  } else if (m == 0) {
    _edit(ctx, DIFF_DELETE, aoff, n);
    d = n;
  } else {
    /* Find the middle "snake" around which we
     * recursively solve the sub-problems.
     */
    d = _find_middle_snake(a, aoff, n, b, boff, m, ctx, &ms);
    if (d == -1) {
      return -1;
    } else if (d >= ctx->dmax) {
      return ctx->dmax;
    } else if (ctx->ses == NULL) {
      return d;
    } else if (d > 1) {
      if (_ses(a, aoff, ms.x, b, boff, ms.y, ctx) == -1) {
        return -1;
      }

      _edit(ctx, DIFF_MATCH, aoff + ms.x, ms.u - ms.x);

      aoff += ms.u;
      boff += ms.v;
      n -= ms.u;
      m -= ms.v;
      if (_ses(a, aoff, n, b, boff, m, ctx) == -1) {
        return -1;
      }
    } else {
      int x = ms.x;
      int u = ms.u;

      /* There are only 4 base cases when the
       * edit distance is 1.
       *
       * n > m   m > n
       *
       *   -       |
       *    \       \    x != u
       *     \       \
       *
       *   \       \
       *    \       \    x == u
       *     -       |
       */

      if (m > n) {
        if (x == u) {
          _edit(ctx, DIFF_MATCH, aoff, n);
          _edit(ctx, DIFF_INSERT, boff + (m - 1), 1);
        } else {
          _edit(ctx, DIFF_INSERT, boff, 1);
          _edit(ctx, DIFF_MATCH, aoff, n);
        }
      } else {
        if (x == u) {
          _edit(ctx, DIFF_MATCH, aoff, m);
          _edit(ctx, DIFF_DELETE, aoff + (n - 1), 1);
        } else {
          _edit(ctx, DIFF_DELETE, aoff, 1);
          _edit(ctx, DIFF_MATCH, aoff + 1, m);
        }
      }
    }
  }

  return d;
}

static int _diff(const void *a, int aoff, int n,
                 const void *b, int boff, int m,
                 cmp_fn cmp, void *context, int dmax,
                 struct diff_edit *ses, int *sn,
                 int *buf, int bufsize) {
  struct _ctx ctx;
  int d, x, y;
  struct diff_edit *e = NULL;

  ctx.cmp = cmp;
  ctx.context = context;
  ctx.buf = buf;
  ctx.bufsize = bufsize;
  ctx.ses = ses;
  ctx.si = 0;
  ctx.dmax = dmax ? dmax : INT_MAX;

  if (ses && sn) {
    if ((e = ses) == NULL) {
      return -1;  // __NO_COVERAGE__
    }
    e->op = 0;
  }

  /* The _ses function assumes the SES will begin or end with a delete
   * or insert. The following will ensure this is true by eating any
   * beginning matches. This is also a quick way to process sequences
   * that match entirely.
   */
  x = y = 0;
  while (x < n && y < m && cmp(aoff + x, boff + y, context) == 0) {
    x++; y++;
  }
  _edit(&ctx, DIFF_MATCH, aoff, x);

  if ((d = _ses(a, aoff + x, n - x, b, boff + y, m - y, &ctx)) == -1) {
    return -1;
  }
  if (ses && sn) {
    *sn = e->op ? ctx.si + 1 : 0;
  }


  return d;
}
