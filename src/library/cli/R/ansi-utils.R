re_table <- function(...) {
  lapply(gregexpr(...), function(x) {
    res <- cbind(
      start = x,
      end = x + attr(x, "match.length") - 1,
      length = attr(x, "match.length")
    )
    res <- res[res[, "start"] != -1, , drop = FALSE]
  })
}

## Create the non-matching table from the matching table

non_matching <- function(table, str, empty = FALSE) {
  mapply(table, str, SIMPLIFY = FALSE, FUN = function(t, s) {
    if (!nrow(t)) {
      cbind(start = 1, end = base::nchar(s), length = base::nchar(s))
    } else {
      start <- c(1, t[, "end"] + 1)
      end <- c(t[, "start"] - 1, base::nchar(s))
      res <- cbind(start = start, end = end, length = end - start + 1)
      if (!empty) res[res[, "length"] != 0, , drop = FALSE] else res
    }
  })
}

myseq <- function(from, to, by = 1) {
  stopifnot(by != 0)
  if (by > 0) {
    if (to < from) {
      integer()
    } else {
      seq(from, to, by = by)
    }
  } else {
    if (to > from) {
      integer()
    } else {
      seq(from, to, by = by)
    }
  }
}

`%:%` <- myseq
