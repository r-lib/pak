encapsulate({
  # Given two named vectors, join them together, and keep only the last element
  # with a given name in the resulting vector. If b has any elements with the
  # same name as elements in a, the element in a is dropped. Also, if there are
  # any duplicated names in a or b, only the last one with that name is kept.
  merge_vectors <- function(a, b) {
    if ((!is.null(a) && length(a) > 1 && is.null(names(a))) ||
        (!is.null(b) && length(b) > 1 && is.null(names(b)))) {
      stop("merge_vectors: vectors must be either NULL or named vectors")
    }

    x <- c(a, b)
    drop_idx <- duplicated(names(x), fromLast = TRUE)
    x[!drop_idx]
  }

  # Check that all elements of a list are named.
  # NULL and empty lists return TRUE.
  all_named <- function(x) {
    if (length(names(x)) != length(x) || any(names(x) == "")) {
      return(FALSE)
    }
    TRUE
  }

  # Return all the functions in a list.
  get_functions <- function(x) {
    funcs <- vapply(x, is.function, logical(1))
    if (all(!funcs)) return(NULL)
    x[funcs]
  }

  # Return all the non-functions in a list.
  get_nonfunctions <- function(x) {
    funcs <- vapply(x, is.function, logical(1))
    if (all(funcs)) return(NULL)
    x[!funcs]
  }
})
