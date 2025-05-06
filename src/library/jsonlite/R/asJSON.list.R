setMethod("asJSON", "list", function(x, collapse = TRUE, na = NULL, oldna = NULL, is_df = FALSE, auto_unbox = FALSE, indent = NA_integer_, no_dots = FALSE, ...) {
  # reset na arg when called from data frame
  if (identical(na, "NA")) {
    na <- oldna
  }

  # coerse pairlist if needed
  if (is.pairlist(x)) {
    x <- as.vector(x, mode = "list")
  }

  # empty vector
  #if (!length(x)) {
  #  if(collapse) {
  #    return(if (is.null(names(x))) "[]" else "{}")
  #  } else {
  #    return(character())
  #  }
  #}

  # this condition appears when a dataframe contains a column with lists we need to
  # do this, because the [ operator always returns a list of length 1
  # if (length(x) == 1 && is.null(names(x)) && collapse == FALSE) {
  #   return(asJSON(x[[1]], ...))
  # }

  # note we are NOT passing on the container argument.
  tmp <- if (is_df && auto_unbox) {
    vapply(
      x,
      function(y, ...) {
        asJSON(y, auto_unbox = is.list(y), ...)
      },
      character(1),
      na = na,
      indent = indent_increment(indent),
      no_dots = no_dots,
      ...
    )
  } else {
    vapply(x, asJSON, character(1), na = na, auto_unbox = auto_unbox, indent = indent_increment(indent), no_dots = no_dots, ...)
  }

  if (!is.null(names(x))) {
    if (!collapse) {
      #this should never happen
      warning("collapse=FALSE called for named list.")
    }
    #in case of named list:
    objnames <- deparse_vector(cleannames(names(x), no_dots = no_dots))
    collapse_object(objnames, tmp, indent)
  } else {
    #in case of unnamed list:
    if (collapse) {
      collapse(tmp, inner = FALSE, indent)
    } else {
      tmp
    }
  }
})
