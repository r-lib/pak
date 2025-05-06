setMethod("asJSON", "logical", function(x, collapse = TRUE, na = c("null", "string", "NA"), auto_unbox = FALSE, keep_vec_names = FALSE, indent = NA_integer_, ...) {
  # shiny legacy exception
  if (isTRUE(keep_vec_names) && length(names(x))) {
    warn_keep_vec_names()
    return(asJSON(as.list(x), collapse = collapse, na = na, auto_unbox = TRUE, ...))
  }

  # validate arg
  na <- match.arg(na)

  # json true/false
  tmp <- ifelse(x, "true", "false")

  # replace missing values, unless na="NA"
  if (!identical(na, "NA")) {
    # logical values can have NA (but not Inf/NaN). Default is to encode as null.
    if (any(missings <- which(is.na(x)))) {
      tmp[missings] <- ifelse(identical(na, "string"), "\"NA\"", "null")
    }
  }

  #this is needed when !length(tmp) or all(is.na(tmp))
  if (!is.character(tmp)) {
    tmp <- as.character(tmp)
  }

  if (isTRUE(auto_unbox) && length(tmp) == 1) {
    return(tmp)
  }

  # collapse it
  if (collapse) {
    collapse(tmp, indent = indent)
  } else {
    tmp
  }
})
