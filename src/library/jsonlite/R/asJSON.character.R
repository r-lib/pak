setMethod("asJSON", "character", function(x, collapse = TRUE, na = c("null", "string", "NA"),
  auto_unbox = FALSE, keep_vec_names = FALSE, indent = NA_integer_, ...) {

  # Needed for multi-byte Windows locales
  # See: https://github.com/jeroen/jsonlite/issues/329
  x <- enc2utf8(x)

  # shiny legacy exception
  if(isTRUE(keep_vec_names) && length(names(x))){
    warn_keep_vec_names()
    return(asJSON(as.list(x), na = na, auto_unbox = TRUE, collapse = collapse, ...))
  }

  # vectorized escaping
  tmp <- deparse_vector(x)

  # this was used with deparse_vector_old
  #if(identical(Encoding(x), "UTF-8")){
  #  if(!grepl("UTF", Sys.getlocale("LC_CTYPE"), ignore.case=TRUE)){
  #    tmp <- utf8conv(tmp);
  #  }
  #}

  # validate NA
  if (any(missings <- which(is.na(x)))) {
    na <- match.arg(na)
    if (na %in% c("null")) {
      tmp[missings] <- "null"
    } else if(na %in% "string") {
      tmp[missings] <- "\"NA\""
    } else {
      tmp[missings] <- NA_character_
    }
  }

  if(isTRUE(auto_unbox) && length(tmp) == 1){
    return(tmp);
  }

  # this is almost always true, except for class 'scalar'
  if (isTRUE(collapse)) {
    collapse(tmp, indent = indent)
  } else {
    tmp
  }
})
