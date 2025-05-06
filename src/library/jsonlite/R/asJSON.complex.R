setMethod("asJSON", "complex", function(x, digits = 5, collapse = TRUE, complex = c("string", "list"), na = c("string", "null", "NA"), oldna = NULL, ...) {
  # validate
  na <- match.arg(na)
  complex <- match.arg(complex)

  #turn into strings
  if (complex == "string") {
    #default NA is "NA"
    mystring <- prettyNum(x = x, digits = digits)
    if (any(missings <- which(!is.finite(x)))) {
      if (na %in% c("null", "NA")) {
        mystring[missings] <- NA_character_
      }
    }
    asJSON(mystring, collapse = collapse, na = na, ...)
  } else {
    if (na == "NA") {
      na <- oldna
    }
    asJSON(list(real = Re(x), imaginary = Im(x)), na = na, digits = digits, ...)
  }
})
