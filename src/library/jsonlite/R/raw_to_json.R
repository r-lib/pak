# This function deals with some uncertainty in character encoding when reading
# from files and URLs. It tries UTF8 first, but falls back on native if it is
# certainly not UTF8.
raw_to_json <- function(x) {
  txt <- rawToChar(x)
  Encoding(txt) <- "UTF-8"
  isvalid <- validate(txt)
  if (!isvalid && grepl("invalid bytes in UTF8", attr(isvalid, "err"), fixed = TRUE, useBytes = TRUE)) {
    warning("The json string is not valid UTF-8. Assuming native encoding.", call. = FALSE)
    Encoding(txt) <- ""
  }
  return(txt)
}
