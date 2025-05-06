cleannames <- function(objnames, no_dots = FALSE) {
  objnames[objnames == ""] <- NA_character_
  is_missing <- is.na(objnames)
  objnames[is_missing] <- as.character(seq_len(length(objnames)))[is_missing]
  if (isTRUE(no_dots)) objnames <- gsub(".", "_", objnames, fixed = TRUE)
  make.unique(objnames)
}
