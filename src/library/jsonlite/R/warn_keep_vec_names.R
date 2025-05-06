warn_keep_vec_names <- function() {
  message("Input to asJSON(keep_vec_names=TRUE) is a named vector. ", "In a future version of jsonlite, this option will not be supported, ", "and named vectors will be translated into arrays instead of objects. ", "If you want JSON object output, please use a named list instead. See ?toJSON.")
}
