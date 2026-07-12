#' POST files or data
#'
#' Build multipart form data elements. The `form_file` function uploads a
#' file. The `form_data` function allows for posting a string or raw vector
#' with a custom content-type.
#'
#' @param path a string with a path to an existing file on disk
#' @param type MIME content-type of the file.
#' @param name a string with the file name to use for the upload
#' @export
#' @name multipart
#' @rdname multipart
form_file <- function(path, type = NULL, name = NULL) {
  path <- enc2native(normalizePath(path[1], mustWork = TRUE))
  if(!is.null(type)){
    stopifnot(is.character(type))
  }
  structure(list(path = path, type = type, name = name), class = "form_file")
}

#' @export
#' @name multipart
#' @rdname multipart
#' @param value a character or raw vector to post
form_data <- function(value, type = NULL){
  if(is.character(value))
    value <- charToRaw(paste(enc2utf8(value), collapse = "\n"))
  if(!is.raw(value))
    stop("Argument 'value' must be string or raw vector")
  structure(list(value = value, type = type), class = "form_data")
}

#' @export
print.form_file <- function(x, ...){
  txt <- paste("Form file:", basename(x$path))
  if(!is.null(x$name)) {
    txt <- sprintf("%s => %s", txt, x$name)
  }
  if(!is.null(x$type)){
    txt <- sprintf("%s (type: %s)", txt, x$type)
  }
  cat(txt, "\n")
}

#' @export
print.form_data <- function(x, ...){
  txt <- paste(sprintf("Form data of length %.0f", length(x$value)))
  if(!is.null(x$type)){
    txt <- sprintf("%s (type: %s)", txt, x$type)
  }
  cat(txt, "\n")
}
