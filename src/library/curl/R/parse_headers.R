#' Parse response headers
#'
#' Parse response header data as returned by curl_fetch, either as a set of strings
#' or into a named list.
#'
#' The parse_headers_list function parses the headers into a normalized (lowercase
#' field names, trimmed whitespace) named list.
#'
#' If a request has followed redirects, the data can contain multiple sets of headers.
#' When multiple = TRUE, the function returns a list with the response headers
#' for each request. By default it only returns the headers of the final request.
#'
#' @param txt raw or character vector with the header data
#' @param multiple parse multiple sets of headers separated by a blank line. See details.
#' @export
#' @rdname parse_headers
#' @examples req <- curl_fetch_memory("https://hb.cran.dev/redirect/3")
#' parse_headers(req$headers)
#' parse_headers(req$headers, multiple = TRUE)
#'
#' # Parse into named list
#' parse_headers_list(req$headers)
parse_headers <- function(txt, multiple = FALSE){
  if(!length(txt))
    return(NULL)
  if(is.raw(txt)){
    txt <- rawToChar(txt)
  }
  stopifnot(is.character(txt))
  if(length(txt) > 1){
    txt <- paste(txt, collapse = "\n")
  }

  # Allow for either "\r\n" line breaks or just "\r" or "\n" (i.e. windows servers)
  sets <- strsplit(txt, "\\r\\n\\r\\n|\\n\\n|\\r\\r")[[1]]
  headers <- strsplit(sets, "\\r\\n|\\n|\\r")
  if(multiple){
    headers
  } else {
    headers[[length(headers)]]
  }
}

#' @export
#' @rdname parse_headers
parse_headers_list <- function(txt){
  headers <- grep(":", parse_headers(txt), fixed = TRUE, value = TRUE)
  out <- lapply(headers, split_string, ":")
  names <- tolower(vapply(out, `[[`, character(1), 1)) #names are case insensitive
  values <- lapply(lapply(out, `[[`, 2), trimws)
  names(values) <- names
  values
}
