#' @rdname curl_options
#' @export
curl_symbols <- function(filter = ""){
  m <- grep(filter, curl_symbol_data$name, ignore.case = TRUE)
  curl_symbol_data[m,]
}

#' @useDynLib curl R_curl_version
#' @export
#' @rdname curl_options
#' @examples
#' # Curl/ssl version info
#' curl_version()
curl_version <- function(){
  .Call(R_curl_version);
}

#' Parse date/time
#'
#' Can be used to parse dates appearing in http response headers such
#' as `Expires` or `Last-Modified`. Automatically recognizes
#' most common formats. If the format is known, [strptime()]
#' might be easier.
#'
#' @param datestring a string consisting of a timestamp
#' @useDynLib curl R_curl_getdate
#' @export
#' @examples
#' # Parse dates in many formats
#' parse_date("Sunday, 06-Nov-94 08:49:37 GMT")
#' parse_date("06 Nov 1994 08:49:37")
#' parse_date("20040911 +0200")
parse_date <- function(datestring){
  out <- .Call(R_curl_getdate, datestring);
  class(out) <- c("POSIXct", "POSIXt")
  out
}


#' @useDynLib curl R_split_string
split_string <- function(x, split = ":"){
  .Call(R_split_string, x, split)
}

trimws <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

is_string <- function(x){
  is.character(x) && length(x) && nchar(x)
}

# Callback for typed libcurl errors
raise_libcurl_error <- function(errnum, message, errbuf = NULL, source_url = NULL){
  error_code <- libcurl_error_codes[errnum]
  if(is.na(error_code))
    error_code <- NULL #future proof new error codes
  if(is_string(source_url)){
    host <- try_parse_url(source_url)$host
    if(is_string(host))
      message <- sprintf('%s [%s]', message, host)
  }
  if(is_string(errbuf)){
    message <- sprintf('%s: %s', message, errbuf)
  }
  cl <- sys.call(-1)
  e <- structure(
    class = c(error_code, "curl_error", "error", "condition"),
    list(message = message, call = cl)
  )
  stop(e)
}
