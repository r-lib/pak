#' Create and configure a curl handle
#'
#' Handles are the work horses of libcurl. A handle is used to configure a
#' request with custom options, headers and payload. Once the handle has been
#' set up, it can be passed to any of the download functions such as \code{\link{curl}}
#' ,\code{\link{curl_download}} or \code{\link{curl_fetch_memory}}. The handle will maintain
#' state in between requests, including keep-alive connections, cookies and
#' settings.
#'
#' Use \code{new_handle()} to create a new clean curl handle that can be
#' configured with custom options and headers. Note that \code{handle_setopt}
#' appends or overrides options in the handle, whereas \code{handle_setheaders}
#' replaces the entire set of headers with the new ones. The \code{handle_reset}
#' function resets only options/headers/forms in the handle. It does not affect
#' active connections, cookies or response data from previous requests. The safest
#' way to perform multiple independent requests is by using a separate handle for
#' each request. There is very little performance overhead in creating handles.
#'
#' @family handles
#' @param ... named options / headers to be set in the handle.
#'   To send a file, see \code{\link{form_file}}. To list all allowed options,
#'   see \code{\link{curl_options}}
#' @return A handle object (external pointer to the underlying curl handle).
#'   All functions modify the handle in place but also return the handle
#'   so you can create a pipeline of operations.
#' @export
#' @name handle
#' @useDynLib curl R_new_handle
#' @rdname handle
#' @examples
#' h <- new_handle()
#' handle_setopt(h, customrequest = "PUT")
#' handle_setform(h, a = "1", b = "2")
#' r <- curl_fetch_memory("https://hb.cran.dev/put", h)
#' cat(rawToChar(r$content))
#'
#' # Or use the list form
#' h <- new_handle()
#' handle_setopt(h, .list = list(customrequest = "PUT"))
#' handle_setform(h, .list = list(a = "1", b = "2"))
#' r <- curl_fetch_memory("https://hb.cran.dev/put", h)
#' cat(rawToChar(r$content))
new_handle <- function(...){
  h <- .Call(R_new_handle)
  handle_setopt(h, ...)
  h
}

#' @export
#' @useDynLib curl R_handle_setopt
#' @param handle Handle to modify
#' @param .list A named list of options. This is useful if you've created
#'   a list of options elsewhere, avoiding the use of \code{do.call()}.
#' @rdname handle
handle_setopt <- function(handle, ..., .list = list()){
  stopifnot(inherits(handle, "curl_handle"))
  values <- c(list(...), .list)
  opt_names <- fix_options(tolower(names(values)))
  keys <- as.integer(curl_options()[opt_names])
  na_keys <- is.na(keys)
  if(any(na_keys)){
    bad_opts <- opt_names[na_keys]
    stop("Unknown option", ifelse(length(bad_opts) > 1, "s: ", ": "),
      paste(bad_opts, collapse=", "))
  }
  stopifnot(length(keys) == length(values))
  .Call(R_handle_setopt, handle, keys, values)
  invisible(handle)
}

#' @export
#' @rdname handle
handle_setheaders <- function(handle, ..., .list = list()){
  x <- c(list(...), .list)
  if(!all(vapply(x, is.character, logical(1)))){
    stop("All headers must be strings.")
  }
  handle_setopt(handle, httpheader = format_request_headers(x))
}

format_request_headers <- function(x){
  x$Expect = ""
  names <- names(x)
  values <- as.character(unlist(x))
  postfix <- ifelse(grepl("^\\s+$", values), ";", paste(":", values))
  paste0(names, postfix)
}

#' @useDynLib curl R_handle_getheaders
#' @rdname handle
handle_getheaders <- function(handle){
  stopifnot(inherits(handle, "curl_handle"))
  .Call(R_handle_getheaders, handle)
}

#' @useDynLib curl R_handle_getcustom
#' @rdname handle
handle_getcustom <- function(handle){
  stopifnot(inherits(handle, "curl_handle"))
  .Call(R_handle_getcustom, handle)
}

#' @export
#' @useDynLib curl R_handle_setform
#' @rdname handle
handle_setform <- function(handle, ..., .list = list()){
  stopifnot(inherits(handle, "curl_handle"))
  form <- c(list(...), .list)
  for(i in seq_along(form)){
    val <- form[[i]];
    if(is.character(val)){
      form[[i]] <- charToRaw(enc2utf8(val))
    } else if(!is.raw(val) && !inherits(val, "form_file") && !inherits(val, "form_data")){
      stop("Unsupported value type for form field '", names(form[i]), "'.")
    }
  }
  .Call(R_handle_setform, handle, form)
  invisible(handle)
}

#' @export
#' @rdname handle
#' @useDynLib curl R_handle_reset
handle_reset <- function(handle){
  stopifnot(inherits(handle, "curl_handle"))
  .Call(R_handle_reset, handle)
  invisible(handle)
}

#' Extract cookies from a handle
#'
#' The \code{handle_cookies} function returns a data frame with 7 columns as specified in the
#' \href{http://www.cookiecentral.com/faq/#3.5}{netscape cookie file format}.
#'
#' @useDynLib curl R_get_handle_cookies
#' @export
#' @param handle a curl handle object
#' @family handles
#' @examples
#' h <- new_handle()
#' handle_cookies(h)
#'
#' # Server sets cookies
#' req <- curl_fetch_memory("https://hb.cran.dev/cookies/set?foo=123&bar=ftw", handle = h)
#' handle_cookies(h)
#'
#' # Server deletes cookies
#' req <- curl_fetch_memory("https://hb.cran.dev/cookies/delete?foo", handle = h)
#' handle_cookies(h)
#'
#' # Cookies will survive a reset!
#' handle_reset(h)
#' handle_cookies(h)
handle_cookies <- function(handle){
  stopifnot(inherits(handle, "curl_handle"))
  cookies <- .Call(R_get_handle_cookies, handle)
  df <- if(length(cookies)){
    values <- lapply(strsplit(cookies, split="\t"), `[`, 1:7)
    as.data.frame(do.call(rbind, values), stringsAsFactors = FALSE)
  } else {
    as.data.frame(matrix(ncol=7, nrow=0))
  }
  names(df) <- c("domain", "flag", "path", "secure", "expiration", "name", "value")
  df$flag <- as.logical(df$flag)
  df$secure <- as.logical(df$secure)
  expires <- as.numeric(df$expiration)
  expires[expires==0] <- Inf
  class(expires) = c("POSIXct", "POSIXt");
  df$expiration <- expires
  df

}

#' @export
#' @rdname handle
#' @useDynLib curl R_get_handle_response
handle_data <- function(handle){
  stopifnot(inherits(handle, "curl_handle"))
  out <- .Call(R_get_handle_response, handle)
  out$content = NULL
  out
}

# This is for internal use in progress bars. When the download is complete,
# the speed is equal to content-size / elapsed-time.
#' @useDynLib curl R_get_handle_speed
handle_speed <- function(handle){
  .Call(R_get_handle_speed, handle)
}

#' @useDynLib curl R_get_handle_clength
handle_clength <- function(handle){
  .Call(R_get_handle_clength, handle)
}

#' @useDynLib curl R_get_handle_received
handle_received <- function(handle){
  .Call(R_get_handle_received, handle)
}

#' @useDynLib curl R_get_handle_mtime
handle_mtime <- function(handle){
  .Call(R_get_handle_mtime, handle)
}

#' @export
print.curl_handle <- function(x, ...){
  stopifnot(inherits(x, "curl_handle"))
  url <- handle_data(x)$url
  if(!nchar(url)) url <- "empty"
  cat(sprintf("<curl handle> (%s)\n", url))
}

# Only for testing memory leaks
#' @useDynLib curl R_total_handles
total_handles <- function(){
  .Call(R_total_handles)
}

## Some hacks for backward compatibilty
fix_options <- function(opt_names){
  # Recent libcurl should use xferinfo instead of progress
  has_xferinfo <- length(curl_options("xferinfofunction"))
  if(has_xferinfo){
    opt_names[opt_names == "progressfunction"] <- "xferinfofunction"
    return(opt_names)
  } else {
    opt_names[opt_names == "xferinfofunction"] <- "progressfunction"
    return(opt_names)
  }
}
