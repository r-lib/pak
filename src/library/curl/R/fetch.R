#' Fetch the contents of a URL
#'
#' Low-level bindings to write data from a URL into memory, disk or a callback
#' function.
#'
#' The `curl_fetch_*()` functions automatically raise an error upon protocol problems
#' (network, disk, TLS, etc.) but do not implement application logic. For example,
#' you need to check the status code of HTTP requests in the response by yourself,
#' and deal with it accordingly.
#'
#' Both `curl_fetch_memory()` and `curl_fetch_disk` have a blocking and a
#' non-blocking C implementation. The latter is slightly slower but allows for
#' interrupting the download prematurely (using e.g. CTRL+C or ESC). Interrupting
#' is enabled when R runs in interactive mode or when
#' `getOption("curl_interrupt") == TRUE`.
#'
#' The `curl_fetch_multi()` function is the asynchronous equivalent of
#' `curl_fetch_memory()`. It wraps [`multi_add()`][multi_add] to
#' schedule requests which are executed concurrently when calling
#' [`multi_run()`][multi_run]\code{}. For each successful request, the
#' `done` callback is triggered with response data. For failed requests
#' (when `curl_fetch_memory()` would raise an error), the `fail` function
#' is triggered with the error message.
#'
#' @param url A character string naming the URL of a resource to be downloaded.
#' @param handle A curl handle object.
#' @export
#' @rdname curl_fetch
#' @useDynLib curl R_curl_fetch_memory
#' @examples
#' \donttest{
#' # Load in memory
#' res <- curl_fetch_memory("https://hb.cran.dev/cookies/set?foo=123&bar=ftw")
#' res$content
#'
#' # Save to disk
#' res <- curl_fetch_disk("https://hb.cran.dev/stream/10", tempfile())
#' res$content
#' readLines(res$content)
#'
#' # Stream with callback
#' drip_url <- "https://hb.cran.dev/drip?duration=3&numbytes=15&code=200"
#' res <- curl_fetch_stream(drip_url, function(x){
#'   cat(rawToChar(x))
#' })
#'
#' # Async API
#' data <- list()
#' success <- function(res){
#'   cat("Request done! Status:", res$status, "\n")
#'   data <<- c(data, list(res))
#' }
#' failure <- function(msg){
#'   cat("Oh noes! Request failed!", msg, "\n")
#' }
#' curl_fetch_multi("https://hb.cran.dev/get", success, failure)
#' curl_fetch_multi("https://hb.cran.dev/status/418", success, failure)
#' curl_fetch_multi("https://urldoesnotexist.xyz", success, failure)
#' multi_run()
#' str(data)
#' }
curl_fetch_memory <- function(url, handle = new_handle()){
  nonblocking <- isTRUE(getOption("curl_interrupt", TRUE))
  output <- .Call(R_curl_fetch_memory, enc2utf8(url), handle, nonblocking)
  res <- handle_data(handle)
  res$content <- output
  res
}

#' @export
#' @param path Path to save results
#' @rdname curl_fetch
#' @useDynLib curl R_curl_fetch_disk
curl_fetch_disk <- function(url, path, handle = new_handle()){
  nonblocking <- isTRUE(getOption("curl_interrupt", TRUE))
  path <- enc2native(normalizePath(path, mustWork = FALSE))
  output <- .Call(R_curl_fetch_disk, enc2utf8(url), handle, path, "wb", nonblocking)
  res <- handle_data(handle)
  res$content <- output
  res
}

#' @export
#' @param fun Callback function. Should have one argument, which will be
#'   a raw vector.
#' @rdname curl_fetch
#' @useDynLib curl R_curl_connection
curl_fetch_stream <- function(url, fun, handle = new_handle()){
  # Blocking = TRUE and partial = TRUE to prevent busy-waiting
  con <- curl_connection(url, mode = "", handle = handle, partial = TRUE)

  # 'f' means: do not error for status code
  open(con, "rbf")
  on.exit(close(con))
  while(isIncomplete(con)){
    buf <- readBin(con, raw(), 32768L)
    if(length(buf))
      fun(buf)
  }
  handle_data(handle)
}

#' @export
#' @rdname curl_fetch
#' @inheritParams multi
#' @useDynLib curl R_curl_connection
curl_fetch_multi <- function(url, done = NULL, fail = NULL, pool = NULL,
                             data = NULL, handle = new_handle()){
  handle_setopt(handle, url = enc2utf8(url))
  multi_add(handle = handle, done = done, fail = fail, data = data, pool = pool)
  invisible(handle)
}

#' @export
#' @rdname curl_fetch
curl_fetch_echo <- function(url, handle = new_handle()){
  handle_setopt(handle, url = enc2utf8(url))
  curl_echo(handle)
}
