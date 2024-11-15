#' Async Concurrent Requests
#'
#' AJAX style concurrent requests, possibly using HTTP/2 multiplexing.
#' Results are only available via callback functions. Advanced use only!
#' For downloading many files in parallel use [multi_download] instead.
#'
#' Requests are created in the usual way using a curl [handle] and added
#' to the scheduler with [multi_add]. This function returns immediately
#' and does not perform the request yet. The user needs to call [multi_run]
#' which performs all scheduled requests concurrently. It returns when all
#' requests have completed, or case of a `timeout` or `SIGINT` (e.g.
#' if the user presses `ESC` or `CTRL+C` in the console). In case of
#' the latter, simply call [multi_run] again to resume pending requests.
#'
#' When the request succeeded, the `done` callback gets triggered with
#' the response data. The structure if this data is identical to [curl_fetch_memory].
#' When the request fails, the `fail` callback is triggered with an error
#' message. Note that failure here means something went wrong in performing the
#' request such as a connection failure, it does not check the http status code.
#' Just like [curl_fetch_memory], the user has to implement application logic.
#'
#' Raising an error within a callback function stops execution of that function
#' but does not affect other requests.
#'
#' A single handle cannot be used for multiple simultaneous requests. However
#' it is possible to add new requests to a pool while it is running, so you
#' can re-use a handle within the callback of a request from that same handle.
#' It is up to the user to make sure the same handle is not used in concurrent
#' requests.
#'
#' The [multi_cancel] function can be used to cancel a pending request.
#' It has no effect if the request was already completed or canceled.
#'
#' The [multi_fdset] function returns the file descriptors curl is
#' polling currently, and also a timeout parameter, the number of
#' milliseconds an application should wait (at most) before proceeding. It
#' is equivalent to the `curl_multi_fdset` and
#' `curl_multi_timeout` calls. It is handy for applications that is
#' expecting input (or writing output) through both curl, and other file
#' descriptors.
#'
#' @name multi
#' @rdname multi
#' @seealso Advanced download interface: [multi_download]
#' @useDynLib curl R_multi_add
#' @param handle a curl [handle] with preconfigured `url` option.
#' @param done callback function for completed request. Single argument with
#' response data in same structure as [curl_fetch_memory].
#' @param fail callback function called on failed request. Argument contains
#' error message.
#' @param data (advanced) callback function, file path, or connection object for writing
#' incoming data. This callback should only be used for *streaming* applications,
#' where small pieces of incoming data get written before the request has completed. The
#' signature for the callback function is `write(data, final = FALSE)`. If set
#' to `NULL` the entire response gets buffered internally and returned by in
#' the `done` callback (which is usually what you want).
#' @param pool a multi handle created by [new_pool]. Default uses a global pool.
#' @export
#' @examples
#' results <- list()
#' success <- function(x){
#'   results <<- append(results, list(x))
#' }
#' failure <- function(str){
#'   cat(paste("Failed request:", str), file = stderr())
#' }
#' # This handle will take longest (3sec)
#' h1 <- new_handle(url = "https://hb.cran.dev/delay/3")
#' multi_add(h1, done = success, fail = failure)
#'
#' # This handle writes data to a file
#' con <- file("output.txt")
#' h2 <- new_handle(url = "https://hb.cran.dev/post", postfields = "bla bla")
#' multi_add(h2, done = success, fail = failure, data = con)
#'
#' # This handle raises an error
#' h3 <- new_handle(url = "https://urldoesnotexist.xyz")
#' multi_add(h3, done = success, fail = failure)
#'
#' # Actually perform the requests
#' multi_run(timeout = 2)
#' multi_run()
#'
#' # Check the file
#' readLines("output.txt")
#' unlink("output.txt")
multi_add <- function(handle, done = NULL, fail = NULL, data = NULL, pool = NULL){
  if(is.null(pool))
    pool <- multi_default()
  if(inherits(data, "connection")){
    con <- data
    if(isOpen(con) && identical(summary(con)$text, "text")){
      data <- function(x, finalize = FALSE){
        cat(rawToChar(x), file = con)
        flush(con)
      }
    } else {
      was_open <- isOpen(con)
      data <- function(x, finalize = FALSE){
        if(!isOpen(con))
          open(con, 'wb')
        writeBin(x, con = con)
        if(isTRUE(finalize && !was_open)){
          close(con)
        } else {
          flush(con)
        }
      }
    }
  } else if(is_string(data)){
    data <- file_writer(path = data)
  }
  stopifnot(inherits(handle, "curl_handle"))
  stopifnot(inherits(pool, "curl_multi"))
  stopifnot(is.null(done) || is.function(done))
  stopifnot(is.null(fail) || is.function(fail))
  stopifnot(is.null(data) || is.function(data))
  .Call(R_multi_add, handle, done, fail, data, pool)
}

#' @param timeout max time in seconds to wait for results. Use `0` to poll for results without
#' waiting at all.
#' @param poll If `TRUE` then return immediately after any of the requests has completed.
#' May also be an integer in which case it returns after n requests have completed.
#' @export
#' @useDynLib curl R_multi_run
#' @rdname multi
multi_run <- function(timeout = Inf, poll = FALSE, pool = NULL){
  if(is.null(pool))
    pool <- multi_default()
  stopifnot(is.numeric(timeout))
  stopifnot(inherits(pool, "curl_multi"))
  .Call(R_multi_run, pool, timeout, as.integer(poll))
}

#' @param total_con max total concurrent connections.
#' @param host_con max concurrent connections per host.
#' @param multiplex enable HTTP/2 multiplexing if supported by host and client.
#' @export
#' @useDynLib curl R_multi_setopt
#' @rdname multi
multi_set <- function(total_con = 50, host_con = 6, multiplex = TRUE, pool = NULL){
  if(is.null(pool))
    pool <- multi_default()
  stopifnot(inherits(pool, "curl_multi"))
  stopifnot(is.numeric(total_con))
  stopifnot(is.numeric(host_con))
  stopifnot(is.logical(multiplex))
  .Call(R_multi_setopt, pool, total_con, host_con, multiplex)
}

#' @export
#' @useDynLib curl R_multi_list
#' @rdname multi
multi_list <- function(pool = NULL){
  if(is.null(pool))
    pool <- multi_default()
  stopifnot(inherits(pool, "curl_multi"))
  as.list(.Call(R_multi_list, pool))
}

#' @export
#' @useDynLib curl R_multi_cancel
#' @rdname multi
multi_cancel <- function(handle){
  stopifnot(inherits(handle, "curl_handle"))
  .Call(R_multi_cancel, handle)
}

#' @export
#' @useDynLib curl R_multi_new
#' @rdname multi
new_pool <- function(total_con = 100, host_con = 6, multiplex = TRUE){
  pool <- .Call(R_multi_new)
  multi_set(pool = pool, total_con = total_con, host_con = host_con, multiplex = multiplex)
}

multi_default <- local({
  global_multi_handle <- NULL
  function(){
    if(is.null(global_multi_handle)){
      global_multi_handle <<- new_pool()
    }
    stopifnot(inherits(global_multi_handle, "curl_multi"))
    return(global_multi_handle)
  }
})

#' @export
print.curl_multi <- function(x, ...){
  len <- length(multi_list(x))
  cat(sprintf("<curl multi-pool> (%d pending requests)\n", len))
}

#' @export
#' @useDynLib curl R_multi_fdset
#' @rdname multi

multi_fdset <- function(pool = NULL){
  if(is.null(pool))
    pool <- multi_default()
  # line below duplicates checks made by C code, but may need to be reinstated if that ever changes
  # stopifnot(inherits(pool, c("curl_multi", "curl")))
  .Call(R_multi_fdset, pool)
}
