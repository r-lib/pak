#' Advanced download interface
#'
#' Download multiple files concurrently, with support for resuming large files.
#' This function is based on [multi_run()] and hence does not error in case any
#' of the individual requests fail; you should inspect the return value to find
#' out which of the downloads were completed successfully.
#'
#' Upon completion of all requests, this function returns a data frame with results.
#' The `success` column indicates if a request was successfully completed (regardless
#' of the HTTP status code). If it failed, e.g. due to a networking issue, the error
#' message is in the `error` column. A `success` value `NA` indicates that the request
#' was still in progress when the function was interrupted or reached the elapsed
#' `multi_timeout` and perhaps the download can be resumed if the server supports it.
#'
#' It is also important to inspect the `status_code` column to see if any of the
#' requests were successful but had a non-success HTTP code, and hence the downloaded
#' file probably contains an error page instead of the requested content.
#'
#' Note that when you set `resume = TRUE` you should expect HTTP-206 or HTTP-416
#' responses. The latter could indicate that the file was already complete, hence
#' there was no content left to resume from the server. If you try to resume a file
#' download but the server does not support this, success if `FALSE` and the file
#' will not be touched. In fact, if we request to a download to be resumed and the
#' server responds `HTTP 200` instead of `HTTP 206`, libcurl will error and not
#' download anything, because this probably means the server did not respect our
#' range request and is sending us the full file.
#'
#' ## About HTTP/2
#'
#' Availability of HTTP/2 can increase the performance when making many parallel
#' requests to a server, because HTTP/2 can multiplex many requests over a single
#' TCP connection. Support for HTTP/2 depends on the version of `libcurl` that
#' your system has, and the TLS back-end that is in use, check [curl_version].
#' For clients or servers without HTTP/2, curl makes at most 6 connections per
#' host over which it distributes the queued downloads.
#'
#' On Windows and MacOS you can switch the active TLS backend by setting an
#' environment variable [`CURL_SSL_BACKEND`](https://curl.se/libcurl/c/libcurl-env.html)
#' in your `~/.Renviron` file. On Windows you can switch between `SecureChannel`
#' (default) and `OpenSSL` where only the latter supports HTTP/2. On MacOS you
#' can use either `SecureTransport` or `LibreSSL`, the default varies by MacOS
#' version.
#'
#' @returns The function returns a data frame with one row for each downloaded file and
#' the following columns:
#'  - `success` if the HTTP request was successfully performed, regardless of the
#'  response status code. This is `FALSE` in case of a network error, or in case
#'  you tried to resume from a server that did not support this. A value of `NA`
#'  means the download was interrupted while in progress.
#'  - `status_code` the HTTP status code from the request. A successful download is
#'  usually `200` for full requests or `206` for resumed requests. Anything else
#'  could indicate that the downloaded file contains an error page instead of the
#'  requested content.
#'  - `resumefrom` the file size before the request, in case a download was resumed.
#'  - `url` final url (after redirects) of the request.
#'  - `destfile` downloaded file on disk.
#'  - `error` if `success == FALSE` this column contains an error message.
#'  - `type` the `Content-Type` response header value.
#'  - `modified` the `Last-Modified` response header value.
#'  - `time` total elapsed download time for this file in seconds.
#'  - `headers` vector with http response headers for the request.
#'
#' @export
#' @param urls vector with URLs to download. Alternatively it may also be a
#' list of [handle][new_handle] objects that have the `url` option already set.
#' @param destfiles vector (of equal length as `urls`) with paths of output files,
#' or `NULL` to use [basename] of urls.
#' @param resume if the file already exists, resume the download. Note that this may
#' change server responses, see details.
#' @param multi_timeout in seconds, passed to [multi_run]
#' @param progress print download progress information
#' @param multiplex passed to [new_pool]
#' @param ... extra handle options passed to each request [new_handle]
#' @examples \dontrun{
#' # Example: some large files
#' urls <- sprintf(
#'   "https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2021-%02d.parquet", 1:12)
#' res <- multi_download(urls, resume = TRUE) # You can interrupt (ESC) and resume
#'
#' # Example: revdep checker
#' # Download all reverse dependencies for the 'curl' package from CRAN:
#' pkg <- 'curl'
#' mirror <- 'https://cloud.r-project.org'
#' db <- available.packages(repos = mirror)
#' packages <- c(pkg, tools::package_dependencies(pkg, db = db, reverse = TRUE)[[pkg]])
#' versions <- db[packages,'Version']
#' urls <- sprintf("%s/src/contrib/%s_%s.tar.gz", mirror, packages,  versions)
#' res <- multi_download(urls)
#' all.equal(unname(tools::md5sum(res$destfile)), unname(db[packages, 'MD5sum']))
#' # And then you could use e.g.: tools:::check_packages_in_dir()
#'
#' # Example: URL checker
#' pkg_url_checker <- function(dir){
#'   db <- tools:::url_db_from_package_sources(dir)
#'   res <- multi_download(db$URL, rep('/dev/null', nrow(db)), nobody=TRUE)
#'   db$OK <- res$status_code == 200
#'   db
#' }
#'
#' # Use a local package source directory
#' pkg_url_checker(".")
#'
#' }
multi_download <- function(urls, destfiles = NULL, resume = FALSE, progress = TRUE,
                           multi_timeout = Inf, multiplex = FALSE, ...){
  if(inherits(urls, 'curl_handle'))
    urls <- list(urls)
  if(!is.character(urls) && !is.list(urls))
    stop("Argument 'urls' must be a character vector or list of handles")
  handles <- lapply(urls, make_one_handle)
  if(is.null(destfiles)){
    destfiles <- vapply(handles, guess_handle_filename, character(1))
  }
  dupes <- setdiff(destfiles[duplicated(destfiles)], c("/dev/null", "NUL"))
  if(length(dupes)){
    stop("Duplicate destfiles: ", paste(dupes, collapse = ', '))
  }
  stopifnot(length(urls) == length(destfiles))
  destfiles <- normalizePath(destfiles, mustWork = FALSE)
  writers <- rep(list(NULL), length(urls))
  errors <- rep(NA_character_, length(urls))
  success <- rep(NA, length(urls))
  resumefrom <- rep(0, length(urls))
  dlspeed <- rep(0, length(urls))
  expected <- rep(NA, length(urls))
  pool <- new_pool(multiplex = multiplex)
  total <- 0
  lapply(seq_along(urls), function(i){
    dest <- destfiles[i]
    handle <- handles[[i]]
    handle_setopt(handle, ..., noprogress = TRUE)
    if(isTRUE(resume) && file.exists(dest)){
      startsize <- file.info(dest)$size
      handle_setopt(handle, resume_from_large = startsize)
      total <<- total + startsize
      resumefrom[i] <- startsize
    }
    writer <- file_writer(dest, append = resume)
    multi_add(handle, pool = pool, data = function(buf, final){
      total <<- total + length(buf)
      writer(buf, final)
      if(isTRUE(progress)){
        if(is.na(expected[i])){
          expected[i] <<- handle_clength(handle) + resumefrom[i]
        }
        dlspeed[i] <<- ifelse(final, 0, handle_speed(handle)[1])
        print_progress(success, total, sum(dlspeed), sum(expected))
      }
    }, done = function(req){
      expected[i] <<- handle_received(handle) + resumefrom[i]
      success[i] <<- TRUE
      dlspeed[i] <<- 0
      if(expected[i] == 0 && !file.exists(dest)){
        file.create(dest) #create empty file
      }
      mtime <- handle_mtime(handle)
      if(!is.na(mtime)){
        Sys.setFileTime(dest, handle_mtime(handle))
      }
    }, fail = function(err){
      expected[i] <<- handle_received(handle) + resumefrom[i]
      success[i] <<- FALSE
      errors[i] <<- err
      dlspeed[i] <<- 0
    })
    writers[[i]] <<- writer
    if(isTRUE(progress) && (i %% 100 == 0)){
      print_stream("\rPreparing request %d of %d...", i, length(urls))
    }
  })
  on.exit(lapply(writers, function(writer){
    # fallback to close writer in case the download got interrupted
    writer(raw(0), close = TRUE)
  }))
  tryCatch({
    multi_run(timeout = multi_timeout, pool = pool)
    if(isTRUE(progress)){
      print_progress(success, total, sum(dlspeed), sum(expected), TRUE)
    }
  }, interrupt = function(e){
    message("download interrupted")
  })
  out <- lapply(handles, handle_data)
  results <- data.frame(
    success = success,
    status_code = vapply(out, function(x){x$status_code}, numeric(1)),
    resumefrom = resumefrom,
    url = vapply(out, function(x){x$url}, character(1)),
    destfile = normalizePath(destfiles, mustWork = FALSE),
    error = errors,
    type = vapply(out, function(x){x$type}, character(1)),
    modified = structure(vapply(out, function(x){x$modified}, numeric(1)), class = c("POSIXct", "POSIXt")),
    time = vapply(out, function(x){unname(x$times['total'])}, numeric(1)),
    stringsAsFactors = FALSE
  )
  results$headers <- lapply(out, function(x){parse_headers(x$headers)})
  class(results) <- c("tbl_df", "tbl", "data.frame")
  results
}

make_one_handle <- function(url_or_handle){
  if(inherits(url_or_handle, 'curl_handle')){
    handle <- url_or_handle
    url <- handle_data(handle)$url
    if(is.na(url) || identical(url, ""))
      stop("Handle passed to multi_download() but has no URL set")
    return(handle)
  }
  if(is.character(url_or_handle)){
    return(new_handle(url = url_or_handle))
  }
  stop("Argument urls does not contain url or curl handle")
}

guess_handle_filename <- function(handle){
  url <- handle_data(handle)$url
  destfile <- basename(curl_parse_url(url)$path)
  if(!length(destfile) || is.na(destfile) || !nchar(destfile))
    stop("Failed to guess filename for: ", url)
  destfile
}

# Print at most 10x per second in interactive, and once per sec in batch/CI
print_progress <- local({
  last <- 0
  function(sucvec, total, speed, expected, finalize = FALSE){
    throttle <- ifelse(interactive(), 0.1, 5)
    now <- unclass(Sys.time())
    if(isTRUE(finalize) || now - last > throttle){
      last <<- now
      done <- sum(!is.na(sucvec))
      pending <- sum(is.na(sucvec))
      pctstr <- if(!identical(expected, 0.0)){
        sprintf("(%s%%)", ifelse(is.na(expected), "??", as.character(round(100 * total/expected))))
      } else {""}
      speedstr <- if(!finalize){
        sprintf(" (%s/s)", format_size(speed))
      } else {""}
      downloaded <- format_size(total)
      print_stream('\rDownload status: %d done; %d in progress%s. Total size: %s %s...',
                   done, pending, speedstr, downloaded, pctstr)
    }
    if(finalize){
      cat(" done!             \n", file = stderr())
      flush(stderr())
    }
  }
})

print_stream <- function(...){
  cat(sprintf(...), file = stderr())
}
