
default_http_version <- function() {
  os <- Sys.info()["sysname"]
  if (!is.na(os) && os %in% c("Darwin", "Linux")) {
    # FIXME: when is it safe to remove this? Does it depend on the OS
    # version? The libcurl version?
    # UPDATE: HTTP/2 now also causes issues on Linux:
    # https://github.com/r-lib/pak/issues/358
    # https://github.com/r-lib/actions/issues/483
    # So this will be here for now. :(
    2 # HTTP 1.1
  } else {
    0 # whatever curl chooses
  }
}

#' @importFrom utils modifyList

update_async_timeouts <- function(options) {
  getopt <- function(nm) {
    if (!is.null(v <- options[[nm]])) return(v)
    anm <- paste0("pkgcache_", nm)
    if (!is.null(v <- getOption(anm))) return(v)
    if (!is.na(v <- Sys.getenv(toupper(anm), NA_character_))) return (v)
  }
  utils::modifyList(
    options,
    list(
      timeout = as.integer(getopt("timeout") %||% 0),
      connecttimeout = as.integer(getopt("connecttimeout") %||% 300),
      low_speed_time = as.integer(getopt("low_speed_time") %||% 0),
      low_speed_limit = as.integer(getopt("low_speed_limit") %||% 0),
      http_version = as.integer(getopt("http_version") %||% default_http_version())
    )
  )
}

#' Download a file, asynchronously
#'
#' This is the asynchronous version of [utils::download.file()].
#'
#' `download_file` also has some nice improvements:
#' * It uses a temporary file, so never leaves a partial file at `destfile`.
#' * It can write the HTTP ETag from the response to a file, which can
#'   be used in [download_if_newer()], etc.
#' * It returns the HTTP response as part of the error message if the
#'   response status code indicates a client or server error.
#' * Well, it is asynchronous.
#'
#' @param url URL to download.
#' @param destfile Destination file.
#' @param etag_file If not `NULL`, and the response is successful and
#'   includes an `ETag` header, then this header is stored in this file.
#'   It can be used to cache the file, with the [download_if_newer()] or
#'   the [download_one_of()] functions.
#' @param tmp_destfile Where to store the temporary destination file.
#' @param error_on_status Whether to error for an HTTP status larger
#'   than or equal to 400. If this is `FALSE`, then an error object is
#'   returned for these status codes.
#' @param options Curl options.
#' @param ... Additional arguments are passed to [http_get()].
#' @return A [deferred] object. It resolves to a list with entries:
#'   * `url`: The URL in the request.
#'   * `destfile`: The destination file.
#'   * `response`: The response object from the curl package.
#'   * `etag`: The ETag of the response, of `NULL` if missing.
#'   * `etag_file`: The file the ETag was written to, or `NULL` otherwise
#'
#' @family async HTTP tools
#' @noRd
#' @section Examples:
#' ```
#' dest1 <- tempfile(fileext = ".jpeg")
#' dest2 <- tempfile(fileext = ".png")
#' dl <- function() {
#'   when_all(
#'     download_file("https://httpbin.org/image/jpeg", dest1),
#'     download_file("https://httpbin.org/image/png", dest2)
#'   )
#' }
#' resps <- synchronise(dl())
#' lapply(resps, function(x) x$response$status_code)
#' resps[[1]]$url
#' resps[[1]]$destfile
#' resps[[1]]$response$times
#' file.exists(dest1)
#' file.exists(dest2)
#'
#' ## HTTP errors contain the response
#' dest <- tempfile()
#' err <- tryCatch(
#'   synchronise(download_file("https://httpbin.org/status/418", dest)),
#'   error = function(e) e
#' )
#' err
#' names(err)
#' cat(rawToChar(err$response$content))
#' ```

download_file <- function(url, destfile, etag_file = NULL,
                          tmp_destfile = paste0(destfile, ".tmp"),
                          error_on_status = TRUE,
                          options = list(), ...) {
  "!DEBUG downloading `url`"
  assert_that(
    is_string(url),
    is_path(destfile),
    is_path(tmp_destfile),
    is_path_or_null(etag_file),
    is_flag(error_on_status),
    is.list(options))
  force(list(...))

  options <- update_async_timeouts(options)
  destfile <- normalizePath(destfile, mustWork = FALSE)
  tmp_destfile <- normalizePath(tmp_destfile, mustWork = FALSE)
  mkdirp(dirname(tmp_destfile))

  http_get(url, file = tmp_destfile, options = options, ...)$
    then(http_stop_for_status)$
    then(function(resp) {
      "!DEBUG downloaded `url`"
      file.rename(tmp_destfile, destfile)
      etag <- curl::parse_headers_list(resp$headers)[["etag"]] %||% NA_character_
      if (!is.null(etag_file) && !is.na(etag[1])) {
        mkdirp(dirname(etag_file))
        writeLines(etag, etag_file)
      }
      list(url = url, destfile = destfile, response = resp, etag = etag,
           etag_file = etag_file)
    })$
    catch(error = function(err) {
      "!DEBUG downloading `url` failed"
      err$destfile <- destfile
      err$url <- url
      if (error_on_status) stop(err) else err
    })
}

read_etag <- function(etag_file) {
  tryCatch(
    suppressWarnings(read_lines(etag_file, n = 1, warn = FALSE)[1]),
    error = function(e) NA
  )
}

get_etag_header_from_file <- function(destfile, etag_file) {
  if (!is.null(etag_file)) {
    etag_old <- read_etag(etag_file)
    if (file.exists(destfile) && !is.na(etag_old)) {
      c("If-None-Match" = etag_old)
    }
  }
}

#' Download a file, if it is newer than a local file
#'
#' A version of [download_file()] that only downloads if the file at the
#' specified URL is different from the local one.
#'
#' @inheritParams download_file
#' @param etag_file If not `NULL` then the path to a file that may contain
#'   the ETag of a previous request to this URL. If `destfile` exists, and
#'   `etag_file` exists and it is not empty, then the `If-None-Match` HTTP
#'   header is used with this ETag to avoid downloading the file if it has
#'   not changed. If the file at `url` has changed, then it is downloaded,
#'   and the the new ETag is stored in `etag_file`.
#' @param headers HTTP headers to add to the request, a named character
#'   vector.
#' @inherit download_file return
#'
#' @family async HTTP tools
#' @noRd
#' @section Examples:
#' ```
#' dest <- tempfile(fileext = ".jpeg")
#' etag <- tempfile()
#' dl <- function() {
#'   ## This URL will repond with an ETag
#'   download_if_newer("https://httpbin.org/etag/test", dest,
#'                     etag_file = etag)
#' }
#' file.exists(dest)
#' file.exists(etag)
#'
#' res1 <- synchronise(dl())
#'
#' ## Downloaded the file, and also created the etag file
#' file.exists(dest)
#' file.exists(etag)
#' readLines(etag)
#' res1$response$status_code
#'
#' ## This will not download the file again, as the ETag matches
#' ## The status code is 304 Not Modified
#' res2 <- synchronise(dl())
#' res2$response$status_code
#'
#' ## HTTP errors contain the response
#' dest <- tempfile()
#' etag <- tempfile()
#' err <- tryCatch(
#'   synchronise(download_if_newer("https://httpbin.org/status/418",
#'                                 dest, etag)),
#'   error = function(e) e
#' )
#' err
#' names(err)
#' cat(rawToChar(err$response$content))
#' ```

download_if_newer <- function(url, destfile, etag_file = NULL,
                              headers = NULL,
                              tmp_destfile = paste0(destfile, ".tmp"),
                              error_on_status = TRUE,
                              options = list(), ...) {
  "!DEBUG download if newer `url`"
  headers <- headers %||% structure(character(), names = character())
  assert_that(
    is_string(url),
    is_path(destfile),
    is_path(tmp_destfile),
    is_path_or_null(etag_file),
    is.character(headers), all_named(headers),
    is_flag(error_on_status),
    is.list(options))
  force(list(...))

  options <- update_async_timeouts(options)
  etag_old <- get_etag_header_from_file(destfile, etag_file)
  headers <- c(headers, etag_old)

  destfile <- normalizePath(destfile, mustWork = FALSE)
  tmp_destfile <- normalizePath(tmp_destfile, mustWork = FALSE)
  mkdirp(dirname(tmp_destfile))

  http_get(url, file = tmp_destfile, headers = headers,
           options = options, ...)$
    then(http_stop_for_status)$
    then(function(resp) {
      if (resp$status_code == 304) {
        "!DEBUG download not needed, `url` current"
        etag <- unname(etag_old)
      } else if (resp$status_code == 200 || resp$status_code == 0) {
        "!DEBUG downloaded `url`"
        file.rename(tmp_destfile, destfile)
        etag <- curl::parse_headers_list(resp$headers)[["etag"]] %||% NA_character_
        if (!is.null(etag_file) && !is.na(etag[1])) {
          mkdirp(dirname(etag_file))
          writeLines(etag, etag_file)
        }
      } else {
        err <- structure(
          list(response = resp, message = "Unknown HTTP response"),
          class = c("error", "condition"))
        stop(err)
      }
      list(url = url, destfile = destfile, response = resp, etag = etag,
           etag_file = etag_file)
    })$
    catch(error = function(err) {
      "!DEBUG downloading `url` failed"
      err$destfile <- destfile
      err$url <- url
      if (error_on_status) stop(err) else err
    })

}

#' Download a files from multiple candidate URLs
#'
#' Uses [download_if_newer()] to starts downloads in parallel, and the
#' download that completes first is kept. (The others will be cancelled.)
#' Download errors are ignored, as long as at least one download completes
#' successfully.
#'
#' It also uses ETags, so if the destination file already exists, and one
#' of the URLs contain the same file (and this request completes first),
#' the file is not downloaded again.
#'
#' If all URLs fail, then `download_one_of` throws an error of class
#' `download_one_of_error`. The error object contains all errors from
#' the underlying [download_if_newer()] calls, in a list, in the
#' `errors` member.
#'
#' @inheritParams download_if_newer
#' @param urls A non-empty character vector of alternative URLs to try.
#' @inherit download_if_newer return
#'
#' @family async HTTP tools
#' @noRd
#' @section Examples:
#' ```
#' dest <- tempfile()
#' ## The first URL answers after a 1s delay,
#' ## the second after a 10s delay,
#' ## the third throws an error immediately, so it will be ignored.
#' ## Once the first URL responds, the second is cancelled, so the call
#' ## will finish well before the 10s are over.
#' dl <- function() {
#'   download_one_of(
#'     c("https://httpbin.org/delay/1",
#'       "https://httpbin.org/delay/10",
#'       "https://httpbin.org/status/404"),
#'     dest)
#' }
#' system.time(res <- synchronise(dl()))
#' file.exists(dest)
#' readLines(dest)
#'
#' ## Which URL responded
#' res$response$url
#'
#' ## If all URLs fail
#' dl2 <- function() {
#'   download_one_of(
#'     c("https://httpbin.org/status/418",
#'       "https://httpbin.org/status/401"),
#'     tempfile()
#'   )
#' }
#' res <- tryCatch(synchronise(dl2()), error = function(e) e)
#' res
#' res$errors
#' cat(rawToChar(res$errors[[1]]$response$content))
#' ```

download_one_of <- function(urls, destfile, etag_file = NULL,
                            headers = NULL, error_on_status = TRUE,
                            options = list(), ...) {
  "!DEBUG trying multiple URLs"
  headers <- headers %||% structure(character(), names = character())
  assert_that(
    is_character(urls),  length(urls) >= 1,
    is_path(destfile),
    is_path_or_null(etag_file),
    is.character(headers), all_named(headers),
    is_flag(error_on_status),
    is.list(options))
  force(list(...))

  options <- update_async_timeouts(options)
  tmps <- paste0(destfile, ".tmp.", seq_along(urls))
  dls <- mapply(
    download_if_newer, url = urls, tmp_destfile = tmps,
    MoreArgs = list(destfile = destfile, etag_file = etag_file,
                    headers = headers, options = options, ...),
    SIMPLIFY = FALSE)

  when_any(.list = dls)$
    catch(error = function(err) {
      err$message <- "All URLs failed"
      class(err) <- c("download_one_of_error", class(err))
      if (error_on_status) stop(err) else err
    })
}

download_files <- function(data, error_on_status = TRUE,
                           options = list(), ...) {

  if (any(dup <- duplicated(data$path))) {
    stop("Duplicate target paths in download_files: ",
         paste0("`", unique(data$path[dup]), "`", collapse = ", "), ".")
  }

  options <- update_async_timeouts(options)
  bar <- create_progress_bar(data)
  prog_cb <- function(upd) update_progress_bar_progress(bar, upd)

  dls <- lapply(seq_len(nrow(data)), function(idx) {
    row <- data[idx, ]
    dx <- download_if_newer(
      row$url, row$path, row$etag,
      on_progress = prog_cb,
      error_on_status = error_on_status,
      options = options, ...
    )

    if ("fallback_url" %in% names(row) && !is.na(row$fallback_url)) {
      dx <- dx$catch(error = function(err) {
        download_if_newer(
          row$fallback_url, row$path, row$etag,
          error_on_status = error_on_status,
          options = options, ...
        )
      })
    }

    dx <- dx$
      then(function(result) {
        status_code <- result$response$status_code
        if (status_code == 304) {
          update_progress_bar_uptodate(bar, row$url)
        } else {
          update_progress_bar_done(bar, row$url)
        }
        result
      })

    if (isTRUE(row$mayfail)) {
      dx$catch(error = function(err) {
        cat("", file = row$path, append = TRUE)
        err
      })
    } else {
      dx
    }
  })

  ok <- FALSE
  when_all(.list = dls)$
    then(function(result) { ok <<- TRUE; result })$
    finally(function() finish_progress_bar(ok, bar))
}
