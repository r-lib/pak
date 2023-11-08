#' Streaming JSON input/output
#'
#' The `stream_in` and `stream_out` functions implement line-by-line processing
#' of JSON data over a [connection], such as a socket, url, file or pipe. JSON
#' streaming requires the [ndjson](http://ndjson.org) format, which slightly differs
#' from [fromJSON()] and [toJSON()], see details.
#'
#' Because parsing huge JSON strings is difficult and inefficient, JSON streaming is done
#' using **lines of minified JSON records**, a.k.a. [ndjson](http://ndjson.org).
#' This is pretty standard: JSON databases such as MongoDB use the same format to
#' import/export datasets. Note that this means that the
#' total stream combined is not valid JSON itself; only the individual lines are. Also note
#' that because line-breaks are used as separators, prettified JSON is not permitted: the
#' JSON lines *must* be minified. In this respect, the format is a bit different from
#' [fromJSON()] and [toJSON()] where all lines are part of a single JSON
#' structure with optional line breaks.
#'
#' The `handler` is a callback function which is called for each page (batch) of
#' JSON data with exactly one argument (usually a data frame with `pagesize` rows).
#' If `handler` is missing or `NULL`, a default handler is used which stores all
#' intermediate pages of data, and at the very end binds all pages together into one single
#' data frame that is returned by `stream_in`. When a custom `handler` function
#' is specified, `stream_in` does not store any intermediate results and always returns
#' `NULL`. It is then up to the `handler` to process or store data pages.
#' A `handler` function that does not store intermediate results in memory (for
#' example by writing output to another connection) results in a pipeline that can process an
#' unlimited amount of data. See example.
#'
#' Note that a vector of JSON strings already in R can parsed with `stream_in` by
#' creating a connection to it with [textConnection()].
#'
#' If a connection is not opened yet, `stream_in` and `stream_out`
#' will automatically open and later close the connection. Because R destroys connections
#' when they are closed, they cannot be reused. To use a single connection for multiple
#' calls to `stream_in` or `stream_out`, it needs to be opened
#' beforehand. See example.
#'
#' @param con a [connection] object. If the connection is not open,
#' `stream_in` and `stream_out` will automatically open
#' and later close (and destroy) the connection. See details.
#' @param handler a custom function that is called on each page of JSON data. If not specified,
#' the default handler stores all pages and binds them into a single data frame that will be
#' returned by `stream_in`. See details.
#' @param x object to be streamed out. Currently only data frames are supported.
#' @param pagesize number of lines to read/write from/to the connection per iteration.
#' @param verbose print some information on what is going on.
#' @param ... arguments for [fromJSON()] and [toJSON()] that
#' control JSON formatting/parsing where applicable. Use with caution.
#' @name stream_in, stream_out
#' @export stream_in stream_out
#' @rdname stream_in
#' @references MongoDB export format: <https://docs.mongodb.com/manual/reference/program/mongoexport/>
#' @references Documentation for the JSON Lines text file format: <https://jsonlines.org/>
#' @seealso [fromJSON()], [read_json()]
#' @return The `stream_out` function always returns `NULL`.
#' When no custom handler is specified, `stream_in` returns a data frame of all pages binded together.
#' When a custom handler function is specified, `stream_in` always returns `NULL`.
#' @examples # compare formats
#' x <- iris[1:3,]
#' toJSON(x)
#' stream_out(x)
#'
#' # Trivial example
#' mydata <- stream_in(url("https://jeroen.github.io/data/iris.json"))
#'
#' \dontrun{
#' #stream large dataset to file and back
#' library(nycflights13)
#' stream_out(flights, file(tmp <- tempfile()))
#' flights2 <- stream_in(file(tmp))
#' unlink(tmp)
#' all.equal(flights2, as.data.frame(flights))
#'
#' # stream over HTTP
#' diamonds2 <- stream_in(url("https://jeroen.github.io/data/diamonds.json"))
#'
#' # stream over HTTP with gzip compression
#' flights3 <- stream_in(gzcon(url("https://jeroen.github.io/data/nycflights13.json.gz")))
#' all.equal(flights3, as.data.frame(flights))
#'
#' # stream over HTTPS (HTTP+SSL) via curl
#' library(curl)
#' flights4 <- stream_in(gzcon(curl("https://jeroen.github.io/data/nycflights13.json.gz")))
#' all.equal(flights4, as.data.frame(flights))
#'
#' # or alternatively:
#' flights5 <- stream_in(gzcon(pipe("curl https://jeroen.github.io/data/nycflights13.json.gz")))
#' all.equal(flights5, as.data.frame(flights))
#'
#' # Full JSON IO stream from URL to file connection.
#' # Calculate delays for flights over 1000 miles in batches of 5k
#' library(dplyr)
#' con_in <- gzcon(url("https://jeroen.github.io/data/nycflights13.json.gz"))
#' con_out <- file(tmp <- tempfile(), open = "wb")
#' stream_in(con_in, handler = function(df){
#'   df <- dplyr::filter(df, distance > 1000)
#'   df <- dplyr::mutate(df, delta = dep_delay - arr_delay)
#'   stream_out(df, con_out, pagesize = 1000)
#' }, pagesize = 5000)
#' close(con_out)
#'
#' # stream it back in
#' mydata <- stream_in(file(tmp))
#' nrow(mydata)
#' unlink(tmp)
#'
#' # Data from http://openweathermap.org/current#bulk
#' # Each row contains a nested data frame.
#' daily14 <- stream_in(gzcon(url("http://78.46.48.103/sample/daily_14.json.gz")), pagesize=50)
#' subset(daily14, city$name == "Berlin")$data[[1]]
#'
#' # Or with dplyr:
#' library(dplyr)
#' daily14f <- flatten(daily14)
#' filter(daily14f, city.name == "Berlin")$data[[1]]
#'
#' # Stream import large data from zip file
#' tmp <- tempfile()
#' download.file("http://jsonstudio.com/wp-content/uploads/2014/02/companies.zip", tmp)
#' companies <- stream_in(unz(tmp, "companies.json"))
#' }
stream_in <- function(con, handler = NULL, pagesize = 500, verbose = TRUE, ...) {

  # Maybe also handle URLs here in future.
  if(!is(con, "connection")){
    stop("Argument 'con' must be a connection.")
  }

  # Same as mongolite
  count <- 0
  cb <- if(is.null(handler)){
    out <- new.env()
    function(x){
      if(length(x)){
        count <<- count + length(x)
        out[[as.character(count)]] <<- x
      }
    }
  } else {
    if(verbose)
      message("using a custom handler function.")
    function(x){
      handler(post_process(x, ...))
      count <<- count + length(x)
    }
  }

  if(!isOpen(con, "r")){
    if(verbose)
      message("opening ", is(con) ," input connection.")

    # binary connection prevents recoding of utf8 to latin1 on windows
    open(con, "rb")
    on.exit({
      if(verbose)
        message("closing ", is(con) ," input connection.")
      close(con)
    })
  }

  # Read data page by page
  repeat {
    page <- readLines(con, n = pagesize, encoding = "UTF-8")
    if(length(page)){
      cleanpage <- Filter(nchar, page)
      cb(lapply(cleanpage, parseJSON))
      if(verbose)
        cat("\r Found", count, "records...")
    }
    if(length(page) < pagesize)
      break
  }

  # Either return a big data frame, or nothing.
  if(is.null(handler)){
    if(verbose) cat("\r Imported", count, "records. Simplifying...\n")
    out <- as.list(out, sorted = FALSE)
    post_process(unlist(out[order(as.numeric(names(out)))], FALSE, FALSE), ...)
  } else {
    invisible()
  }
}

post_process <- function(x, simplifyVector = TRUE, simplifyDataFrame = simplifyVector,
                         simplifyMatrix = simplifyVector, flatten = FALSE){
  out <- simplify(x, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame,
    simplifyMatrix = simplifyMatrix, flatten = flatten)

  # We assume ndjson with objects
  if(isTRUE(simplifyDataFrame)){
    return(as.data.frame(out))
  } else {
    out
  }
}

#' @rdname stream_in
#' @param prefix string to write before each line (use `"\u001e"` to write rfc7464 text sequences)
stream_out <- function(x, con = stdout(), pagesize = 500, verbose = TRUE, prefix = "", ...) {

  if(!is(con, "connection")){
    # Maybe handle URLs here in future.
    stop("Argument 'con' must be a connection.")
  }

  if(!isOpen(con, "w")){
    if(verbose) message("opening ", is(con) ," output connection.")
    open(con, "wb")
    on.exit({
      if(verbose) message("closing ", is(con) ," output connection.")
      close(con)
    })
  }

  invisible(apply_by_pages(x, stream_out_page, pagesize = pagesize, con = con, verbose = verbose, prefix = prefix, ...));
}

stream_out_page <- function(page, con, prefix, ...){
  # useBytes can sometimes prevent recoding of utf8 to latin1 on windows.
  # on windows there is a bug when useBytes is used with a (non binary) text connection.
  str <- enc2utf8(asJSON(page, collapse = FALSE, ...))
  if(is.character(prefix) && length(prefix) && nchar(prefix))
    str <- paste0(prefix[1], str)
  writeLines(str, con = con, useBytes = TRUE)
}
