#' Curl connection interface
#'
#' Drop-in replacement for base \code{\link{url}} that supports https, ftps,
#' gzip, deflate, etc. Default behavior is identical to \code{\link{url}}, but
#' request can be fully configured by passing a custom \code{\link{handle}}.
#'
#' As of version 2.3 curl connections support \code{open(con, blocking = FALSE)}.
#' In this case \code{readBin} and \code{readLines} will return immediately with data
#' that is available without waiting. For such non-blocking connections the caller
#' needs to call \code{\link{isIncomplete}} to check if the download has completed
#' yet.
#'
#' @useDynLib curl R_curl_connection
#' @export
#' @param url character string. See examples.
#' @param open character string. How to open the connection if it should be opened
#'   initially. Currently only "r" and "rb" are supported.
#' @param handle a curl handle object
#' @examples \dontrun{
#' con <- curl("https://hb.cran.dev/get")
#' readLines(con)
#'
#' # Auto-opened connections can be recycled
#' open(con, "rb")
#' bin <- readBin(con, raw(), 999)
#' close(con)
#' rawToChar(bin)
#'
#' # HTTP error
#' curl("https://hb.cran.dev/status/418", "r")
#'
#' # Follow redirects
#' readLines(curl("https://hb.cran.dev/redirect/3"))
#'
#' # Error after redirect
#' curl("https://hb.cran.dev/redirect-to?url=https://hb.cran.dev/status/418", "r")
#'
#' # Auto decompress Accept-Encoding: gzip / deflate (rfc2616 #14.3)
#' readLines(curl("https://hb.cran.dev/gzip"))
#' readLines(curl("https://hb.cran.dev/deflate"))
#'
#' # Binary support
#' buf <- readBin(curl("https://hb.cran.dev/bytes/98765", "rb"), raw(), 1e5)
#' length(buf)
#'
#' # Read file from disk
#' test <- paste0("file://", system.file("DESCRIPTION"))
#' readLines(curl(test))
#'
#' # Other protocols
#' read.csv(curl("ftp://cran.r-project.org/pub/R/CRAN_mirrors.csv"))
#' readLines(curl("ftps://test.rebex.net:990/readme.txt"))
#' readLines(curl("gopher://quux.org/1"))
#'
#' # Streaming data
#' con <- curl("http://jeroen.github.io/data/diamonds.json", "r")
#' while(length(x <- readLines(con, n = 5))){
#'   print(x)
#' }
#'
#' # Stream large dataset over https with gzip
#' library(jsonlite)
#' con <- gzcon(curl("https://jeroen.github.io/data/nycflights13.json.gz"))
#' nycflights <- stream_in(con)
#' }
#'
curl <- function(url = "https://hb.cran.dev/get", open = "", handle = new_handle()){
  curl_connection(url, open, handle)
}

# 'stream' currently only used for non-blocking connections to prevent
# busy looping in curl_fetch_stream()
curl_connection <- function(url, mode, handle, partial = FALSE){
  con <- .Call(R_curl_connection, url, handle, partial)
  if(!identical(mode, "")){
    withCallingHandlers(open(con, open = mode), error = function(err) {
      close(con)
    })
  }
  return(con)
}
