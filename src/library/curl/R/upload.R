#' Upload a File
#'
#' Upload a file to an \code{http://}, \code{ftp://}, or \code{sftp://} (ssh)
#' server. Uploading to HTTP means performing an \code{HTTP PUT} on that URL.
#' Be aware that sftp is only available for libcurl clients built with libssh2.
#'
#' @export
#' @param file connection object or path to an existing file on disk
#' @param url where to upload, should start with e.g. \code{ftp://}
#' @param verbose emit some progress output
#' @param reuse try to keep alive and recycle connections when possible
#' @param ... other arguments passed to \code{\link{handle_setopt}}, for
#' example a \code{username} and \code{password}.
#' @examples \dontrun{# Upload package to winbuilder:
#' curl_upload('mypkg_1.3.tar.gz', 'ftp://win-builder.r-project.org/R-devel/')
#' }
curl_upload <- function(file, url, verbose = TRUE, reuse = TRUE, ...) {
  infilesize <- NA
  con <- if(is.character(file)){
    file <- normalizePath(file, mustWork = TRUE)
    infilesize <- file.info(file)$size
    base::file(file, open = 'rb')
  } else if(inherits(file, 'connection')){
    file
  } else {
    stop("Parameter 'file' must be a ")
  }
  on.exit(close(con))
  total_bytes <- 0
  h <- new_handle(upload = TRUE, filetime = FALSE, readfunction = function(n) {
    buf <- readBin(con, raw(), n = n)
    total_bytes <<- total_bytes + length(buf)
    if(verbose){
      if(length(buf) == 0 || identical(total_bytes, infilesize)){
        cat(sprintf("\rUploaded %.0f bytes... all done!\n", total_bytes), file = stderr())
      } else {
        cat(sprintf("\rUploaded %.0f bytes...", total_bytes), file = stderr())
      }
    }
    return(buf)
  }, seekfunction = function(offset){
    seek(con, where = offset)
  }, forbid_reuse = !isTRUE(reuse), verbose = verbose, ...)
  if(!is.na(infilesize)){
    handle_setopt(h, infilesize_large = infilesize)
  }
  if(grepl('/$', url) && is.character(file)){
    url <- paste0(url, basename(file))
  }
  curl_fetch_memory(url, handle = h)
}
