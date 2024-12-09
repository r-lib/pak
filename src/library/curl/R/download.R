#' Download file to disk
#'
#' Libcurl implementation of \code{C_download} (the "internal" download method)
#' with added support for https, ftps, gzip, etc. Default behavior is identical
#' to \code{\link{download.file}}, but request can be fully configured by passing
#' a custom \code{\link{handle}}.
#'
#' The main difference between \code{curl_download} and \code{curl_fetch_disk}
#' is that \code{curl_download} checks the http status code before starting the
#' download, and raises an error when status is non-successful. The behavior of
#' \code{curl_fetch_disk} on the other hand is to proceed as normal and write
#' the error page to disk in case of a non success response.
#'
#' For a more advanced download interface which supports concurrent requests and
#' resuming large files, have a look at the [multi_download] function.
#'
#' @useDynLib curl R_download_curl
#' @seealso Advanced download interface: [multi_download]
#' @param url A character string naming the URL of a resource to be downloaded.
#' @param destfile A character string with the name where the downloaded file
#'   is saved. Tilde-expansion is performed.
#' @param quiet If \code{TRUE}, suppress status messages (if any), and the
#'   progress bar.
#' @param mode A character string specifying the mode with which to write the file.
#'   Useful values are \code{"w"}, \code{"wb"} (binary), \code{"a"} (append)
#'   and \code{"ab"}.
#' @param handle a curl handle object
#' @return Path of downloaded file (invisibly).
#' @export
#' @examples
#' # Download large file
#' \dontrun{
#' url <- "http://www2.census.gov/acs2011_5yr/pums/csv_pus.zip"
#' tmp <- tempfile()
#' curl_download(url, tmp)
#' }
curl_download <- function(url, destfile, quiet = TRUE, mode = "wb", handle = new_handle()){
  destfile <- enc2native(normalizePath(destfile, mustWork = FALSE))
  nonblocking <- isTRUE(getOption("curl_interrupt", TRUE))
  tmp <- enc2native(paste0(destfile, ".curltmp"))
  on.exit(unlink(tmp))
  .Call(R_download_curl, url, tmp, quiet, mode, handle, nonblocking)
  file.rename(tmp, destfile)
  invisible(destfile)
}
