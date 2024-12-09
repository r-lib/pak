#' Lazy File Writer
#'
#' Generates a closure that writes binary (raw) data to a file.
#'
#' The writer function automatically opens the file on the first write and closes when
#' it goes out of scope, or explicitly by setting \code{close = TRUE}. This can be used
#' for the \code{data} callback in \code{multi_add()} or \code{curl_fetch_multi()} such
#' that we only keep open file handles for active downloads. This prevents running out
#' of file descriptors when performing thousands of concurrent requests.
#'
#' @export
#' @param path file name or path on disk
#' @param append open file in append mode
#' @return Function with signature \code{writer(data = raw(), close = FALSE)}
#' @examples
#' # Doesn't open yet
#' tmp <- tempfile()
#' writer <- file_writer(tmp)
#'
#' # Now it opens
#' writer(charToRaw("Hello!\n"))
#' writer(charToRaw("How are you?\n"))
#'
#' # Close it!
#' writer(charToRaw("All done!\n"), close = TRUE)
#'
#' # Check it worked
#' readLines(tmp)
file_writer <- function(path, append = FALSE){
  path <- enc2native(normalizePath(path, mustWork = FALSE))
  fp <- new_file_writer(path, append)
  structure(function(data = raw(), close = FALSE){
    stopifnot(is.raw(data))
    write_file_writer(fp, data, as.logical(close))
  }, class = "file_writer")
}

#' @useDynLib curl R_new_file_writer
new_file_writer <- function(path, append){
  .Call(R_new_file_writer, list(path, append))
}

#' @useDynLib curl R_write_file_writer
write_file_writer <- function(fp, data, close){
  .Call(R_write_file_writer, fp, data, close)
}

#' @useDynLib curl R_total_writers
total_writers <- function(){
  .Call(R_total_writers)
}
