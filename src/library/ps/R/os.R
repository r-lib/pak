
#' Query the type of the OS
#'
#' @return `ps_os_type` returns a named logical vector. The rest of the
#' functions return a logical scalar.
#'
#' `ps_is_supported()` returns `TRUE` if ps supports the current platform.
#'
#' @export
#' @examples
#' ps_os_type()
#' ps_is_supported()

ps_os_type <- function() {
  if (is.null(ps_env$os_type)) ps_env$os_type <- .Call(ps__os_type)
  ps_env$os_type
}

ps_os_name <- function() {
  os <- ps_os_type()
  os <- os[setdiff(names(os), c("BSD", "POSIX"))]
  names(os)[which(os)]
}

#' @rdname ps_os_type
#' @export

ps_is_supported <- function() {
  os <- ps_os_type()
  if (os[["LINUX"]]) {
    # On Linux we need to check if /proc is readable
    supported <- FALSE
    tryCatch({
      readLines("/proc/stat", warn = FALSE, n = 1)
      supported <- TRUE
    }, error = function(e) e)
    supported

  } else {
    os <- os[setdiff(names(os), c("BSD", "POSIX"))]
    any(os)
  }
}

supported_str <- function() {
  os <- ps_os_type()
  os <- os[setdiff(names(os), c("BSD", "POSIX"))]
  paste(caps(names(os)), collapse = ", ")
}
