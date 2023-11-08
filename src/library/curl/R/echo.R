#' Echo Service
#'
#' This function is only for testing purposes. It starts a local httpuv server to
#' echo the request body and content type in the response.
#'
#' @export
#' @param handle a curl handle object
#' @param port the port number on which to run httpuv server
#' @param progress show progress meter during http transfer
#' @param file path or connection to write body. Default returns body as raw vector.
#' @examples if(require('httpuv')){
#' h <- new_handle(url = 'https://hb.cran.dev/post')
#' handle_setform(h, foo = "blabla", bar = charToRaw("test"),
#'   myfile = form_file(system.file("DESCRIPTION"), "text/description"))
#'
#' # Echo the POST request data
#' formdata <- curl_echo(h)
#'
#' # Show the multipart body
#' cat(rawToChar(formdata$body))
#'
#' # Parse multipart
#' webutils::parse_http(formdata$body, formdata$content_type)
#' }
curl_echo <- function(handle, port = 9359, progress = interactive(), file = NULL){
  progress <- isTRUE(progress)
  if(!(is.null(file) || inherits(file, "connection") || is.character(file)))
    stop("Argument 'file' must be a file path or connection object")
  output <- NULL
  echo_handler <- function(req){
    if(progress){
      cat("\nRequest Complete!\n", file = stderr())
      progress <<- FALSE
    }
    body <- if(tolower(req[["REQUEST_METHOD"]]) %in% c("post", "put")){
      if(!length(file)){
        req[["rook.input"]]$read()
      } else {
        write_to_file(req[["rook.input"]]$read, file)
      }
    }

    output <<- list(
      method = req[["REQUEST_METHOD"]],
      path = req[["PATH_INFO"]],
      query = req[["QUERY_STRING"]],
      content_type = req[["CONTENT_TYPE"]],
      body = body,
      headers = req[["HEADERS"]]
    )

    # Dummy response
    list(
      status = 200,
      body = "",
      headers = c("Content-Type" = "text/plain")
    )
  }

  # Start httpuv
  server_id <- httpuv::startServer("0.0.0.0", port, list(call = echo_handler))
  on.exit(httpuv::stopServer(server_id), add = TRUE)

  # Workaround bug in httpuv on windows that keeps protecting handler until next startServer()
  on.exit(rm(handle), add = TRUE)

  # Post data from curl
  xfer <- function(down, up){
    if(progress){
      if(up[1] == 0 && down[1] == 0){
        cat("\rConnecting...", file = stderr())
      } else {
        cat(sprintf("\rUpload: %s (%.0f%%)   ", format_size(up[2]),
                    as.integer(100 * up[2] / up[1])), file = stderr())
      }
    }
    # Need very low wait to prevent gridlocking!
    httpuv::service(NA)
    TRUE
  }
  handle_setopt(handle, connecttimeout = 2, xferinfofunction = xfer, noprogress = FALSE, forbid_reuse = TRUE)
  if(progress) cat("\n", file = stderr())
  input_url <- handle_data(handle)$url
  if(length(input_url) && nchar(input_url)){
    target_url <- replace_host(input_url, paste0("http://127.0.0.1:", port))
    host <- sub("https?://([^/]+).*", "\\1", input_url)
    #hostname <- gsub(":[0-9]+$", "", host)
    #handle_setopt(handle, port = port, resolve = paste0(hostname, ":", port, ':127.0.0.1'))
    handle_setopt(handle, httpheader = c(paste0("Host:", host), handle_getheaders(handle)))
  } else {
    target_url <- paste0("http://127.0.0.1:", port)
  }
  curl_fetch_memory(target_url, handle = handle)
  output$url <- input_url
  if(progress) cat("\n", file = stderr())
  return(output)
}

replace_host <- function(url, new_host = 'http://127.0.0.1'){
  sub("[a-zA-Z]+://[^/]+", new_host, url)
}

write_to_file <- function(readfun, filename){
  con <- if(inherits(filename, "connection")){
    filename
  } else {
    base::file(filename)
  }
  if(!isOpen(con)){
    open(con, "wb")
    on.exit(close(con))
  }
  while(length(buf <- readfun(1e6))){
    writeBin(buf, con)
  }
  return(filename)
}

format_size <- function(x){
  if(x < 1024)
    return(sprintf("%d b", x))
  if(x < 1048576)
    return(sprintf("%.2f Kb", x / 1024))
  return(sprintf("%.2f Mb", x / 1048576))
}
