get_default_curl_options <- function(options) {
  getopt <- function(nm) {
    if (!is.null(v <- options[[nm]])) return(v)
    anm <- paste0("async_http_", nm)
    if (!is.null(v <- getOption(anm))) return(v)
    if (!is.na(v <- Sys.getenv(toupper(anm), NA_character_))) return (v)
  }
  utils::modifyList(
    options,
    drop_nulls(list(
      timeout = as.integer(getopt("timeout") %||% 0),
      connecttimeout = as.integer(getopt("connecttimeout") %||% 300),
      low_speed_time = as.integer(getopt("low_speed_time") %||% 0),
      low_speed_limit = as.integer(getopt("low_speed_limit") %||% 0),
      cainfo = getopt("cainfo")
    ))
  )
}

http_get <- function(url, options = list()) {
  handle <- curl::new_handle(url = url)
  options <- get_default_curl_options(options)
  curl::handle_setopt(handle, .list = options)
  curl::curl_fetch_memory(url, handle = handle)
}

http_post <- function(url, body, headers = character(), options = list()) {
  if (!is.raw(body)) body <- charToRaw(body)
  handle <- curl::new_handle(url = url)
  curl::handle_setheaders(handle, .list = headers)
  options <- get_default_curl_options(options)
  curl::handle_setopt(
    handle,
    customrequest = "POST",
    postfieldsize = length(body),
    postfields = body,
    .list = options
 )
  curl::curl_fetch_memory(url, handle = handle)
}

http_stop_for_status <- function(resp) {
  if (!is.integer(resp$status_code)) stop("Not an HTTP response")
  if (resp$status_code < 400) return(invisible(resp))
  stop(http_error(resp))
}

http_error <- function(resp, call = sys.call(-1)) {
  status <- resp$status_code
  reason <- http_status(status)$reason
  message <- sprintf("%s (HTTP %d).", reason, status)
  status_type <- (status %/% 100) * 100
  if (length(resp[["content"]]) == 0 && !is.null(resp$file) &&
              file.exists(resp$file)) {
    tryCatch({
      n <- file.info(resp$file, extra_cols = FALSE)$size
      resp$content <- readBin(resp$file, what = raw(), n = n)
    }, error = identity)
  }
  http_class <- paste0("async_http_", unique(c(status, status_type, "error")))
  structure(
    list(message = message, call = call, response = resp),
    class = c(http_class, "error", "condition")
  )
}

http_status <- function(status) {
  status_desc <- http_statuses[as.character(status)]
  if (is.na(status_desc)) {
    stop("Unknown http status code: ", status, call. = FALSE)
  }

  status_types <- c("Information", "Success", "Redirection", "Client error",
    "Server error")
  status_type <- status_types[[status %/% 100]]

  # create the final information message
  message <- paste(status_type, ": (", status, ") ", status_desc, sep = "")

  list(
    category = status_type,
    reason = status_desc,
    message = message
  )
}

http_statuses <- c(
  "100" = "Continue",
  "101" = "Switching Protocols",
  "102" = "Processing (WebDAV; RFC 2518)",
  "200" = "OK",
  "201" = "Created",
  "202" = "Accepted",
  "203" = "Non-Authoritative Information",
  "204" = "No Content",
  "205" = "Reset Content",
  "206" = "Partial Content",
  "207" = "Multi-Status (WebDAV; RFC 4918)",
  "208" = "Already Reported (WebDAV; RFC 5842)",
  "226" = "IM Used (RFC 3229)",
  "300" = "Multiple Choices",
  "301" = "Moved Permanently",
  "302" = "Found",
  "303" = "See Other",
  "304" = "Not Modified",
  "305" = "Use Proxy",
  "306" = "Switch Proxy",
  "307" = "Temporary Redirect",
  "308" = "Permanent Redirect (experimental Internet-Draft)",
  "400" = "Bad Request",
  "401" = "Unauthorized",
  "402" = "Payment Required",
  "403" = "Forbidden",
  "404" = "Not Found",
  "405" = "Method Not Allowed",
  "406" = "Not Acceptable",
  "407" = "Proxy Authentication Required",
  "408" = "Request Timeout",
  "409" = "Conflict",
  "410" = "Gone",
  "411" = "Length Required",
  "412" = "Precondition Failed",
  "413" = "Request Entity Too Large",
  "414" = "Request-URI Too Long",
  "415" = "Unsupported Media Type",
  "416" = "Requested Range Not Satisfiable",
  "417" = "Expectation Failed",
  "418" = "I'm a teapot (RFC 2324)",
  "420" = "Enhance Your Calm (Twitter)",
  "422" = "Unprocessable Entity (WebDAV; RFC 4918)",
  "423" = "Locked (WebDAV; RFC 4918)",
  "424" = "Failed Dependency (WebDAV; RFC 4918)",
  "424" = "Method Failure (WebDAV)",
  "425" = "Unordered Collection (Internet draft)",
  "426" = "Upgrade Required (RFC 2817)",
  "428" = "Precondition Required (RFC 6585)",
  "429" = "Too Many Requests (RFC 6585)",
  "431" = "Request Header Fields Too Large (RFC 6585)",
  "444" = "No Response (Nginx)",
  "449" = "Retry With (Microsoft)",
  "450" = "Blocked by Windows Parental Controls (Microsoft)",
  "451" = "Unavailable For Legal Reasons (Internet draft)",
  "499" = "Client Closed Request (Nginx)",
  "500" = "Internal Server Error",
  "501" = "Not Implemented",
  "502" = "Bad Gateway",
  "503" = "Service Unavailable",
  "504" = "Gateway Timeout",
  "505" = "HTTP Version Not Supported",
  "506" = "Variant Also Negotiates (RFC 2295)",
  "507" = "Insufficient Storage (WebDAV; RFC 4918)",
  "508" = "Loop Detected (WebDAV; RFC 5842)",
  "509" = "Bandwidth Limit Exceeded (Apache bw/limited extension)",
  "510" = "Not Extended (RFC 2774)",
  "511" = "Network Authentication Required (RFC 6585)",
  "598" = "Network read timeout error (Unknown)",
  "599" = "Network connect timeout error (Unknown)"
)
