#' Lookup a hostname
#'
#' The \code{nslookup} function is similar to \code{nsl} but works on all platforms
#' and can resolve ipv6 addresses if supported by the OS. Default behavior raises an
#' error if lookup fails.
#'
#' The \code{has_internet} function tests for internet connectivity by performing a
#' dns lookup. If a proxy server is detected, it will also check for connectivity by
#' connecting via the proxy.
#'
#' @export
#' @param host a string with a hostname
#' @param error raise an error for failed DNS lookup. Otherwise returns \code{NULL}.
#' @param ipv4_only always return ipv4 address. Set to `FALSE` to allow for ipv6 as well.
#' @param multiple returns multiple ip addresses if possible
#' @rdname nslookup
#' @useDynLib curl R_nslookup
#' @examples # Should always work if we are online
#' nslookup("www.r-project.org")
#'
#' # If your OS supports IPv6
#' nslookup("ipv6.test-ipv6.com", ipv4_only = FALSE, error = FALSE)
nslookup <- function(host, ipv4_only = FALSE, multiple = FALSE, error = TRUE){
  stopifnot(is.character(host))
  host <- enc2utf8(host)
  if(grepl("://", host, fixed = TRUE))
    stop("This looks like a URL, not a hostname")
  out <- .Call(R_nslookup, host[1], as.logical(ipv4_only))
  if(isTRUE(error) && is.null(out))
    stop("Unable to resolve host: ", host)
  if(isTRUE(multiple))
    return(unique(out))
  utils::head(out, 1)
}

#' @export
#' @rdname nslookup
has_internet <- local({
  proxy_vars_previous <- NULL
  has_internet_via_proxy <- NULL
  function(){
    # do not wait for nslookup() if we know proxy works
    if(isTRUE(has_internet_via_proxy))
      return(TRUE)

    # Method 1: try DNS lookup. May resolve to 8.8.8.8 or 8.8.4.4
    ip_addr <- nslookup('dns.google.com', multiple = TRUE, error = FALSE, ipv4_only = TRUE)
    if(any(c("8.8.4.4", "8.8.8.8") %in% ip_addr))
      return(TRUE)

    # Method 2: look for a proxy server
    proxy_vars <- Sys.getenv(c('ALL_PROXY', 'https_proxy', 'HTTPS_PROXY', 'HTTPS_proxy'), NA)

    # If variables are unchanged, use the cached result
    if(length(has_internet_via_proxy) && identical(proxy_vars, proxy_vars_previous)) {
      return(has_internet_via_proxy)
    } else {
      proxy_vars_previous <<- proxy_vars
    }

    # Try via a proxy (mostly for Windows)
    test_url <- 'http://captive.apple.com/hotspot-detect.html'
    ie_proxy <- ie_get_proxy_for_url(test_url)

    handle <- if(any(!is.na(proxy_vars))){
      cat("Testing for internet connectivity via https_proxy... ", file = stderr())
      new_handle(CONNECTTIMEOUT = 5)
    } else if(length(ie_proxy)){
      cat("Testing for internet connectivity via IE proxy... ", file = stderr())
      new_handle(CONNECTTIMEOUT = 5, proxy = ie_proxy)
    } else {
      # No proxy server found. Do not cache in case user sets one later.
      return(FALSE)
    }
    req <- try(curl_fetch_memory(url = test_url, handle = handle), silent = TRUE)
    has_internet_via_proxy <<- is.list(req) && identical(req$status_code, 200L) &&
      grepl("Success", rawToChar(req$content))
    cat(ifelse(has_internet_via_proxy, "success!\n", "failed.\n"), file = stderr())
    return(has_internet_via_proxy)
  }
})
