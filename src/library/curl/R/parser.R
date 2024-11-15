#' Normalizing URL parser
#'
#' Interfaces the libcurl [URL parser](https://curl.se/libcurl/c/libcurl-url.html).
#' URLs are automatically normalized where possible, such as in the case of
#' relative paths or url-encoded queries (see examples).
#' When parsing hyperlinks from a HTML document, it is possible to set `baseurl`
#' to the location of the document itself such that relative links can be resolved.
#'
#' A valid URL contains at least a scheme and a host, other pieces are optional.
#' If these are missing, the parser raises an error. Otherwise it returns
#' a list with the following elements:
#'  - *url*: the normalized input URL
#'  - *scheme*: the protocol part before the `://` (required)
#'  - *host*: name of host without port (required)
#'  - *port*: decimal between 0 and 65535
#'  - *path*: normalized path up till the `?` of the url
#'  - *query*: search query: part between the `?` and `#` of the url. Use `params` below to get individual parameters from the query.
#'  - *fragment*: the hash part after the `#` of the url
#'  - *user*: authentication username
#'  - *password*: authentication password
#'  - *params*: named vector with parameters from `query` if set
#'
#' Each element above is either a string or `NULL`, except for `params` which
#' is always a character vector with the length equal to the number of parameters.
#'
#' Note that the `params` field is only usable if the `query` is in the usual
#' `application/x-www-form-urlencoded` format which is technically not part of
#' the RFC. Some services may use e.g. a json blob as the query, in which case
#' the parsed `params` field here can be ignored. There is no way for the parser
#' to automatically infer or validate the query format, this is up to the caller.
#'
#' For more details on the URL format see
#' [rfc3986](https://datatracker.ietf.org/doc/html/rfc3986)
#' or the steps explained in the [whatwg basic url parser](https://url.spec.whatwg.org/#concept-basic-url-parser).
#'
#' On platforms that do not have a recent enough curl version (basically only
#' RHEL-8) the [Ada URL](https://github.com/ada-url/ada) library is used as fallback.
#' Results should be identical, though curl has nicer error messages. This is
#' a temporary solution, we plan to remove the fallback when old systems are
#' no longer supported.
#'
#' @export
#' @param url a character string of length one
#' @param baseurl use this as the parent if `url` may be a relative path
#' @param decode automatically [url-decode][curl_escape] output.
#' Set to `FALSE` to get output in url-encoded format.
#' @param params parse individual parameters assuming query is in `application/x-www-form-urlencoded` format.
#' @useDynLib curl R_parse_url
#' @examples
#' url <- "https://jerry:secret@google.com:888/foo/bar?test=123#bla"
#' curl_parse_url(url)
#'
#' # Resolve relative links from a baseurl
#' curl_parse_url("/somelink", baseurl = url)
#'
#' # Paths get normalized
#' curl_parse_url("https://foobar.com/foo/bar/../baz/../yolo")$url
#'
#' # Also normalizes URL-encoding (these URLs are equivalent):
#' url1 <- "https://ja.wikipedia.org/wiki/\u5bff\u53f8"
#' url2 <- "https://ja.wikipedia.org/wiki/%e5%af%bf%e5%8f%b8"
#' curl_parse_url(url1)$path
#' curl_parse_url(url2)$path
#' curl_parse_url(url1, decode = FALSE)$path
#' curl_parse_url(url1, decode = FALSE)$path
curl_parse_url <- function(url, baseurl = NULL, decode = TRUE, params = TRUE){
  stopifnot(is.character(url))
  stopifnot(length(url) == 1)
  baseurl < as.character(baseurl)
  result <- .Call(R_parse_url, url, baseurl)
  if(inherits(result, 'ada')){
    result <- normalize_ada(result)
  }
  # Need to parse query before url-decoding
  if(params){
    result$params <- tryCatch(parse_query_urlencoded(result$query), error = message)
  }

  if(isTRUE(decode)){
    if(length(result$url))
      result$url <- curl_unescape(result$url)
    if(length(result$path))
      result$path <- curl_unescape(result$path)
    if(length(result$query))
      result$query <- curl_unescape(result$query)
    if(length(result$fragment))
      result$fragment <- curl_unescape(result$fragment)
    if(length(result$user))
      result$user <- curl_unescape(result$user)
    if(length(result$password))
      result$password <- curl_unescape(result$password)
  }
  result
}


# NB: Ada also automatically removes the 'port' if it is the default
# for that scheme such as https://host:443. I don't think we can prevent that.
normalize_ada <- function(result){
  if(length(result$scheme))
    result$scheme <- sub("\\:$", "", result$scheme)
  if(length(result$query))
    result$query <- sub("^\\?", "", result$query)
  if(length(result$fragment))
    result$fragment <- sub("^\\#", "", result$fragment)
  unclass(result)
}

# Parses a string in 'application/x-www-form-urlencoded' format
parse_query_urlencoded <- function(query){
  if(!length(query)) return(character())
  query <- chartr('+',' ', query)
  argstr <- strsplit(query, "&", fixed = TRUE)[[1]]
  args <- lapply(argstr, function(x){
    c(curl_unescape(strsplit(x, "=", fixed = TRUE)[[1]]), "")
  })
  values <- vapply(args, `[`, character(1), 2)
  names(values) <- vapply(args, `[`, character(1), 1)
  return(values)
}

try_parse_url <- function(url){
  tryCatch(curl_parse_url(url), error = function(e){})
}
