#' URL encoding
#'
#' Escape all special characters (i.e. everything except for a-z, A-Z, 0-9, '-',
#' '.', '_' or '~') for use in URLs.
#'
#' @useDynLib curl R_curl_escape
#' @export
#' @param url A character vector (typically containing urls or parameters) to be
#'   encoded/decoded
#' @examples # Escape strings
#' out <- curl_escape("foo = bar + 5")
#' curl_unescape(out)
#'
#' # All non-ascii characters are encoded
#' mu <- "\u00b5"
#' curl_escape(mu)
#' curl_unescape(curl_escape(mu))
curl_escape <- function(url){
  .Call(R_curl_escape, enc2utf8(as.character(url)), FALSE);
}

#' @rdname curl_escape
#' @export
curl_unescape <- function(url){
  .Call(R_curl_escape, enc2utf8(as.character(url)), TRUE);
}
