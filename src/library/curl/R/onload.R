.onAttach <- function(libname, pkgname){
  version <- curl_version()
  ssl <- sub("\\(.*\\)\\W*", "", version$ssl_version)
  msg <- paste("Using libcurl", version$version, "with", ssl)
  packageStartupMessage(msg)
  if(grepl("redhat", R.version$platform) && !('smtp' %in% version$protocols)){
    packageStartupMessage(c("Your system runs libcurl-minimal which does not support all protocols: ",
                          "See also https://github.com/jeroen/curl/issues/350"))
  }
  try({
    proxy <- Sys.getenv('ALL_PROXY')
    if(nchar(proxy)){
      proxy_info <- curl::curl_parse_url(proxy)
      packageStartupMessage(sprintf("Using proxy server %s://%s:%s",
        proxy_info$scheme, proxy_info$host, proxy_info$port))
    }
  }, silent = TRUE)
}

.onLoad <- function(libname, pkgname){
  if(grepl('emscripten', R.version[['platform']])){
    set_emscripten_gateway()
  }
}

# Sets a default http gateway for using curl in WebR
# See https://github.com/r-wasm/ws-proxy
# Note socks5 proxy is just a dumb gateway that relays the encrypted https
# traffic as-is. There is no snooping or tempering with cert verification
# possible by the proxy server.
set_emscripten_gateway <- function(){
  proxy <- Sys.getenv('ALL_PROXY')

  # TODO: fix this unfortunate default envvar in webR?
  if(proxy == '' || proxy == "socks5h://localhost:8580"){
    try({
      # Note the websocket runs wss (port 443) but inside we mimic plain http
      # therefore for curl it looks like http:// but on 443. But it is actually
      # https because of the wss:// layer in emscripten.
      h <- new_handle(connecttimeout = 2, noproxy = '*')
      req <- curl_fetch_memory("http://get-ws-proxy.r-universe.dev:443", handle = h)
      if(req$status == 200){
        wsproxy <- rawToChar(req$content)
        if(grepl('^socks5h://', wsproxy)){
          Sys.setenv(ALL_PROXY = wsproxy)
        }
      }
    }, silent = TRUE)
  }
}
