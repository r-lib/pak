.onAttach <- function(libname, pkgname){
  ssl <- sub("\\(.*\\)\\W*", "", curl_version()$ssl_version)
  msg <- paste("Using libcurl", curl_version()$version, "with", ssl)
  packageStartupMessage(msg)
}

.onLoad <- function(libname, pkgname){
  assign("option_type_table", make_option_type_table(), environment(.onLoad))
}
